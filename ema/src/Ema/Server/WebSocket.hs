module Ema.Server.WebSocket where

import Colog (Msg (msgText), logDebug, logError)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Unique (hashUnique, newUnique)
import Ema.Asset (
  Asset (AssetGenerated, AssetStatic),
  Format (Html, Other),
 )
import Ema.CLI (AppM)
import Ema.Route.Class (IsRoute (RouteModel, routePrism))
import Ema.Route.Prism (
  fromPrism_,
 )
import Ema.Server.Common
import Ema.Server.WebSocket.Options (EmaWsHandler (..))
import Ema.Site (EmaStaticSite)
import Network.WebSockets (ConnectionException)
import Network.WebSockets qualified as WS
import Optics.Core (review)
import Text.Printf (printf)
import UnliftIO (liftIOOp)
import UnliftIO.Async (race)
import UnliftIO.Exception (try)

wsApp ::
  forall r.
  (Eq r, Show r, IsRoute r, EmaStaticSite r) =>
  LVar (RouteModel r) ->
  EmaWsHandler r ->
  WS.PendingConnection ->
  AppM ()
wsApp model emaWsHandler pendingConn = do
  conn :: WS.Connection <- liftIO $ WS.acceptRequest pendingConn
  subId <- liftIO $ hashUnique <$> newUnique
  let logId msg =
        msg
          { msgText =
              toText @String (printf "[ema.ws.%.2d] " subId)
                <> msgText msg
          }
  liftIOOp (WS.withPingThread conn 30 pass) $
    local (contramap logId) $ do
      logDebug "Connected"
      let wsHandler = unEmaWsHandler emaWsHandler conn
          sendRouteHtmlToClient path s = do
            decodeUrlRoute @r s path & \case
              Left err -> do
                logError $ badRouteEncodingMsg err
                liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse $ badRouteEncodingMsg err
              Right Nothing ->
                liftIO $ WS.sendTextData conn $ emaErrorHtmlResponse decodeRouteNothingMsg
              Right (Just r) -> do
                renderCatchingErrors s r >>= \case
                  AssetGenerated Html html ->
                    liftIO $ WS.sendTextData conn $ html <> toLazy wsClientHtml
                  -- HACK: We expect the websocket client should check for REDIRECT prefix.
                  -- Not bothering with JSON response to avoid having to JSON parse every HTML dump.
                  AssetStatic _staticPath ->
                    liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ routePrism s) r)
                  AssetGenerated Other _s ->
                    liftIO $ WS.sendTextData conn $ "REDIRECT " <> toText (review (fromPrism_ $ routePrism s) r)
                logDebug $ " ~~> " <> show r
          -- @mWatchingRoute@ is the route currently being watched.
          loop mWatchingRoute = do
            -- Listen *until* either we get a new value, or the client requests
            -- to switch to a new route.
            currentModel <- LVar.get model
            race (LVar.listenNext model) (wsHandler currentModel) >>= \case
              Left newModel -> do
                -- The page the user is currently viewing has changed. Send
                -- the new HTML to them.
                sendRouteHtmlToClient mWatchingRoute newModel
                loop mWatchingRoute
              Right mNextRoute -> do
                -- The user clicked on a route link; send them the HTML for
                -- that route this time, ignoring what we are watching
                -- currently (we expect the user to initiate a watch route
                -- request immediately following this).
                sendRouteHtmlToClient mNextRoute =<< LVar.get model
                loop mNextRoute
      -- Wait for the client to send the first request with the initial route.
      mInitialRoute <- wsHandler =<< LVar.get model
      try (loop mInitialRoute) >>= \case
        Right () -> pass
        Left (connExc :: ConnectionException) -> do
          case connExc of
            WS.CloseRequest _ (decodeUtf8 -> reason) ->
              logDebug $ "Closing websocket connection (reason: " <> reason <> ")"
            _ ->
              logDebug $ "Websocket error: " <> show connExc
