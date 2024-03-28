module Ema.Server (
  EmaWebSocketOptions (..),
  runServerWithWebSocketHotReload,
) where

import Colog (logInfo)
import Data.LVar (LVar)
import Ema.CLI (AppM, Host (unHost))
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Server.HTTP (httpApp)
import Ema.Server.WebSocket (wsApp)
import Ema.Server.WebSocket.Options (EmaWebSocketOptions (..))
import Ema.Site (EmaStaticSite)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS
import UnliftIO (UnliftIO (..), askUnliftIO)
import UnliftIO.Concurrent (threadDelay)

runServerWithWebSocketHotReload ::
  forall r.
  ( Show r
  , Eq r
  , IsRoute r
  , EmaStaticSite r
  ) =>
  Maybe (EmaWebSocketOptions r) ->
  Host ->
  Maybe Port ->
  LVar (RouteModel r) ->
  AppM ()
runServerWithWebSocketHotReload mWsOpts host mport model = do
  (unliftIO -> unlift) <- askUnliftIO
  let settings =
        Warp.defaultSettings
          & Warp.setHost (fromString . toString . unHost $ host)
      app =
        case mWsOpts of
          Nothing ->
            (unlift .) . httpApp @r model Nothing
          Just opts ->
            WaiWs.websocketsOr
              WS.defaultConnectionOptions
              (unlift . wsApp @r model (emaWebSocketServerHandler opts))
              ((unlift .) . httpApp @r model (Just $ emaWebSocketClientShim opts))
      banner port = do
        logInfo "==============================================="
        logInfo $ "Ema live server RUNNING: http://" <> unHost host <> ":" <> show port <> " (" <> maybe "no ws" (const "ws") mWsOpts <> ")"
        logInfo "==============================================="
  liftIO $ warpRunSettings settings mport (unlift . banner) app

-- Like Warp.runSettings but takes *optional* port. When no port is set, a
-- free (random) port is used.
warpRunSettings :: Warp.Settings -> Maybe Port -> (Port -> IO a) -> Wai.Application -> IO ()
warpRunSettings settings mPort banner app = do
  case mPort of
    Nothing ->
      Warp.withApplicationSettings settings (pure app) $ \port -> do
        void $ banner port
        threadDelay maxBound
    Just port -> do
      void $ banner port
      Warp.runSettings (settings & Warp.setPort port) app
