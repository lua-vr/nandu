{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ema.Generate (
  generateSiteFromModel,
  generateSiteFromModel',
) where

import Colog
import Control.Exception (throwIO)
import Ema.Asset (Asset (..))
import Ema.CLI (AppM, crash)
import Ema.Route.Class (IsRoute (RouteModel, routePrism, routeUniverse))
import Ema.Route.Prism (
  checkRoutePrismGivenRoute,
  fromPrism_,
 )
import Ema.Site (EmaSite (siteOutput), EmaStaticSite)
import Optics.Core (review)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import UnliftIO.Async (pooledForConcurrently)

{- | Generate the static site at `dest`

  The *only* data we need is the `RouteModel`.
-}
generateSiteFromModel ::
  forall r.
  (Eq r, Show r, IsRoute r, EmaStaticSite r) =>
  -- | Target directory to write files to. Must exist.
  FilePath ->
  -- | The model data used to generate assets.
  RouteModel r ->
  AppM [FilePath]
generateSiteFromModel dest model =
  withBlockBuffering $ generateSiteFromModel' @r dest model
  where
    -- Temporarily use block buffering before calling an IO action that is
    -- known ahead to log rapidly, so as to not hamper serial processing speed.
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)

-- | Like `generateSiteFromModel` but without buffering or error handling.
generateSiteFromModel' ::
  forall r.
  (Eq r, Show r, EmaStaticSite r) =>
  FilePath ->
  RouteModel r ->
  -- | List of generated files.
  AppM [FilePath]
generateSiteFromModel' dest model = do
  let enc = routePrism @r
      rp = fromPrism_ $ enc model
  -- Sanity checks
  unlessM (liftIO $ doesDirectoryExist dest) $ do
    crash $ "Destination directory does not exist: " <> toText dest
  let routes = routeUniverse @r model
  when (null routes) $
    crash "Your app's `routeUniverse` is empty; nothing to generate!"
  forM_ routes $ \route ->
    checkRoutePrismGivenRoute enc model route
      `whenLeft_` crash
  -- For Github Pages
  noBirdbrainedJekyll dest
  -- Enumerate and write all routes.
  logInfo $ "Writing " <> show (length routes) <> " routes"
  fmap concat . pooledForConcurrently routes $ \r -> do
    let fp = dest </> review rp r
    siteOutput rp model r >>= \case
      AssetStatic staticPath -> do
        liftIO (doesPathExist staticPath) >>= \case
          True ->
            -- NOTE: A static path can indeed be a directory. The user is not
            -- obliged to recursively list the files.
            copyRecursively staticPath fp
          False ->
            logError $ toText $ "? " <> staticPath <> " (missing)"
        pure []
      AssetGenerated _fmt !s -> do
        writeFileGenerated fp s
        pure [fp]

{- | Disable birdbrained hacks from GitHub to disable surprises like,
 https://github.com/jekyll/jekyll/issues/55
-}
noBirdbrainedJekyll :: FilePath -> AppM ()
noBirdbrainedJekyll dest = do
  let nojekyll = dest </> ".nojekyll"
  liftIO (doesFileExist nojekyll) >>= \case
    True -> pass
    False -> do
      logInfo $ "Disabling Jekyll by writing " <> toText nojekyll
      writeFileLBS nojekyll ""

newtype StaticAssetMissing = StaticAssetMissing FilePath
  deriving stock (Show)
  deriving anyclass (Exception)

writeFileGenerated :: FilePath -> LByteString -> AppM ()
writeFileGenerated fp s = do
  logInfo $ toText $ "W " <> fp
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory fp)
    writeFileLBS fp s

{- | Copy a file or directory recursively to the target directory

  Like `cp -R src dest`.
-}
copyRecursively ::
  -- | Absolute path to source file or directory to copy.
  FilePath ->
  -- | Target file or directory path.
  FilePath ->
  AppM ()
copyRecursively src dest = do
  fs <- enumerateFilesToCopy src dest
  forM_ fs $ \(a, b) -> do
    logInfo $ toText $ "C " <> b
    copyFileCreatingParents a b
  where
    enumerateFilesToCopy :: FilePath -> FilePath -> AppM [(FilePath, FilePath)]
    enumerateFilesToCopy a b = do
      liftIO (doesFileExist a) >>= \case
        True ->
          pure [(a, b)]
        False -> do
          liftIO (doesDirectoryExist a) >>= \case
            False ->
              liftIO $ throwIO $ StaticAssetMissing a
            True -> do
              fs <- liftIO $ getDirectoryFiles src ["**"]
              pure $ fs <&> \fp -> (a </> fp, b </> fp)

    copyFileCreatingParents a b =
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory b)
        copyFile a b
