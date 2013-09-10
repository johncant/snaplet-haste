{-# language OverloadedStrings #-}

-- | Snaplet that serves javascript files compiled with haste 
--   (https://github.com/valderman/haste-compiler). This Snaplet is meant to be
--   used for development. You can work on client side Haskell code and
--   immedietely test the code with a simple browser reload. It certainly adds
--   some overhead and is not meant to be used in production websites.
--
-- Usage:
--
-- Put haskell source files in the snaplet path (e.g. @$ROOT\/snaplets\/haste@).
-- For every such haskell file there will be a new javascript file available via
-- http.
--
-- * Other files won't be served through http. The snaplet will 'mzero' on .hs,
--   .hi, .o and all other files.
--
-- * The haste snaplet does not track haskell import dependencies. When any
--   haskell file in the snaplet path is newer than the requested javascript
--   file, it will be recompiled.
--
-- * If hastec exits with an error code this snaplet will serve a special
--   javascript file that contains the error message as a comment and a
--   javascript command that will raise the error message as a javascript
--   exception.


module Snap.Snaplet.Haste (
    Haste,
    initialize,
  ) where


import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class as State
import Data.String.Conversions
import Snap.Core
import Snap.Snaplet
import System.Directory
import System.Exit
import Data.List
import System.FilePath
import System.Process
import Snap.Util.FileServe
import Text.Printf


-- | Internal data type for the haste snaplet.
data Haste = Haste FilePath [String]

-- | Initializes the haste snaplet. Use it with e.g. 'nestSnaplet'.
initialize :: SnapletInit app Haste
initialize = makeSnaplet "haste" description Nothing $ do
    addRoutes [("", handler)]
    return $ Haste "hastec" []
  where
    description = "handler for delivering javascript files compiled with haste"

handler :: Handler app Haste ()
handler = do
    jsPath <- cs <$> rqPathInfo <$> getRequest
    hasteDir <- getSnapletFilePath
    if takeExtension jsPath /= ".js" then
        mzero
      else
        deliverJS (dropExtension (hasteDir </> jsPath))

deliverJS :: FilePath -> Handler app Haste ()
deliverJS basename = do
--     Haste hastec args <- State.get
    hsExists <- liftIO $ doesFileExist (basename <.> "hs")
    when (not hsExists) mzero
    snapletDir <- getSnapletFilePath
    jsNewer <- liftIO $ isJSNewer (basename <.> "js") snapletDir
    if jsNewer then
            serveFile (basename <.> "js")
          else
            compile basename

-- | Returns whether the given javascript file exists and is newer than
--   all Haskell files in the given directory.
isJSNewer :: FilePath -> FilePath -> IO Bool
isJSNewer jsFile dir = do
    exists <- liftIO $ doesFileExist jsFile
    if not exists then
        return False
      else do
        hsFiles <- collectAllHsFiles dir
        hsTimeStamps <- mapM getModificationTime hsFiles
        jsTimeStamp <- getModificationTime jsFile
        return (jsTimeStamp > maximum hsTimeStamps)
  where
    collectAllHsFiles :: FilePath -> IO [FilePath]
    collectAllHsFiles dir = do
        paths <- fmap (dir </>) <$>
            filter (not . ("." `isPrefixOf`)) <$>
            getDirectoryContents dir
        (files, dirs) <- partitionM doesFileExist paths
        let hsFiles = filter (\ f -> takeExtension f == ".hs") files
        subHsFiles <- concat <$> mapM collectAllHsFiles dirs
        return (hsFiles ++ subHsFiles)

    partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
    partitionM pred (a : r) = do
        is <- pred a
        (yes, no) <- partitionM pred r
        return $ if is then (a : yes, no) else (yes, a : no)
    partitionM pred [] = return ([], [])

compile :: FilePath -> Handler app Haste ()
compile name = do
    Haste hastec snapletArgs <- State.get
    let args = snapletArgs ++ [name <.> "hs"]
    wd <- liftIO getCurrentDirectory
    liftIO . setCurrentDirectory =<< getSnapletFilePath
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode hastec args ""
    liftIO $ setCurrentDirectory wd
    case exitCode of
        ExitFailure _ ->
            writeBS $ cs (printf ("/*\n\n%s\n\n*/\n\nconsole.log(%s);") stderr (show stderr) :: String)
        ExitSuccess -> serveFile (name <.> "js")
