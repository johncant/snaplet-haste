{-# language OverloadedStrings #-}

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


data Haste = Haste FilePath [String]

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
