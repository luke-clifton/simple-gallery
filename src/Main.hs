{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Text (Text, pack)
import Graphics.ImageMagick.MagickWand
import Lucid
import System.Directory
import System.Environment
import System.Exit
import System.FileLock
import System.FilePath
import System.IO

data Config = Config
    { cacheDir :: FilePath
    }

dir :: FilePath
dir = "cache"

toCachePath :: MonadReader Config m => FilePath -> m FilePath
toCachePath file = do
    dir <- asks cacheDir
    let
        base = takeBaseName file
    return $ dir </> base <.> "jpg"

isImage :: FilePath -> Bool
isImage file = or
    [ suffix `isSuffixOf` file
    | suffix <- ["png", "jpeg", "jpg", "tiff"]
    ]

updateCache :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
updateCache file = do
    cache <- toCachePath file
    
    fileExists <- liftIO $ doesFileExist cache
    expired <- liftIO $
        if fileExists
        then do
            cTime <- getModificationTime cache
            fTime <- getModificationTime file
            return (fTime > cTime)
        else
            return True

    when (fileExists && expired) (liftIO $ removeFile cache)

    if expired
    then do
        liftIO $ withMagickWandGenesis $ do
            (r,p) <- magickWand
            readImage p (pack file)
            scaleImage p 100 100
            writeImage p (Just $ pack cache)

    else return ()

generateListing :: MonadReader Config m => [FilePath] -> HtmlT m ()
generateListing fps = doctypehtml_
    $ body_ [style_ "padding: 0px; margin: 0px; background: black"]
    $ ul_ [ style_ "list-style-type: none; line-height: 0px; padding: 0px; margin: 0px" ]
    $ mapM_ imageRow fps
    where
        imageRow :: MonadReader Config m => FilePath -> HtmlT m ()
        imageRow fp = do
            cached <- pack <$> toCachePath fp
            li_ [ style_ "float: left"]
                $ a_ [ href_ . pack $ fp ]
                $ img_ [ src_ cached ]

loadImages :: (MonadReader Config m, MonadIO m) => m [FilePath]
loadImages = do
    imgs <- liftIO $ filter isImage <$> listDirectory "."
    mapM_ updateCache imgs
    return imgs

main :: IO ()
main = do
    config <- getArgs >>= \case
        [cacheDir] -> return Config{..}
        [] -> return Config{cacheDir="cache"}
        _ -> hPutStrLn stderr "Bad arguments: usage: simple-gallery [CACHE_DIR]" >> exitFailure
    flip runReaderT config $ do
        dir <- asks cacheDir
        liftIO $ do
            createDirectoryIfMissing False dir
            lockFile (dir </> "cache.lock") Exclusive
        imgs <- sort <$> loadImages
        doc <- renderBST $ generateListing imgs
        liftIO $ do
            withFile "index.html.tmp" WriteMode $ \h -> BS.hPutStr h doc
            renameFile "index.html.tmp" "index.html"
