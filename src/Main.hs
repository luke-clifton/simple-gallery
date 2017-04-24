{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Graphics.ImageMagick.MagickWand
import Lucid
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FileLock
import System.FilePath
import System.IO


-----------------------------------------------------------
-- Config
-----------------------------------------------------------
data Config = Config
    { cacheDir :: FilePath
    , thumbSize :: ThumbSize
    , compression :: Int
    , verbose :: Bool
    } deriving Show

-----------------------------------------------------------
-- ThumbSize
-----------------------------------------------------------
data ThumbSize
    = ThumbFixed Int Int
    | ThumbAspect Int

instance Show ThumbSize where
    show (ThumbFixed w h) = show w ++ "x" ++ show h
    show (ThumbAspect h) = show h

instance Read ThumbSize where
    readsPrec _ s = do
        (w,rest) <- reads s
        case rest of
            'x':height -> do
                (h,extra) <- reads height
                return (ThumbFixed w h, extra)
            extra -> return (ThumbAspect w, extra)


-----------------------------------------------------------
-- Cache handling
-----------------------------------------------------------
toCachePath :: MonadReader Config m => FilePath -> m FilePath
toCachePath file = do
    dir <- asks cacheDir
    dim <- show <$> asks thumbSize
    com <- show <$> asks compression
    let
        base = takeBaseName file
    return $ dir </> base <.> dim <.> com <.> "jpg"

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

    when (fileExists && expired) $ do
        msg $ "Removing " ++ cache ++ " (expired)"
        liftIO $ removeFile cache

    thumb <- asks thumbSize
    compr <- asks compression
    if expired
    then do
        msg $ "Generating thumbnail for " ++ file
        liftIO $ withMagickWandGenesis $ do
            (r,p) <- magickWand
            readImage p (pack file)
            (w,h) <- case thumb of
                ThumbFixed w h -> return (w,h)
                ThumbAspect h -> do
                    oh <- getImageHeight p
                    ow <- getImageWidth p
                    let
                        r = fromIntegral h / fromIntegral oh :: Double
                        w = round $ fromIntegral ow * r
                    return (w,h)

            setImageCompressionQuality p compr
            scaleImage p w h
            writeImage p (Just $ pack cache)

    else msg $ "Nothing to do for " ++ file

-----------------------------------------------------------
-- Generate HTML
-----------------------------------------------------------
generateListing :: MonadReader Config m => [FilePath] -> HtmlT m ()
generateListing fps = doctypehtml_ $ do
    head_ $ style_ "li { display: inline; }"
    body_ [style_ "padding: 0px; margin: 0px; background: black"]
        $ ul_ [ style_ "list-style-type: none; line-height: 0px; padding: 0px; margin: 0px; text-align: center;" ]
        $ mapM_ imageRow fps
    where
        imageRow :: MonadReader Config m => FilePath -> HtmlT m ()
        imageRow fp = do
            cached <- pack <$> toCachePath fp
            li_ $ a_ [ href_ . pack $ fp ] $ img_ [ src_ cached ]

-----------------------------------------------------------
-- Load Images
-----------------------------------------------------------
loadImages :: (MonadReader Config m, MonadIO m) => m [FilePath]
loadImages = do
    imgs <- liftIO $ filter isImage <$> listDirectory "."
    mapM_ updateCache imgs
    return imgs

-----------------------------------------------------------
-- Option handling
-----------------------------------------------------------
parseConfig :: Parser Config
parseConfig = Config
    <$> parseCacheDir
    <*> parseThumb
    <*> parseCompression
    <*> parseVerbose
    where
        parseCacheDir = strOption
            ( long "cache"
            <> metavar "DIR"
            <> help "Where to save cached thumbnails, default: cache"
            <> value "cache"
            )
        parseThumb = option auto
            ( long "dim"
            <> help "Size of thumbnails. 50x20 for fixed size. 20 for fixed height with correct aspect ratio, default: 60"
            <> value (ThumbAspect 60)
            )
        parseCompression = option auto
            ( long "compression"
            <> help "compression amount (0-100), default: 60"
            <> value 60
            )
        parseVerbose = switch
            ( long "verbose"
            <> short 'v'
            <> help "Be more verbose"
            )

msg :: (MonadIO m, MonadReader Config m) => String -> m ()
msg m = asks verbose >>= \case
    True -> liftIO $ putStrLn m
    False -> return ()

main :: IO ()
main = do
    config@Config{..} <- execParser $ info (parseConfig <**> helper) (fullDesc)
    runReaderT (msg $ show config) config
    createDirectoryIfMissing False cacheDir
    withFileLock (cacheDir </> "cache.lock") Exclusive $ \_ ->
        flip runReaderT config $ do
            imgs <- sort <$> loadImages
            when (null imgs) $ msg "No images in current working directory"
            doc <- renderBST $ generateListing imgs
            liftIO $ do
                withFile "index.html.tmp" WriteMode $ \h -> BS.hPutStr h doc
                renameFile "index.html.tmp" "index.html"
