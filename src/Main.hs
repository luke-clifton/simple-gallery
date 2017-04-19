{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Picture
import Codec.Picture.Extra
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Text (Text, pack)
import Lucid
import Network.CGI hiding (Html)
import System.Directory
import System.Environment
import System.FileLock
import System.FilePath

dir :: FilePath
dir = "cache"

toCachePath :: FilePath -> FilePath
toCachePath file =
    let
        base = takeBaseName file
        cache = dir </> base <.> "jpg"
    in
        cache

isImage :: FilePath -> Bool
isImage file = or
    [suffix `isSuffixOf` file
    | suffix <- ["png", "jpeg", "jpg", "tiff"]
    ]

updateCache :: FilePath -> IO ()
updateCache file = do
    let cache = toCachePath file
    
    fileExists <- doesFileExist cache
    expired <-
        if fileExists
        then do
            cTime <- getModificationTime cache
            fTime <- getModificationTime file
            return (fTime > cTime)
        else
            return True

    when (fileExists && expired) (removeFile cache)

    if expired
    then do
        eimg <- fmap (ImageRGB8 . scaleBilinear 100 100 . convertRGB8) <$> readImage file
        either putStrLn (saveJpgImage 90 cache) eimg
    else return ()

generateListing :: [FilePath] -> Html ()
generateListing fps = doctypehtml_
    $ body_ [style_ "padding: 0px; margin: 0px; background: black"]
    $ ul_ [ style_ "list-style-type: none; line-height: 0px; padding: 0px; margin: 0px" ]
    $ mapM_ imageRow fps
    where
        imageRow :: FilePath -> Html ()
        imageRow fp = li_ [ style_ "float: left"]
            $ a_ [ href_ . pack $ fp ]
            $ img_ [ src_ . pack . toCachePath $ fp ]

loadImages :: IO [FilePath]
loadImages = do
    lock <- tryLockFile "cache.lock" Exclusive
    case lock of
        -- TODO: A little concurrency would be nice.
        Just _ -> do
            imgs <- filter isImage <$> listDirectory "."
            mapM_ updateCache imgs
            return imgs
        Nothing -> filter isImage <$> listDirectory "."

main :: IO ()
main = do
    args <- getArgs
    imgs <- sort <$> loadImages
    when (args /= ["build-cache"]) $ runCGI $ handleErrors (cgiMain imgs)

cgiMain :: [FilePath] -> CGI CGIResult
cgiMain imgs = do
    liftIO $ createDirectoryIfMissing False dir
    setHeader "Content-type" "text/html; charset=UTF-8"
    outputFPS $ renderBS $ generateListing imgs

