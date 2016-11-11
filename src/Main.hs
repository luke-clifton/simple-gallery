module Main where

import Network.CGI
import Lucid
import Codec.Picture
import Codec.Picture.Extra

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
    setHeader "Content-type" "text/html; charset=UTF-8"
    m <- pathInfo
    output $ show m


