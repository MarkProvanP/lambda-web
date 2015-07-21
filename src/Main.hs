import Network
import Control.Concurrent
import System.IO
import Data.Char
import Debug.Trace

import qualified Data.ByteString.Lazy as B

import HTTP
import Request
import Response

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 12345
  loop sock

loop sock = do
  (handle, hostname, portnumber) <- accept sock
  hSetBuffering handle LineBuffering
  forkIO $ handleConnection handle
  loop sock

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

handleConnection :: Handle -> IO ()
handleConnection handle = do
  request <- readRequest handle
  response <- handleRequest request
  handleResponse handle response

defaultFilename = "index.html"

readRequest :: Handle -> IO Request
readRequest handle = do
  incompleteRequest <- readFirstLineOfRequest handle
  completeRequest <- readNextLineOfRequest handle incompleteRequest
  return $ completeRequest

  where
    readFirstLineOfRequest :: Handle -> IO Request
    readFirstLineOfRequest handle = do
      firstLine <- hGetLine handle
      let split = words firstLine
      let httpMethod = parseHTTPMethod $ split !! 0
      let removedFrontSlash = tail $ split !! 1
      let httpFilename = case removedFrontSlash of
                            "" -> defaultFilename
                            s -> s
      let httpVersion = parseHTTPVersion $ split !! 2
      return $ createFirstLineRequest httpMethod httpFilename httpVersion

    readNextLineOfRequest :: Handle -> Request -> IO Request
    readNextLineOfRequest handle existingRequest = do
      headerLine <- hGetLine handle
      case headerLine of
        "\r" -> return existingRequest
        s -> let split = splitRequestHeader s
                 requestFieldName = split !! 0
                 requestFieldValue = split !! 1
                 changedRequest = addFieldToRequest existingRequest requestFieldName requestFieldValue
             in readNextLineOfRequest handle changedRequest

    splitRequestHeader :: String -> [String]
    splitRequestHeader s = case dropWhile isSpace s of
      "" -> []
      s' -> w : [s'']
            where (w, s'') = break isSpace s'

handleResponse :: Handle -> Response -> IO ()
handleResponse handle response = do
  hPutStr handle (show (sHTTPVersion response) ++ " " ++ show (sHTTPStatus response) ++ httpLineSeparator)
  hPutStr handle ("Content-Length: " ++ show (sContentLength response) ++ httpLineSeparator)
  hPutStr handle httpLineSeparator
  case (sFileHandle response) of
    Just fileHandle -> B.hGetContents fileHandle >>= B.hPut handle
    _ -> hPutStr handle "<!DOCTYPE html><html><head><title>Oops!</title></head><body><h1>Oh dear!</h1></body></html>"
  hFlush handle
  hClose handle
