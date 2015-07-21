import Network
import Control.Concurrent
import System.IO
import Data.Char
import Debug.Trace

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
  let response = handleRequest request
  handleResponse handle response


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
      let httpFilename = split !! 1
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
  hPutStr handle msg
  hFlush handle
  hClose handle
