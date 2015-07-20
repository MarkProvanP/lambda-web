import Network
import Control.Concurrent
import System.IO
import Debug.Trace
import Data.Char

import Server

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 12345
  loop sock

loop sock = do
  (handle, hostname, portnumber) <- accept sock
  forkIO $ handleConnection handle
  loop sock

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

handleConnection :: Handle -> IO ()
handleConnection handle = do
  request <- readRequest handle
  hPutStr handle msg
  hFlush handle
  hClose handle

readRequest :: Handle -> IO Request
readRequest handle = do
  incompleteRequest <- readFirstLineOfRequest handle
  readNextLineOfRequest handle incompleteRequest

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
      let split = splitRequestHeader headerLine
      let requestFieldName = split !! 0
      let requestFieldValue = split !! 1
      let changedRequest = addFieldToRequest existingRequest requestFieldName requestFieldValue
      return changedRequest

    splitRequestHeader :: String -> [String]
    splitRequestHeader s = case dropWhile isSpace s of
      "" -> []
      s' -> w : [s'']
            where (w, s'') = break isSpace s'
