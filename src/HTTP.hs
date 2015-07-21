module HTTP where

import Debug.Trace

httpLineSeparator = "\r\n"

type THTTPMethod = DHTTPMethod
data DHTTPMethod = OPTIONS | GET | HEAD | POST
                  | PUT | DELETE | TRACE | CONNECT | PATCH
    deriving (Show)
parseHTTPMethod :: String -> THTTPMethod
parseHTTPMethod string = case string of
  "OPTIONS" -> OPTIONS
  "GET" -> GET
  "HEAD" -> HEAD
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE
  "TRACE" -> TRACE
  "CONNECT" -> CONNECT
  "PATCH" -> PATCH

type THTTPFilename = String

type THTTPVersion = DHTTPVersion
data DHTTPVersion = OneOh | OneOne | TwoOh
instance Show DHTTPVersion where
  show OneOh = "HTTP/1.0"
  show OneOne = "HTTP/1.1"
  show TwoOh = "HTTP/2.0"
parseHTTPVersion :: String -> THTTPVersion
parseHTTPVersion string = case string of
  "HTTP/1.0" -> OneOh
  "HTTP/1.1" -> OneOne
  "HTTP/2.0" -> TwoOh

type THTTPStatus = DHTTPStatus
data DHTTPStatus = CONTINUE_100
                  | OK_200
                  | BAD_RQST_400
                  | UNAUTH_401
                  | FORBIDDEN_403
                  | NOTFOUND_404
instance Show DHTTPStatus where
  show CONTINUE_100 = "100 Continue"
  show OK_200 = "200 OK"
  show BAD_RQST_400 = "400 Bad Request"
  show UNAUTH_401 = "401 Unauthorized"
  show FORBIDDEN_403 = "403 Forbidden"
  show NOTFOUND_404 = "404 Not Found"

type TContentType = DContentType
data DContentType = None | TEXT_HTML | TEXT_CSS | TEXT_PLAIN
                      | APPLICATION_X_JS
instance Show DContentType where
  show TEXT_HTML = "text/html"
  show TEXT_CSS = "text/css"
  show TEXT_PLAIN = "text/plain"
  show APPLICATION_X_JS = "application/x-javascript"
  show _ = "?"
initialContentType = None
parseContentType :: String -> TContentType
parseContentType string = case string of
  "text/html" -> TEXT_HTML
  "text/css" -> TEXT_CSS
  "text/plain" -> TEXT_PLAIN
  "application/x-javascript" -> APPLICATION_X_JS
  _ -> TEXT_PLAIN
