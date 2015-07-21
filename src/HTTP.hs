{-|
Module      : HTTP
Description : Common functionality and data types needed by server
Copyright   : (c) Mark Provan, 2014
License     : MIT
Maintainer  : markprovanp@gmail.com
Stability   : experimental
Portability : Windows & POSIX

The HTTP module stores all the common data types and functions needed across the server code.
-}

module HTTP where

import Debug.Trace

-- | The '\r\n' line separator used for all HTTP communication.
httpLineSeparator :: String
httpLineSeparator = "\r\n"

-- | The HTTP request method type.
data HTTPMethod = OPTIONS -- ^ Check which HTTP methods are supported by the server
                | GET -- ^ Retrieve resource from server
                | HEAD -- ^ Retrieve only header of resource from server
                | POST -- ^ Receive data from client and store it within the given URI
                | PUT -- ^ Receive data from client and store it at the given URI
                | DELETE -- ^ Delete a given resource
                | TRACE -- ^ Return the request to the client
                | CONNECT -- ^ Not to be implemented
                | PATCH -- ^ Not to be implemented
    deriving (Show)
parseHTTPMethod :: String -- ^ The string as sent over the socket connection
                -> HTTPMethod -- ^ The correct HTTP Method data type
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

-- | The default type of the HTTP filename is 'String'
type THTTPFilename = String

data HTTPVersion = OneOh -- ^ 'HTTP/1.0'
                  | OneOne -- ^ 'HTTP/1.1'
                  | TwoOh -- ^ 'HTTP/2.0'
instance Show HTTPVersion where
  show OneOh = "HTTP/1.0"
  show OneOne = "HTTP/1.1"
  show TwoOh = "HTTP/2.0"
-- | Converts the string of the HTTP version to the data type
parseHTTPVersion :: String -- ^ The string as sent over the socket connection
                 -> HTTPVersion -- ^ The correct HTTP version data type
parseHTTPVersion string = case string of
  "HTTP/1.0" -> OneOh
  "HTTP/1.1" -> OneOne
  "HTTP/2.0" -> TwoOh

data HTTPStatus = CONTINUE_100 -- ^ '100 Continue'
                  | OK_200 -- ^ '200 OK'
                  | BAD_RQST_400 -- ^ '400 Bad Request'
                  | UNAUTH_401 -- ^ '401 Unauthorized'
                  | FORBIDDEN_403 -- ^ '403 Forbidden'
                  | NOTFOUND_404 -- ^ '404 Not Found'
instance Show HTTPStatus where
  show CONTINUE_100 = "100 Continue"
  show OK_200 = "200 OK"
  show BAD_RQST_400 = "400 Bad Request"
  show UNAUTH_401 = "401 Unauthorized"
  show FORBIDDEN_403 = "403 Forbidden"
  show NOTFOUND_404 = "404 Not Found"

data ContentType = None -- ^ No type specified. This is for internal use only
                  | TEXT_HTML -- ^ text/html
                  | TEXT_CSS -- ^ text/css
                  | TEXT_PLAIN -- ^ text/plain
                  | APPLICATION_X_JS -- ^ application/x-javascript
instance Show ContentType where
  show TEXT_HTML = "text/html"
  show TEXT_CSS = "text/css"
  show TEXT_PLAIN = "text/plain"
  show APPLICATION_X_JS = "application/x-javascript"
  show _ = undefined

-- | The initial ContentType to be used when creating Request or Response objects
initialContentType :: ContentType
initialContentType = None

-- | Converts the string of the content type to the data type
parseContentType :: String -- ^ The string as sent over the socket connection
                 -> ContentType -- ^ The correct content type data type
parseContentType string = case string of
  "text/html" -> TEXT_HTML
  "text/css" -> TEXT_CSS
  "text/plain" -> TEXT_PLAIN
  "application/x-javascript" -> APPLICATION_X_JS
  _ -> TEXT_PLAIN
