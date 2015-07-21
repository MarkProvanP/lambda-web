module Response where

import System.IO
import Control.Exception

import HTTP
import Request

data Response = Response
  { sHTTPVersion :: THTTPVersion
  , sHTTPStatus :: THTTPStatus
  , sAccessControlAllowOrigin :: TAccessControlAllowOrigin
  , sAcceptPatch :: TAcceptPatch
  , sAcceptRanges :: TAcceptRanges
  , sAge :: TAge
  , sAllow :: TAllow
  , sCacheControl :: TCacheControl
  , sConnection :: TConnection
  , sContentDisposition :: TContentDisposition
  , sContentEncoding :: TContentEncoding
  , sContentLanguage :: TContentLanguage
  , sContentLength :: TContentLength
  , sContentLocation :: TContentLocation
  , sContentMD5 :: TContentMD5
  , sContentRange :: TContentRange
  , sContentType :: TContentType
  , sDate :: TDate
  , sETag :: TETag
  , sExpires :: TExpires
  , sLastModified :: TLastModified
  , sLink :: TLink
  , sLocation :: TLocation
  , sP3P :: TP3P
  , sPragma :: TPragma
  , sProxyAuthenticate :: TProxyAuthenticate
  , sPublicKeyPins :: TPublicKeyPins
  , sRefresh :: TRefresh
  , sRetryAfter :: TRetryAfter
  , sServer :: TServer
  , sSetCookie :: TSetCookie
  , sStatus :: TStatus
  , sStrictTransportAuthority :: TStrictTransportAuthority
  , sTrailer :: TTrailer
  , sTransferEncoding :: TTransferEncoding
  , sUpgrade :: TUpgrade
  , sVary :: TVary
  , sVia :: TVia
  , sWarning :: TWarning
  , sWWWAuthenticate :: TWWWAuthenticate
  , sXFrameOptions :: TXFrameOptions
  }

handleRequest :: Request -> Response
handleRequest request = undefined

retrieveFilename :: String -> (THTTPStatus, Maybe Handle)
retrieveFilename filename = handle fileNotFound tryOpenFile
  where
    tryOpenFile = do
      handle <- openFile filename ReadMode
      return (OK_200, Just handle)
    fileNotFound = do
      handle <- openFile fourOhFourPageFilename ReadMode
      return (NOTFOUND_404, Just handle)

fourOhFourPageFilename = "404.html"
