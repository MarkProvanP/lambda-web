module Response where

import System.IO
import System.IO.Error
import System.FilePath
import Control.Exception
import Debug.Trace

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
  , sFileHandle :: Maybe Handle
  }
   deriving (Show)

type TAccessControlAllowOrigin = String
initialAccessControlAllowOrigin = ""
changeSAccessControlAllowOrigin response value = response { sAccessControlAllowOrigin = value }

type TAcceptPatch = String
initialAcceptPatch = ""
changeSAcceptPatch response value = response { sAcceptPatch = value }

type TAcceptRanges = String
initialAcceptRanges = ""
changeSAcceptRanges response value = response { sAcceptRanges = value }

type TAge = String
initialAge = ""
changeSAge response value = response { sAge = value }

type TAllow = String
initialAllow = ""
changeSAllow response value = response { sAllow = value }

-- TCacheControl in Request.hs
changeSCacheControl response value = response { sCacheControl = value }

-- TConnection in Request.hs
changeSConnection response value = response { sConnection = value }

type TContentDisposition = String
initialContentDisposition = ""
changeSContentDisposition response value = response { sContentDisposition = value }

type TContentEncoding = String
initialContentEncoding = ""

type TContentLanguage = String
initialContentLanguage = ""

-- TContentLength in Request.hs

type TContentLocation = String
initialContentLocation = ""

-- TContentMD5 in Request.hs
type TContentRange = String
initialContentRange = ""

-- TContentType in HTTP.hs

-- TDate in Request.hs

type TETag = String
initialETag = ""

type TExpires = String
initialExpires = ""

type TLastModified = String
initialLastModified = ""

type TLink = String
initialLink = ""

type TLocation = String
initialLocation = ""

type TP3P = String
initialP3P = ""

-- TPragma in Request.hs

type TProxyAuthenticate = String
initialProxyAuthenticate = ""

type TPublicKeyPins = String
initialPublicKeyPins = ""

type TRefresh = String
initialRefresh = ""

type TRetryAfter = String
initialRetryAfter = ""

type TServer = String
initialServer = ""

type TSetCookie = String
initialSetCookie = ""

type TStatus = String
initialStatus = ""

type TStrictTransportAuthority = String
initialStrictTransportAuthority = ""

type TTrailer = String
initialTrailer = ""

type TTransferEncoding = String
initialTransferEncoding = ""

-- TUpgrade in Request.hs

type TVary = String
initialVary = ""

-- TVia in Request.hs

-- TWarning in Request.hs

type TWWWAuthenticate = String
initialWWWAuthenticate = ""

type TXFrameOptions = String
initialXFrameOptions = ""

initialFileHandle = Nothing

createResponse httpVersion httpStatus = Response httpVersion httpStatus
  initialAccessControlAllowOrigin
  initialAcceptPatch
  initialAcceptRanges
  initialAge
  initialAllow
  initialCacheControl
  initialConnection
  initialContentDisposition
  initialContentEncoding
  initialContentLanguage
  initialContentLength
  initialContentLocation
  initialContentMD5
  initialContentRange
  initialContentType
  initialDate
  initialETag
  initialExpires
  initialLastModified
  initialLink
  initialLocation
  initialP3P
  initialPragma
  initialProxyAuthenticate
  initialPublicKeyPins
  initialRefresh
  initialRetryAfter
  initialServer
  initialSetCookie
  initialStatus
  initialStrictTransportAuthority
  initialTrailer
  initialTransferEncoding
  initialUpgrade
  initialVary
  initialVia
  initialWarning
  initialWWWAuthenticate
  initialXFrameOptions
  initialFileHandle

handleRequest :: Request -> IO Response
handleRequest request = let requestFilename = rHTTPFilename request
                            contentType = detectContentType requestFilename
                            httpVersion = rHTTPVersion request
                          in do
                            putStrLn ("Requested filename: " ++ requestFilename)
                            (httpStatus, maybeHandle) <- retrieveFilename requestFilename
                            putStrLn ("Retrieved: " ++ (show httpStatus) ++ " " ++ (show maybeHandle))
                            case trace ("H") maybeHandle of
                              Just handle -> do
                                let defaultResponse = createResponse httpVersion httpStatus
                                let responseWithHandle = defaultResponse { sFileHandle = Just handle }
                                contentLength <- hFileSize handle
                                let responseWithContentLength = responseWithHandle { sContentLength = contentLength }
                                return $ trace ("Response: " ++ show responseWithContentLength) responseWithContentLength



retrieveFilename :: String -> IO (THTTPStatus, Maybe Handle)
retrieveFilename filename = do
  maybeHandle <- try (openFile filename ReadMode)
  case maybeHandle of
    Left e ->
      if isDoesNotExistError e then do
        fourOhFourPageHandle <- openFile fourOhFourPageFilename ReadMode
        return (NOTFOUND_404, Just fourOhFourPageHandle)
      else do
        fourOhThreePageHandle <- openFile fourOhThreePageFilename ReadMode
        return (FORBIDDEN_403, Just fourOhThreePageHandle)
    Right handle ->
      return (OK_200, Just handle)

fourOhThreePageFilename = "403.html"
fourOhFourPageFilename = "404.html"

detectContentType :: String -> TContentType
detectContentType filename = case strippedFilenameEnding of
  "html" -> TEXT_HTML
  "css" -> TEXT_CSS
  "txt" -> TEXT_PLAIN
  "js" -> APPLICATION_X_JS
  _ -> TEXT_PLAIN
  where strippedFilenameEnding = takeExtension filename
