module Server where

import Debug.Trace

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


type TAccept = String
initialAccept = ""
changeRAccept request value = request { rAccept = value }

type TAcceptCharset = String
initialAcceptCharset = ""
changeRAcceptCharset request value = request { rAcceptCharset = value }

type TAcceptEncoding = String
initialAcceptEncoding = ""
changeRAcceptEncoding request value = request { rAcceptEncoding = value }

type TAcceptLanguge = String
initialAcceptLanguage = ""
changeRAcceptLanguage request value = request { rAcceptLanguage = value }

type TAcceptDatetime = String
initialAcceptDatetime = ""
changeRAcceptDatetime request value = request { rAcceptLanguage = value }

type TAuthorization = String
initialAuthorization = ""
changeRAuthorization request value = request { rAuthorization = value }

type TCacheControl = String
initialCacheControl = ""
changeRCacheControl request value = request { rCacheControl = value }

type TConnection = String
initialConnection = ""
changeRConnection request value = request { rConnection = value }

type TCookie = String
initialCookie = ""
changeRCookie request value = request { rCookie = value }

type TContentLength = String
initialContentLength = ""
changeRContentLength request value = request { rContentLength = value }

type TContentMD5 = String
initialContentMD5 = ""
changeRContentMD5 request value = request { rContentMD5 = value }

type TContentType = String
initialContentType = ""
changeRContentType request value = request { rContentType = value }

type TDate = String
initialDate = ""
changeRDate request value = request { rDate = value }

type TExpect = String
initialExpect = ""
changeRExpect request value = request { rExpect = value }

type TFrom = String
initialFrom = ""
changeRFrom request value = request { rFrom = value }

type THost = String
initialHost = ""
changeRHost request value = request { rHost = value }

type TIfMatch = String
initialIfMatch = ""
changeRIfMatch request value = request { rIfMatch = value }

type TIfModifiedSince = String
initialIfModifiedSince = ""
changeRIfModifiedSince request value = request { rIfModifiedSince = value }

type TIfNoneMatch = String
initialIfNoneMatch = ""
changeRIfNoneMatch request value = request { rIfNoneMatch = value }

type TIfRange = String
initialIfRange = ""
changeRIfRange request value = request { rIfRange = value }

type TIfUnmodifiedSince = String
initialIfUnmodifiedSince = ""
changeRIfUnmodifiedSince request value = request { rIfUnmodifiedSince = value }

type TMaxForwards = String
initialMaxForwards = ""
changeRMaxForwards request value = request { rMaxForwards = value }

type TOrigin = String
initialOrigin = ""
changeROrigin request value = request { rOrigin = value }

type TPragma = String
initialPragma = ""
changeRPragma request value = request { rPragma = value }

type TProxyAuthorization = String
initialProxyAuthorization = ""
changeRProxyAuthorization request value = request { rProxyAuthorization = value }

type TRange = String
initialRange = ""
changeRRange request value = request { rRange = value }

type TReferer = String
initialReferer = ""
changeRReferer request value = request { rReferer = value }

type TTE = String
initialTE = ""
changeRTE request value = request { rTE = value }

type TUserAgent = String
initialUserAgent = ""
changeRUserAgent request value = request { rUserAgent = value }

type TUpgrade = String
initialUpgrade = ""
changeRUpgrade request value = request { rUpgrade = value }

type TVia = String
initialVia = ""
changeRVia request value = request { rVia = value }

type TWarning = String
initialWarning = ""
changeRWarning request value = request { rWarning = value }

type TXRequestedWith = String
initialXRequestedWith = ""
changeRXRequestedWith request value = request { rXRequestedWith = value }

type TDNT = String
initialDNT = ""
changeRDNT request value = request { rDNT = value }

type TXForwardedFor = String
initialXForwardedFor = ""
changeRXForwardedFor request value = request { rXForwardedFor = value }

type TXForwardedHost = String
initialXForwardedHost = ""
changeRXForwardedHost request value = request { rXForwardedHost = value }

type TXForwardedProto = String
initialXForwardedProto = ""
changeRXForwardedProto request value = request { rXForwardedProto = value }

type TFrontEndHttps = String
initialFrontEndHttps = ""
changeRFrontEndHttps request value = request { rFrontEndHttps = value }

type TXHttpMethodOverride = String
initialXHttpMethodOverride = ""
changeRXHttpMethodOverride request value = request { rXHttpMethodOverride = value }

type TXATTDeviceId = String
initialXATTDeviceId = ""
changeRXATTDeviceId request value = request { rXATTDeviceId = value }

type TXWapProfile = String
initialXWapProfile = ""
changeRXWapProfile request value = request { rXWapProfile = value }

type TProxyConnection = String
initialProxyConnection = ""
changeRProxyConnection request value = request { rProxyConnection = value }

type TXUIDH = String
initialXUIDH = ""
changeRXUIDH request value = request { rXUIDH = value }

type TXCsrfToken = String
initialXCsrfToken = ""
changeRXCsrfToken request value = request { rXCsrfToken = value }

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

data Request = Request
  { rHTTPMethod :: THTTPMethod
  , rHTTPFilename :: THTTPFilename
  , rHTTPVersion :: THTTPVersion
  , rAccept :: TAccept
  , rAcceptCharset :: TAcceptCharset
  , rAcceptEncoding :: TAcceptEncoding
  , rAcceptLanguage :: TAcceptLanguge
  , rAcceptDatetime :: TAcceptDatetime
  , rAuthorization :: TAuthorization
  , rCacheControl :: TCacheControl
  , rConnection :: TConnection
  , rCookie :: TCookie
  , rContentLength :: TContentLength
  , rContentMD5 :: TContentMD5
  , rContentType :: TContentType
  , rDate :: TDate
  , rExpect :: TExpect
  , rFrom :: TFrom
  , rHost :: THost
  , rIfMatch :: TIfMatch
  , rIfModifiedSince :: TIfModifiedSince
  , rIfNoneMatch :: TIfNoneMatch
  , rIfRange :: TIfRange
  , rIfUnmodifiedSince :: TIfUnmodifiedSince
  , rMaxForwards :: TMaxForwards
  , rOrigin :: TOrigin
  , rPragma :: TPragma
  , rProxyAuthorization :: TProxyAuthorization
  , rRange :: TRange
  , rReferer :: TReferer
  , rTE :: TTE
  , rUserAgent :: TUserAgent
  , rUpgrade :: TUpgrade
  , rVia :: TVia
  , rWarning :: TWarning
  , rXRequestedWith :: TXRequestedWith
  , rDNT :: TDNT
  , rXForwardedFor :: TXForwardedFor
  , rXForwardedHost :: TXForwardedHost
  , rXForwardedProto :: TXForwardedProto
  , rFrontEndHttps :: TFrontEndHttps
  , rXHttpMethodOverride :: TXHttpMethodOverride
  , rXATTDeviceId :: TXATTDeviceId
  , rXWapProfile :: TXWapProfile
  , rProxyConnection :: TProxyConnection
  , rXUIDH :: TXUIDH
  , rXCsrfToken :: TXCsrfToken
  } deriving (Show)

createFirstLineRequest :: THTTPMethod
                       -> THTTPFilename
                       -> THTTPVersion
                       -> Request
createFirstLineRequest method filename version
  = Request method filename version
   initialAccept
   initialAcceptCharset
   initialAcceptEncoding
   initialAcceptLanguage
   initialAcceptDatetime
   initialAuthorization
   initialCacheControl
   initialConnection
   initialCookie
   initialContentLength
   initialContentMD5
   initialContentType
   initialDate
   initialExpect
   initialFrom
   initialHost
   initialIfMatch
   initialIfModifiedSince
   initialIfNoneMatch
   initialIfRange
   initialIfUnmodifiedSince
   initialMaxForwards
   initialOrigin
   initialPragma
   initialProxyAuthorization
   initialRange
   initialReferer
   initialTE
   initialUserAgent
   initialUpgrade
   initialVia
   initialWarning
   initialXRequestedWith
   initialDNT
   initialXForwardedFor
   initialXForwardedHost
   initialXForwardedProto
   initialFrontEndHttps
   initialXHttpMethodOverride
   initialXATTDeviceId
   initialXWapProfile
   initialProxyConnection
   initialXUIDH
   initialXCsrfToken

addFieldToRequest :: Request
                  -> String
                  -> String
                  -> Request
addFieldToRequest existingRequest fieldName fieldValue
 = case fieldName of
   "Accept:" -> changeRAccept existingRequest fieldValue
   "Accept-Charset:" -> changeRAcceptCharset existingRequest fieldValue
   "Accept-Encoding:" -> changeRAcceptEncoding existingRequest fieldValue
   "Accept-Language:" -> changeRAcceptLanguage existingRequest fieldValue
   "Accept-Datetime:" -> changeRAcceptDatetime existingRequest fieldValue
   "Authorization:" -> changeRAuthorization existingRequest fieldValue
   "Cache-Control:" -> changeRCacheControl existingRequest fieldValue
   "Connection:" -> changeRConnection existingRequest fieldValue
   "Cookie:" -> changeRCookie existingRequest fieldValue
   "Content-Length:" -> changeRContentLength existingRequest fieldValue
   "Content-MD5:" -> changeRContentMD5 existingRequest fieldValue
   "Content-Type:" -> changeRContentType existingRequest fieldValue
   "Date:" -> changeRDate existingRequest fieldValue
   "Expect:" -> changeRExpect existingRequest fieldValue
   "From:" -> changeRFrom existingRequest fieldValue
   "Host:" -> changeRHost existingRequest fieldValue
   "If-Match:" -> changeRIfMatch existingRequest fieldValue
   "If-Modified-Since:" -> changeRIfModifiedSince existingRequest fieldValue
   "If-None-Match:" -> changeRIfNoneMatch existingRequest fieldValue
   "If-Range:" -> changeRIfRange existingRequest fieldValue
   "If-Unmodified-Since:" -> changeRIfUnmodifiedSince existingRequest fieldValue
   "Max-Forwards:" -> changeRMaxForwards existingRequest fieldValue
   "Origin:" -> changeROrigin existingRequest fieldValue
   "Pragma:" -> changeRPragma existingRequest fieldValue
   "Proxy-Authorization:" -> changeRProxyAuthorization existingRequest fieldValue
   "Range:" -> changeRRange existingRequest fieldValue
   "Referer:" -> changeRReferer existingRequest fieldValue
   "TE:" -> changeRTE existingRequest fieldValue
   "User-Agent:" -> changeRUserAgent existingRequest fieldValue
   "Upgrade:" -> changeRUpgrade existingRequest fieldValue
   "Via:" -> changeRVia existingRequest fieldValue
   "Warning:" -> changeRWarning existingRequest fieldValue
   "X-Requested-With:" -> changeRXRequestedWith existingRequest fieldValue
   "DNT:" -> changeRDNT existingRequest fieldValue
   "X-Forwarded-For:" -> changeRXForwardedFor existingRequest fieldValue
   "X-Forwarded-Host:" -> changeRXForwardedHost existingRequest fieldValue
   "X-Forwarded-Proto:" -> changeRXForwardedProto existingRequest fieldValue
   "Front-End-Https:" -> changeRFrontEndHttps existingRequest fieldValue
   "X-Http-Method-Override:" -> changeRXHttpMethodOverride existingRequest fieldValue
   "X-ATT-DeviceId:" -> changeRXATTDeviceId existingRequest fieldValue
   "X-Wap-Profile:" -> changeRXWapProfile existingRequest fieldValue
   "Proxy-Connection:" -> changeRProxyConnection existingRequest fieldValue
   "X-UIDH:" -> changeRXUIDH existingRequest fieldValue
   "X-Csrf-Token:" -> changeRXCsrfToken existingRequest fieldValue
   _ -> trace ("Could not identify request field " ++ fieldName) existingRequest

data Response = Response
  { sHTTPVersion :: THTTPVersion
  , sHTTPStatus :: THTTPStatus
  }

handleRequest :: Request -> Response
handleRequest request = undefined
