module Server where

type THTTPMethod = DHTTPMethod
data DHTTPMethod = OPTIONS | GET | HEAD | POST
                  | PUT | DELETE | TRACE | CONNECT | PATCH
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
type TConnection = String
initialConnection = ""
type TCookie = String
initialCookie = ""
type TContentLength = String
initialContentLength = ""
type TContentMD5 = String
initialContentMD5 = ""
type TContentType = String
initialContentType = ""
type TDate = String
initialDate = ""
type TExcept = String
initialExcept = ""
type TFrom = String
initialFrom = ""
type THost = String
initialHost = ""
type TIfMatch = String
initialIfMatch = ""
type TIfModifiedSince = String
initialIfModifiedSince = ""
type TIfNoneMatch = String
initialIfNoneMatch = ""
type TIfRange = String
initialIfRange = ""
type TIfUnmodifiedSince = String
initialIfUnmodifiedSince = ""
type TMaxForwards = String
initialMaxForwards = ""
type TOrigin = String
initialOrigin = ""
type TPragma = String
initialPragma = ""
type TProxyAuthorization = String
initialProxyAuthorization = ""
type TRange = String
initialRange = ""
type TReferer = String
initialReferer = ""
type TTE = String
initialTE = ""
type TUserAgent = String
initialUserAgent = ""
type TUpgrade = String
initialUpgrade = ""
type TVia = String
initialVia = ""
type TWarning = String
initialWarning = ""
type TXRequestedWith = String
initialXRequestedWith = ""
type TDNT = String
initialDNT = ""
type TXForwardedFor = String
initialXForwardedFor = ""
type TXForwardedHost = String
initialXForwardedHost = ""
type TXForwardedProto = String
initialXForwardedProto = ""
type TFrontEndHttps = String
initialFrontEndHttps = ""
type TXHttpMethodOverride = String
initialXHttpMethodOverride = ""
type TXATTDeviceId = String
initialXATTDeviceId = ""
type TXWapProfile = String
initialXWapProfile = ""
type TProxyConnection = String
initialProxyConnection = ""
type TXUIDH = String
initialXUIDH = ""
type TXCsrfToken = String
initialXCsrfToken = ""

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
  , rExcept :: TExcept
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
  }

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
   initialExcept
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
   _ -> existingRequest
