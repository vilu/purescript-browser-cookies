-- | A module providing operations to manipulate cookies in the browser
module Browser.Cookies.Data where

import Prelude

import Data.Array (catMaybes, intercalate)
import Data.JSDate (JSDate, toUTCString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- | The type of a Cookie value.
-- | This is separated from CookieOpts because they won't be returned on a read.
newtype Cookie = Cookie {
    key :: String
  , value :: String
}
derive instance newtypeCookie :: Newtype Cookie _
derive instance eqCookie :: Eq Cookie

-- | The type needed for setting Cookie values.
-- | This is a compound of Cookie and CookieOpts because the latter is only relevant on a write.
newtype SetCookie = SetCookie {
    cookie :: Cookie
  , opts :: Maybe CookieOpts
}
derive instance newtypeSetCookie :: Newtype SetCookie _
derive instance eqSetCookie :: Eq SetCookie

-- | Prevent browser from sending this cookie along with cross-site requests.
-- | https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies#SameSite_cookies
data SameSite = Lax | Strict
derive instance eqSamesite :: Eq SameSite

-- | Possible options that can be set for a cookie
-- | maxAge, expires: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies#Permanent_cookies
-- | secure, httpOnly: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies#Secure_and_HttpOnly_cookies
-- | samesite: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies#SameSite_cookies
-- | domain, path: https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies#Scope_of_cookies
newtype CookieOpts = CookieOpts {
    maxAge :: Maybe Number
  , expires :: Maybe JSDate
  , secure :: Boolean
  , httpOnly :: Boolean
  , samesite :: Maybe SameSite
  , domain :: Maybe String
  , path :: Maybe String
}
derive instance newtypeCookieOpts :: Newtype CookieOpts _
derive instance eqCookieOpts :: Eq CookieOpts

-- | encodes a value a to a string that matches the api described here https://tools.ietf.org/html/rfc6265
class CookieEncoder a where
  encode :: a -> String

instance jsDateCookieEncoder :: CookieEncoder JSDate where 
    encode d = toUTCString d

instance sameSiteCookieEncoder :: CookieEncoder SameSite where 
    encode Lax = "lax"
    encode Strict = "strict"

instance numberCookieEncoder :: CookieEncoder Number where 
    encode num = show num

instance cookieOptsEncoder :: CookieEncoder CookieOpts where 
    encode (CookieOpts { maxAge, expires, secure, httpOnly, samesite, domain, path}) = intercalate "; " $ catMaybes optMaybies
        where 
            optMaybies = [
                (\n -> "max-age=" <> encode n) <$> maxAge,
                (\d -> "expires=" <> encode d) <$> expires,
                if (secure) then Just "secure" else Nothing,
                if (httpOnly) then Just "httponly" else Nothing,
                (\ss -> "samesite=" <> encode ss) <$> samesite,
                (\str -> "domain=" <> str) <$> domain,
                (\str -> "path=" <> str) <$> path
            ]
  
instance cookieEncoder :: CookieEncoder Cookie where 
    encode (Cookie {key, value}) = key <> "=" <> value

instance setCookieEncoder :: CookieEncoder SetCookie where 
    encode (SetCookie {cookie, opts}) = intercalate "; " $ catMaybes [
        Just (encode cookie)
      , encode <$> opts
    ]
