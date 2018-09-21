module Browser.Cookies.Data where

import Prelude

import Data.Array (catMaybes, intercalate)
import Data.JSDate (JSDate, toUTCString)
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeEncodeURIComponent)

newtype Cookie = Cookie {
    key :: String
  , value :: String
}

newtype SetCookie = SetCookie {
    cookie :: Cookie
  , opts :: Maybe CookieOpts
}

data SameSite = Lax | Strict

newtype CookieOpts = CookieOpts {
    maxAge :: Maybe Number
  , expires :: Maybe JSDate
  , httpsOnly :: Boolean
  , samesite :: Maybe SameSite
  , domain :: Maybe String
  , path :: Maybe String
}

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
    encode (CookieOpts { maxAge, expires, httpsOnly, samesite, domain, path}) = intercalate "; " $ catMaybes optMaybies
        where 
            optMaybies = [
                (\n -> "max-age=" <> encode n) <$> maxAge,
                (\d -> "expires=" <> encode d) <$> expires,
                if (httpsOnly) then Just "secure" else Nothing,
                (\ss -> "samesite=" <> encode ss) <$> samesite,
                (\str -> "domain=" <> str) <$> domain,
                (\str -> "path=" <> str) <$> path
            ]
  
-- TODO this one could throw an exception, not sure what do to in that case yet.
instance cookieEncoder :: CookieEncoder Cookie where 
    encode (Cookie {key, value}) = key <> "=" <> unsafeEncodeURIComponent value

instance setCookieEncoder :: CookieEncoder SetCookie where 
    encode (SetCookie {cookie, opts}) = intercalate "; " $ catMaybes [
        Just (encode cookie)
      , encode <$> opts
    ]