-- | This module provides the API for getting, setting and removing cookies
module Browser.Cookie (getCookies, getCookie, setCookie, removeCookie) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Browser.Cookies.Data (Cookie(..), SetCookie(..), CookieOpts(..), encode)
import Data.JSDate (fromTime)
import Browser.Cookies.Internal (bakeCookies, findCookie)

foreign import _getCookies :: Effect String

foreign import _setCookie :: String -> Effect Unit

-- | Get all cookies
getCookies :: Effect (Array Cookie)
getCookies = bakeCookies <$> _getCookies

-- | Get cookie by key
getCookie :: String -> Effect (Maybe Cookie)
getCookie k = findCookie k <$> getCookies

-- | Set cookie
setCookie :: SetCookie -> Effect Unit
setCookie sc = _setCookie $ encode sc

-- | Remove cookie
removeCookie :: String -> Effect Unit
removeCookie key = _setCookie $ encode $ SetCookie { cookie, opts: opts $ fromTime 0.0 }
    where
      cookie = Cookie { key, value: "delete" }

      opts d = Just $ CookieOpts {
          maxAge: Nothing
        , expires: Just $ d
        , secure: false
        , httpOnly: false
        , samesite: Nothing
        , domain: Nothing
        , path: Nothing
      }