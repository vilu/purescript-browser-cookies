module Browser.Cookie (getCookies, getCookie, setCookie, setCookie2) where


import Data.Maybe
import Data.String.Common
import Data.String.Pattern
import Debug.Trace
import Effect
import Prelude

import Browser.Cookies.Data (Cookie(..), SetCookie(..), CookieOpts(..))
import Data.Foldable (findMap)
import Data.Functor (map, (<$>))
import Data.JSDate (JSDate)
import Browser.Cookies.Data (encode)

foreign import _getCookies :: Effect String

foreign import _setCookie :: String -> Effect Unit

foreign import _setCookie2 :: String -> Unit

getCookies :: Effect (Array String)
getCookies = do
  cs <- _getCookies
  pure $ trim <$> split (Pattern ";") cs

getCookie :: String -> Effect (Maybe Cookie)
getCookie key = findCookie <$> getCookies
  where 
    findCookie = findMap (matchCookie key) <<< toKeyVal
    toKeyVal = map (split (Pattern "="))
    matchCookie key' [k,v]| key' == k = Just $ Cookie { key, value:v }
    matchCookie k kv = Nothing

-- setCookie :: SetCookie -> Effect Unit
-- setCookie sc = _setCookie $ encode sc

setCookie :: String -> Effect Unit
setCookie sc = _setCookie sc

setCookie2 :: String -> Unit
setCookie2 = _setCookie2