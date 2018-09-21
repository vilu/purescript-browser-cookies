-- | A module providing pure operations on Strings and Cookies. These methods live in a separate module for testability
module Browser.Cookies.Internal (bakeCookies, findCookie) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Array (catMaybes)
import Data.Foldable (class Foldable, find)
import Browser.Cookies.Data (Cookie(..))

splitOnSemicolon :: String -> Array String 
splitOnSemicolon str = trim <$> split (Pattern ";") str

splitOnEquals :: String -> Array String
splitOnEquals str = trim <$> split (Pattern "=") str

bakeCookies :: String -> Array Cookie
bakeCookies str = catMaybes $ arrToCookie <$> splitOnEquals <$> splitOnSemicolon str
                    where
                        arrToCookie :: Array String -> Maybe Cookie
                        arrToCookie [key,value] = Just $ Cookie { key, value }
                        arrToCookie _ = Nothing

findCookie :: forall f. Foldable f => String -> f Cookie -> Maybe Cookie
findCookie k cookies = find (matchCookie) cookies
                    where                      
                        matchCookie (Cookie {key, value})| k == key = true
                        matchCookie _ = false