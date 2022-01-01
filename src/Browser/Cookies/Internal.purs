-- | A module providing pure operations on Strings and Cookies. These methods live in a separate module for testability
module Browser.Cookies.Internal (bakeCookies, findCookie) where

import Prelude

import Browser.Cookies.Data (Cookie(..))

import Control.Alternative ((<|>))

import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.Common (split, trim)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Foldable (class Foldable, find)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String (skipSpaces)
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT


-- * Parsing
data Pair = Pair String String

instance showPair :: Show Pair where
  show (Pair k v) = "Pair " <> k <> " " <> v

pairs :: Parser String (Array Pair)
pairs = Array.fromFoldable <$> PC.sepBy pair (PS.string ";")

pair :: Parser String Pair
pair = do
  k <- key
  void $ PS.char '='
  v <- val
  pure $ Pair k v

key :: Parser String String
key = SCU.fromCharArray
      <$> Array.some (PT.alphaNum <|> special)
  where
    special = PC.choice $ PS.char
              <$> ['!', '#', '$', '%', '&', '\''
                  ,'*', '+', '-', '.', '^', '_'
                  ,'`', '|', '~']

val :: Parser String String
val = PC.try (quoted rfc2625) <|> rfc2625
  where quoted = PC.between (PS.string "\"") (PS.string "\"")
        rfc2625 = SCU.fromCharArray
                  <$> Array.some (PT.alphaNum <|> special)
        special = PC.choice $ PS.char
                  <$> ['!', '#', '$', '%', '&', '\'', '(', ')', '*'
                      , '+', '-', '.', '/', ':', '<', '=', '>', '?'
                      , '@', '[', ']', '^', '_', '`', '{', '|', '}'
                      , '~']
                  



splitOnSemicolon :: String -> Array String 
splitOnSemicolon str = trim <$> split (Pattern ";") str

splitOnEquals :: String -> Array String
splitOnEquals str = trim <$> split (Pattern "=") str

-- bakeCookies :: String -> Array Cookie
-- bakeCookies str = catMaybes $ arrToCookie <$> splitOnEquals <$> splitOnSemicolon str
--                     where
--                         arrToCookie :: Array String -> Maybe Cookie
--                         arrToCookie [key,value] = Just $ Cookie { key, value }
--                         arrToCookie _ = Nothing

bakeCookies :: String -> Array Cookie
bakeCookies str = case runParser str pairs of
  Left e -> []
  Right cs -> map pairToCookie cs
  where
    pairToCookie (Pair key value) = Cookie {key, value}

findCookie :: forall f. Foldable f => String -> f Cookie -> Maybe Cookie
findCookie k cookies = find (matchCookie) cookies
                    where                      
                        matchCookie (Cookie {key, value})| k == key = true
                        matchCookie _ = false
