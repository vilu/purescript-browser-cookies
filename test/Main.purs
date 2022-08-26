module Test.Main where

import Prelude

import Browser.Cookies.DataSpec as DataSpec
import Browser.Cookies.Internal (bakeCookies)

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.NonEmpty ((:|))
import Data.String as String
import Data.String.CodeUnits as SCU

import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf1, oneOf)



data CookieSample = CookieSample String String

instance arbitraryCookieSample :: Arbitrary CookieSample where
  arbitrary = CookieSample
              <$> ((SCU.fromCharArray <<< Array.fromFoldable)
                   <$> arrayOf1 keyRFC2625)
              <*> ((SCU.fromCharArray <<< Array.fromFoldable)
                   <$> arrayOf1 valRFC2625)
    where
      keyRFC2625 =
        oneOf $ NEA.fromNonEmpty $ genAlpha :|
        ([genAlpha, genDigitChar]
         <> (pure <$> ['!', '#', '$', '%', '&', '\''
                      , '*', '+', '-', '.', '^', '_'
                      , '`', '|', '~']))
      valRFC2625 =
        oneOf $ NEA.fromNonEmpty $ genAlpha :|
        ([genAlpha, genDigitChar]
         <> (pure <$> ['!', '#', '$', '%', '&', '\'', '(', ')'
                      , '*', '+', '-', '.', '/', ':', '<', '='
                      , '>', '?', '@', '[', ']', '^', '_', '`'
                      , '{', '|', '}', '~']))

main :: Effect Unit
main = runTest do
  suite "Cookies" do
    DataSpec.encodingTests
  test "bakeCookies" do
    quickCheck prop_bakeCookies
  
prop_bakeCookies :: Array CookieSample -> Result
prop_bakeCookies cs =
  let cs' = String.joinWith ";" $ map combine cs
      sampleCount = Array.length cs
      bakeCount = Array.length (bakeCookies cs')
  in if bakeCount == sampleCount
     then Success
     else Failed $
          "Sample Count: " <> show sampleCount <> "\nBake Count: " <> show bakeCount
          <> "\n" <> cs'
  where
    combine (CookieSample k v) = k <> "=" <> v
