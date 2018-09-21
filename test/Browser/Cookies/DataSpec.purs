module Browser.Cookies.DataSpec where

import Prelude

import Browser.Cookies.Data (Cookie(..), CookieOpts(..), SameSite(..), SetCookie(..), encode)
import Data.JSDate (fromTime)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert

encodingTests :: TestSuite
encodingTests = test "Encoding" do
      Assert.equal "key=value" (encode simpleCookie) 
      Assert.equal "key=http://example.com/query?key1=val1&key2=val2;" (encode urlEncodeCookie) -- Didn't really know what's a good API here. Decided not to handle this, needs to be handled by client for now.
      Assert.equal "max-age=1.0; expires=Thu, 01 Jan 1970 00:00:00 GMT; secure; httponly; samesite=strict; domain=domain; path=path" (encode $ allOpts) 
      Assert.equal "" (encode $ noOpts) 
      Assert.equal "max-age=1.0" (encode $ onlyMaxAgeOpt) 
      Assert.equal "expires=Thu, 01 Jan 1970 00:00:00 GMT" (encode $ onlyExpiresOpt) 
      Assert.equal "secure" (encode $ secure) 
      Assert.equal "httponly" (encode $ httpOnly) 
      Assert.equal "samesite=strict" (encode $ onlySamesiteOpt) 
      Assert.equal "domain=domain" (encode $ onlyDomainOpt) 
      Assert.equal "path=path" (encode $ onlyPathOpt) 
      Assert.equal "key=value; max-age=1.0; expires=Thu, 01 Jan 1970 00:00:00 GMT; secure; httponly; samesite=strict; domain=domain; path=path" (encode $ setCookie) 
        where
          simpleCookie = Cookie { key: "key", value: "value" }
          urlEncodeCookie = Cookie { key: "key", value: "http://example.com/query?key1=val1&key2=val2;" }
          allOpts = CookieOpts { maxAge: Just 1.0, expires: Just $ fromTime 0.0, secure: true, httpOnly: true, samesite: Just Strict, domain: Just "domain", path: Just "path"}
          noOpts = CookieOpts { maxAge: Nothing, expires: Nothing, secure: false, httpOnly: false, samesite: Nothing, domain: Nothing, path: Nothing}
          onlyMaxAgeOpt = CookieOpts (unwrap noOpts) { maxAge = Just 1.0 }
          onlyExpiresOpt = CookieOpts (unwrap noOpts) { expires = Just $ fromTime 0.0 }
          secure = CookieOpts (unwrap noOpts) { secure = true }
          httpOnly = CookieOpts (unwrap noOpts) { httpOnly = true }
          onlySamesiteOpt = CookieOpts (unwrap noOpts) { samesite =  Just Strict }
          onlyDomainOpt = CookieOpts (unwrap noOpts) { domain = Just "domain"}
          onlyPathOpt = CookieOpts (unwrap noOpts) { path = Just "path" }
          setCookie = SetCookie { cookie: simpleCookie, opts: Just allOpts}