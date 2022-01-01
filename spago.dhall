{ name = "browser-cookies"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "js-date"
  , "maybe"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
