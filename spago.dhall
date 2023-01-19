{ name = "halogen-helix"
, dependencies =
  [ "aff"
  , "effect"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "lazy"
  , "maybe"
  , "prelude"
  , "refs"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
