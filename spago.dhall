{ name = "halogen-helix"
, dependencies =
  [ "effect"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "lazy"
  , "maybe"
  , "prelude"
  , "refs"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
