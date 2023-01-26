{ name = "halogen-helix"
, license = "MIT"
, repository = "https://github.com/katsujukou/purescript-halogen-helix"
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
