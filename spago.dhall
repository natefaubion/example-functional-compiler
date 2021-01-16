{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "dodo-printer"
  , "effect"
  , "free"
  , "lists"
  , "maybe"
  , "node-fs-aff"
  , "node-process"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "run"
  , "string-parsers"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
