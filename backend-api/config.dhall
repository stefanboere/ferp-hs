{ info =
  { title = "Ferp-hs"
  , description = "A Functional web api framework."
  , version = "0.1.0.0"
  , environment = "Development"
  , logLevel = "LevelDebug"
  }
, database =
  { host = "", port = +5432, user = "stefan", password = "", database = "blog" }
, port = +3005
, corsOrigins =
  [ "http://localhost:3003", "http://localhost:3007", "http://localhost:3008" ]
, oidcProviderUri = "https://id.dev.boerevlist.nl/realms/ferp/"
}
