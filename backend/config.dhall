{ info =
  { title = "Ferp-hs"
  , description = "A Functional web api framework."
  , version = "0.1.0.0"
  , environment = "Development"
  , logLevel = "LevelDebug"
  }
, port = +3007
, oidc =
  { providerUri = "https://id.dev.boerevlist.nl/realms/ferp-hs/"
  , redirectUri = "http://localhost:3007/auth/return"
  , clientSecret = "58a6dffb-6f7a-4faf-a2fe-53ea7ba16ca4"
  , clientId = "ferp-hs-backend"
  }
, staticDirectory = "../result-frontend-min"
, frontend.websocketUrl = "ws://localhost:3005/subscriber"
}
