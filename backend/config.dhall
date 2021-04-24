{ info =
    { title = "Ferp-hs"
    , description = "A Functional web api framework."
    , version = "0.1.0.0"
    , environment = "Development"
    , logLevel = "LevelDebug"
    }
, port = +3007
, oidc =
    { providerUri     = "https://id.dev.boerevlist.nl/realms/ferp/"
    , redirectUri     = "http://localhost:3007/auth/return"
    , clientSecret  = "9996e480-301f-4362-8581-a92d4a6a2626"
    , clientId      = "ferp-hs"
    }
}
