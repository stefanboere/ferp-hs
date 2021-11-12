let clientSecret = /run/secrets/oidc-client-secret as Text
in
{ info =
  { title = "Ferp-hs"
  , description = "A Functional web api framework."
  , version = "0.1.0.0"
  , environment = "Development"
  , logLevel = "LevelDebug"
  }
, port = +3007
, oidc =
  { providerUri = "https://id.boerevlist.nl/realms/ferp-hs/"
  , redirectUri = "http://localhost:3007/auth/return"
  , clientSecret = clientSecret
  , clientId = "ferp-hs-backend"
  }
, staticDirectory = "../result-frontend-min"
, frontend =
  { websocketUrl = "ws://localhost:3005/subscriber"
  , mathjaxUrl = "/static/vendor/mathjax/tex-svg.js"
  , mathjaxConfigUrl = "/static/vendor/mathjax/mathjax-config.js"
  , aceUrl = "/static/vendor/ace/ace.js"
  , firaUrl = "/static/vendor/fira/fira.css"
  }
}
