{ title = "Ferp-hs"
, description = "A Functional web api framework."
, version = "0.1.0.0"
, database =
    { host = ""
    , port = +5432
    , user = "stefan"
    , password = ""
    , database = "blog"
    }
, environment = "Development"
, port = +3005
, logLevel = "LevelDebug"
, baseUrl = "http://localhost:3005"
, corsOrigins = [ "http://localhost:3003" ]
}
