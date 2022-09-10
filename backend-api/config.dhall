let PersistenceStrategy =
      < NoPersistence
      | MinimalPersistence : Text
      | CrashSafePersistence : Text
      >

let ConnectInfo =
      < InProcessConnectInfo : PersistenceStrategy
      | RemoteConnectInfo : { _1 : Text, _2 : Text, _3 : Text }
      >

in  { info =
      { title = "Ferp-hs"
      , description = "A Functional web api framework."
      , version = "0.1.0.0"
      , environment = "Development"
      , logLevel = "LevelDebug"
      }
    , database =
        --        ConnectInfo.InProcessConnectInfo PersistenceStrategy.NoPersistence
        ConnectInfo.RemoteConnectInfo
          { _1 = "ferp", _2 = "127.0.0.1", _3 = "6543" }
    , port = +3005
    , corsOrigins =
      [ "http://localhost:3003"
      , "http://localhost:3007"
      , "http://localhost:3008"
      ]
    , oidcProviderUri = "http://localhost:3009/realms/ferp-hs/"
    }
