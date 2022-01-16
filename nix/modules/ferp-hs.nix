{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.ferp-hs;
  pg = config.services.postgresql;

  infoConfigNix = pkg: {
    inherit (cfg) title description environment logLevel;
    inherit (pkg) version;
  };

  frontendUrl = let http = if cfg.useSSL then "https" else "http";
  in "${http}://${cfg.hostName}";

  frontendUrlWs = let ws = if cfg.useSSL then "wss" else "ws";
  in "${ws}://${cfg.hostName}";

  var = name: {
    type = "variable";
    inherit name;
  };

  toDhall' = { }@args:
    v:
    with builtins;
    let concatItems = lib.strings.concatStringsSep ", ";
    in if isAttrs v then
      if v ? type && v.type == "variable" && v ? name then
        v.name
      else
        "{ ${
          concatItems (lib.attrsets.mapAttrsToList
            (key: value: "${key} = ${toDhall' args value}") v)
        } }"
    else if isList v then
      "[ ${concatItems (map (toDhall' args) v)} ]"
    else if isInt v then
      "${if v < 0 then "" else "+"}${toString v}"
    else if isBool v then
      (if v then "True" else "False")
    else if isFunction v then
      abort "generators.toDhall': cannot convert a function to Dhall"
    else if isNull v then
      abort "generators.toDhall': cannot convert a null to Dhall"
    else
      builtins.toJSON v;

  backendConfig = pkgs.writeText "backend.dhall" ''
    let clientSecret = ${cfg.oidc.clientSecretFile} as Text
    in ${toDhall' { } backendConfigNix}
  '';

  backendConfigNix = {
    info = infoConfigNix cfg.backend.package;
    port = cfg.backend.httpPort;
    oidc = {
      inherit (cfg.oidc) providerUri clientId;
      clientSecret = var "clientSecret";
      redirectUri = "${frontendUrl}/auth/return";
    };
    staticDirectory = "${cfg.frontendPackage}";
    frontend = {
      websocketUrl = "${frontendUrlWs}/subscriber";
      mathjaxUrl = "/static/vendor/mathjax/tex-svg.js";
      mathjaxConfigUrl = "/static/vendor/mathjax/mathjax-config.js";
      aceUrl = "/static/vendor/ace/ace.js";
      firaUrl = "/static/vendor/fira/fira.css";
      truckParamUrl = "/static/truck-param/bootstrap.js";
    };
  };

  backendApiConfig = let
    passwordExpr = if cfg.database.passwordFile != null
    && cfg.database.passwordFile != "" then
      "${cfg.database.passwordFile} as Text"
    else
      ''""'';
  in pkgs.writeText "backend-api.dhall" ''
    let password = ${passwordExpr}
    in ${toDhall' { } backendApiConfigNix}
  '';

  backendApiConfigNix = {
    info = infoConfigNix cfg.backend-api.package;
    port = cfg.backend-api.httpPort;
    database = {
      inherit (cfg.database) host port user;
      database = cfg.database.name;
      password = var "password";
    };
    corsOrigins = cfg.backend-api.corsOrigins;
    oidcProviderUri = cfg.oidc.providerUri;
  };
in {
  options.services.ferp-hs = {
    enable = mkEnableOption "Functional reactive web api";

    title = mkOption {
      type = types.str;
      default = "Ferp-hs";
      description = "Title of the html pages.";
    };

    description = mkOption {
      type = types.str;
      default = "A functional web api framework.";
      description = "Description of the ferp-hs app.";
    };

    environment = mkOption {
      type = types.enum [ "Production" "Development" "Test" ];
      default = "Production";
      description = "If the app is running in production mode.";
    };

    logLevel = mkOption {
      type = types.either
        (types.enum [ "LevelDebug" "LevelInfo" "LevelWarn" "LevelError" ])
        types.str;
      default = "LevelWarn";
      description = "The log level of backend-api and backend";
    };

    backend = {
      package = mkOption {
        default = pkgs.ferp-hs.backend;
        type = types.package;
        defaultText = "pkgs.ferp-hs.backend";
        description = "Ferp-hs prerender backend service";
      };

      httpPort = mkOption {
        type = types.int;
        default = 3007;
        description = "Http port of where backend should listen on.";
      };
    };

    frontendPackage = mkOption {
      default = pkgs.ferp-hs.frontend-min;
      type = types.package;
      defaultText = "pkgs.ferp-hs.frontend-min";
      description = "Ferp-hs compiled static assets";
    };

    backend-api = {
      package = mkOption {
        default = pkgs.ferp-hs.backend-api;
        type = types.package;
        defaultText = "pkgs.ferp-hs.backend-api";
        description = "Ferp-hs api service";
      };

      httpPort = mkOption {
        type = types.int;
        default = 3005;
        description = "Http port of where backend-api should listen on.";
      };

      corsOrigins = mkOption {
        type = types.listOf types.str;
        example = [ "https://www.example.com" "https://example.com" ];
        description = "Domain names which are allowed to access this api.";
      };
    };

    user = mkOption {
      type = types.str;
      default = "ferp-hs";
      description = "User account under which backend and backend-api runs.";
    };

    database = {
      name = mkOption {
        type = types.str;
        default = "ferp-hs";
        description = "Database name.";
      };

      host = mkOption {
        type = types.str;
        default = "";
        example = "";
        description = ''
          Database host address.

          On systems that provide unix domain sockets,
          omitting the host parameter will cause libpq to attempt to connect via unix domain sockets.
          The default filesystem path to the socket is constructed from the port number and
          the DEFAULT_PGSOCKET_DIR constant defined in the pg_config_manual.h header file.
          Connecting via unix sockets tends to use the peer authentication method,
          which is very secure and does not require a password.
        '';
      };

      port = mkOption {
        type = types.int;
        default = pg.port;
        description = "Database host port.";
      };

      user = mkOption {
        type = types.str;
        default = "ferp-hs";
        description = "Database user.";
      };

      passwordFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/run/secrets/ferp-hs-database";
        description = ''
          A file containing the password corresponding to
          <option>database.user</option>.
        '';
      };

      createDatabase = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to create a local database automatically.";
      };
    };

    hostName = mkOption {
      type = types.str;
      default = "localhost";
      description = "The public domain name of ferp-hs server.";
    };

    useSSL = mkOption {
      type = types.bool;
      default = true;
      description = "Use the https protocol.";
    };

    oidc = {
      providerUri = mkOption {
        type = types.str;
        example = "https://id.example.com/realms/ferp-hs/";
        default = "${config.services.keycloak.frontendUrl}/realms/ferp-hs/";
        description = "URL of the openid-connect provider.";
      };

      clientId = mkOption {
        type = types.str;
        default = "ferp-hs-backend";
        description =
          "ID of the client using which the backend should make requests to the provider URI.";
      };

      clientSecretFile = mkOption {
        type = types.str;
        example = "/run/secrets/ferp-hs-oidc";
        description =
          "File containing the client secret as obtained while creating a client at the oidc provider";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = cfg.database.createDatabase -> cfg.database.user == cfg.user;
      message =
        "services.ferp-hs.database.user must match services.ferp-hs.user if the database is to be automatically provisioned";
    }];

    services.postgresql = optionalAttrs cfg.database.createDatabase {
      enable = mkDefault true;

      ensureDatabases = [ cfg.database.name ];
      ensureUsers = [{
        name = cfg.database.user;
        ensurePermissions = {
          "DATABASE \"${cfg.database.name}\"" = "ALL PRIVILEGES";
        };
      }];
    };

    systemd.services.ferp-hs-backend-api = {
      description = "ferp-hs backend api service";
      after = [ "network.target" "postgresql.service" "keycloak.service" ];
      wants = [ "postgresql.service" "keycloak.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = "ferp-hs";
        ExecStart =
          "${cfg.backend-api.package}/bin/backend-api ${backendApiConfig}";
        Restart = "always";
      };
    };

    systemd.services.ferp-hs-backend = {
      description = "ferp-hs backend service";
      after = [ "network.target" "keycloak.service" ];
      wants = [ "keycloak.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = "ferp-hs";
        ExecStart = "${cfg.backend.package}/bin/backend ${backendConfig}";
        Restart = "always";
      };
    };

    users.users = mkIf (cfg.user == "ferp-hs") {
      ferp-hs = {
        description = "Ferp-hs api Service";
        useDefaultShell = true;
        group = "ferp-hs";
        isSystemUser = true;
      };
    };

    users.groups.ferp-hs = { };

    services.nginx.enable = mkDefault true;

    services.nginx.virtualHosts.${cfg.hostName} = {
      forceSSL = config.services.ferp-hs.useSSL;
      enableACME = mkDefault config.services.ferp-hs.useSSL;
      locations."/" = {
        proxyPass = "http://localhost:${
            toString config.services.ferp-hs.backend.httpPort
          }/";
      };
      locations."/api/" = {
        priority = 1;
        proxyPass = "http://localhost:${
            toString config.services.ferp-hs.backend-api.httpPort
          }/";
      };
      locations."/subscriber" = {
        priority = 1;
        proxyPass = "http://localhost:${
            toString config.services.ferp-hs.backend-api.httpPort
          }";
        proxyWebsockets = true;
      };
    };
  };
}
