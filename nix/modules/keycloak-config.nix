{ config, lib, pkgs, ... }:

with lib;

let
  cfg = filterAttrs (_: v: v.enable) config.services.keycloak.realms;

  jsonFile = n: v:
    let
      json = removeAttrs v [ "config" "import" "keycloak" "enable" ]
        // v.config;
    in pkgs.writeText "keycloak-config-${n}.json" (builtins.toJSON json);
in {
  options.services.keycloak.realms = mkOption {
    default = { };
    description = "Keycloak realm configuration";
    type = types.attrsOf (types.submodule ({ config, name, ... }: {
      options = {
        enable = mkEnableOption "Enable keycloak realm configuration";

        id = mkOption {
          type = types.str;
          default = name;
        };

        realm = mkOption {
          type = types.str;
          default = name;
        };

        displayName = mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        config = mkOption {
          type = types.attrs;
          default = { };
          description =
            "Extra configuration. This is joined with the other options in the generated json.";
        };

        import = {
          var-substitution = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Use variable substitution like

              Date:                  $(date:yyyy-MM-dd)
              Environment Variable:  $(env:USERNAME)
              File Content:          $(file:UTF-8:src/test/resources/document.properties)

              See https://github.com/adorsys/keycloak-config-cli#variable-substitution for more info.
            '';
          };
        };
      };
    }));
  };

  config = mkIf (cfg != { }) {
    systemd.services."keycloak-realm-setup" = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "keycloak.service" ];
      script = builtins.concatStringsSep "\n\n" (mapAttrsToList (n: v: ''
        ${pkgs.keycloak-config-cli}/bin/keycloak-config-cli \
          --keycloak.url="https://${config.services.keycloak.settings.hostname}${config.services.keycloak.settings.http-relative-path}" \
          --keycloak.user=admin \
          --keycloak.password="$(cat /run/secrets/keycloak-password)" \
          --import.path=${jsonFile n v} \
          --import.var-substitution=${
            builtins.toString v.import.var-substitution
          } \
          --import.state=false
      '') cfg);
      serviceConfig = {
        User = "keycloak";
        Type = "oneshot";
      };
    };
  };
}
