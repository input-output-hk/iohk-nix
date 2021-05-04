lib: with lib; rec {

  systemdCompatModule.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      assertions = [];
      users = mkOption {};
    };

  prefixNamesWith = p: mapAttrs' (n: v: nameValuePair "${p}${n}" v);

  collectExes = p: listToAttrs (concatMap (n:
    let m = builtins.match ".+:exe:(.+)" n;
    in if m == null then [] else [ (nameValuePair (head m) p.${n}) ]
  ) (attrNames p));

  evalService = { pkgs, serviceName, modules, customConfig }:
    let customConfig' = if (isAttrs customConfig) && !(customConfig ? service) then {
      services.${serviceName} = customConfig;
    } else customConfig;
    in (lib.modules.evalModules {
      prefix = [];
      modules = modules ++ [
        { services.${serviceName}.enable = true; }
        customConfig'
        systemdCompatModule
      ];
      args = { inherit pkgs; };
      check = false;
    }).config.services.${serviceName};
}
