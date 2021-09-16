lib: with lib; rec {

  systemdCompatModule.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      assertions = [];
      users = mkOption {};
    };

  prefixNamesWith = p: mapAttrs' (n: v: nameValuePair "${p}${n}" v);

  collectExes = p: listToAttrs (concatMap (n:
    let s = lib.splitString ":" n;
    in if (lib.length s == 3)
      then [ (nameValuePair (lib.elemAt s 2) p.${n}) ]
      # exclude cross-compiled exes:
      else []
  ) (attrNames p));

  evalService = { pkgs, serviceName, modules, customConfigs }:
    let mkConfig = c: if (isAttrs c)
    then (
      if c ? nixosModules.${serviceName}
      then c.nixosModules.${serviceName}
      else if c ? nixosModule
      then mkConfig c.nixosModule
      else if c ? services.${serviceName} then {
        services.${serviceName} = c.services.${serviceName};
      } else {options, ...}: {
        services.${serviceName} =
          let validOptions = attrNames options.services.${serviceName};
          in filterAttrs (n: _: elem n validOptions) c;
      })
    else c;

    in (lib.modules.evalModules {
      prefix = [];
      modules = modules ++ [
        { services.${serviceName}.enable = true; }
        systemdCompatModule
      ] ++ (map mkConfig customConfigs);
      args = { inherit pkgs; };
      check = false;
    }).config.services.${serviceName};
}
