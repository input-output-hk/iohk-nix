lib: with lib; {

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
}
