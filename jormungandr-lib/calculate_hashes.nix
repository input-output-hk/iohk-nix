{ version, sha256, cargoSha256 }@args:
let
  inherit (import ../. { }) jormungandrLib;
  inherit (jormungandrLib) makeJcli;
  common = f: f (makeJcli args);
in {
  src = (makeJcli args).src;
  cargoDeps = (makeJcli args).cargoDeps;
}
