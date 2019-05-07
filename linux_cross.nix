let
  # Here we try to figure out which qemu to use based on the host platform.
  # This guess can be overriden by passing qemuSuffix
  qemuByHostPlatform = hostPlatform:
    if hostPlatform.isAarch32
    then "arm"
    else abort "Don't know which QEMU to use for hostPlatform ${hostPlatform}. Please provide qemuSuffix";
in
{ stdenv
, lib
, writeScriptBin
, qemu
, qemuSuffix ? (qemuByHostPlatform hostPlatform)
, iserv-proxy
, remote-iserv
, gmp
, extra-test-libs ? []
, buildPlatform
, hostPlatform
, ...
}:
let
  isLinuxCross = buildPlatform != hostPlatform && hostPlatform.isLinux;
  qemuIservWrapper = writeScriptBin "iserv-wrapper" ''
    #!${stdenv.shell}
    set -euo pipefail
    # Unset configure flags as configure should have run already
    unset configureFlags
    PORT=$((5000 + $RANDOM % 5000))
    (>&2 echo "---> Starting remote-iserv on port $PORT")
    ${qemu}/bin/qemu-${qemuSuffix} ${remote-iserv}/bin/remote-iserve tmp $PORT &
    (>&2 echo "---| remote-iserv should have started on $PORT")
    RISERV_PID="$!"
    ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
    (>&2 echo "---> killing remote-iserve...")
    kill $RISERV_PID
    '';
  setupBuildFlags = map (opt: "--ghc-option=" + opt) (lib.optionals isLinuxCross
    [ "--fexternal-interpreter"
      "--pgmi" "${qemuIservWrapper}/bin/iserv-wrapper"
      "-L${gmp}/lib"
    ]);
  qemuTestWrapper = writeScriptBin "test-wrapper" ''
    #!${stdenv.shell}
    set -euo pipefail
    ${qemu}/bin/qemu-${qemuSuffix} $@*
    '';
  setupTestFlags = lib.optionals isLinuxCross [ "--test-wrapper ${qemuTestWrapper}/bin/test-wrapper" ];
  preCheck = lib.optionalString isLinuxCross ''
    echo "================================================================="
    echo "RUNNING TESTS for $name via qemu-${qemuSuffix}"
    echo "================================================================="
    echo "Copying extra test libraries"
    for p in ${lib.concatStringsSep " " extra-test-libs}; do
      find "$p" -iname '*.so*' -exec cp {} . \;
    done
  '';
  postCheck = lib.optionalString isLinuxCross ''
    echo "================================================================="
    echo "END RUNNING TESTS"
    echo "================================================================="
  '';
in { inherit preCheck postCheck setupBuildFlags setupTestFlags; }

