{ pkgs, lib }:

with lib;
let
  writeSupervisorConfig = generators.toINI {};

in {
  inherit writeSupervisorConfig;
}
