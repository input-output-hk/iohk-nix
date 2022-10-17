# Produce a set with two pkgs.releaseTools.aggregate jobs from a `ciJobs` set:
# one for required jobs (`required`) and one for all other jobs (`nonrequired`).
{
  lib,
  releaseTools,
  # all the ci jobs:
  ciJobs,
  # path prefix of jobs that should not be required for CI to pass (end up in `nonrequired`):
  nonRequiredPaths ? map lib.hasPrefix [],
}: let
  collectConstituents = excludeTest:
    lib.collect lib.isDerivation
    (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
      (path: value: let
        stringPath = lib.concatStringsSep "." path;
      in
        if lib.isAttrs value && excludeTest stringPath
        then {}
        else value)
      ciJobs);
  nonRequiredTest = stringPath: lib.any (p: p stringPath) nonRequiredPaths;
in {
  required = releaseTools.aggregate {
    name = "required";
    meta.description = "All jobs required to pass CI";
    constituents = collectConstituents nonRequiredTest;
  };
  nonrequired = releaseTools.aggregate {
    name = "nonrequired";
    meta.description = "Nonrequired jobs that should not affect CI status";
    constituents = collectConstituents (p: !nonRequiredTest p);
  };
}
