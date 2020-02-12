{ python3Packages }:

python3Packages.buildPythonApplication rec {
  pname = "hydra-eval-errors";
  version = "0.0.0";

  src = ./.;
  propagatedBuildInputs = [ python3Packages.requests ];
}
