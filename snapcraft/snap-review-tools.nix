{ stdenv
, lib
, python3Packages
, fetchgit
, squashfsTools
}:

with python3Packages;

buildPythonApplication rec {
  pname = "snap-review-tools";
  version = "3.3";
  src = fetchgit {
    url = "https://git.launchpad.net/review-tools";
    rev = "fbd20c5d160b1741814fb7c503c3d113733ffc52";
    sha256 = "00l611lcq1hmsdln41lb55z9l2js06n84m7lasdb72kii0nznrf9";
  };

  propagatedBuildInputs = with python3Packages; [
    requests
    pysha3
    pyxdg
    pyyaml
    click
    tabulate
    pymacaroons
    simplejson
    #progressbar
    requests_toolbelt
    requests-unixsocket
    pyelftools
    debian
    jsonschema
    magic
    squashfsTools
  ];

  doCheck = false;

  meta = with lib; {
    description = "snapcraft";
    homepage = https://pypi.python.org/pypi/snapcraft/;
    license = licenses.mit;
  };
}
