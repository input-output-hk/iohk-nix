{ stdenv
, lib
, python3Packages
}:

with python3Packages;

buildPythonApplication rec {
  pname = "snapcraft";
  version = "3.3";
  src = fetchPypi {
    inherit pname version;
    sha256 = "1z2xfqf4jzskzx36q0z6zbjy88f15gnggpm4mss9k80dak75f7ba";
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
    progressbar
    requests_toolbelt
    requests-unixsocket
    pyelftools
    debian
    jsonschema
  ];

  doCheck = false;

  meta = with lib; {
    description = "snapcraft";
    homepage = https://pypi.python.org/pypi/snapcraft/;
    license = licenses.mit;
  };
}
