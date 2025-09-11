{ lib, pkgs, python3Packages, fetchFromGitHub }:

python3Packages.buildPythonPackage rec {
  pname = "stack-pr";
  version = "0.1.4";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "modular";
    repo = "stack-pr";
    rev = version;
    hash = "sha256-3i356Qdsvx4UJxCBf8mRjfZbwp/w0Jfx5PxjZ+jAcwQ=";
  };

  propagatedBuildInputs = with python3Packages; [
    pdm-backend
  ];

  meta = with lib; {
    description = "Command-line tool for managing pull requests on GitHub";
    homepage = "https://github.com/modular/stack-pr";
    license = licenses.asl20;
  };
}
