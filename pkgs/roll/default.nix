{
  config,
  lib,
  pkgs,
  stdenv,
  ...
}:
stdenv.mkDerivation rec {
  name = "roll";
  version = "0.1";

  src = ./.;

  nativeBuildInputs = [];
  buildInputs = [];

  buildPhase = ''
    ${pkgs.racket}/bin/raco exe -o roll ./main.rkt
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp roll $out/bin
  '';
}
