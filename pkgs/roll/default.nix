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

  nativeBuildInputs = [pkgs.racket];
  buildInputs = [];

  ## FIXME 2025-01-14: building with --3m implementation of the racket vm rather than the default --cs implementation due to build error "find-exe: can't find Racket executable for variant cs". Likely result of Racket update 8.14 -> 8.15.
  buildPhase = ''
    ${pkgs.racket}/bin/raco exe --3m -o roll ./main.rkt
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp roll $out/bin
  '';
}
