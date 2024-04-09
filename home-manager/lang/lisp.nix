{pkgs, ...}: {
  home.packages = with pkgs; [
    guile
    racket
    mitscheme
    sbcl

    ## clojure-specific:
    clojure
    clojure-lsp
    clj-kondo # linter
    neil # for adding packages to clojure project from emacs
    jet # transforming between JSON EDN YAML and Transit using Clojure
  ];
}
