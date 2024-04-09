{pkgs, ...}: {
  home.packages = with pkgs; [
    # rustup
    rustc
    rustfmt
    cargo
    rust-analyzer
    bacon # background code checker
  ];
}
