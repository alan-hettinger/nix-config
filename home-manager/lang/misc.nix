{ pkgs, ... }: {

  home.packages = with pkgs; [
    shellcheck
    shfmt
    nodejs
    html-tidy
    nodePackages.prettier
  ];

}
