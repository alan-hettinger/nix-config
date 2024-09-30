{pkgs, ...}: {
  home.packages = with pkgs; [
    luajitPackages.luaposix
    luajitPackages.luarocks
    # luajitPackages.luarocks-nix
    cmake
    gnumake
    github-desktop
  ];
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };
  };
}
