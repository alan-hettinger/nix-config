{ pkgs, ... }:

{
  home.packages = with pkgs; [
    luajitPackages.luaposix
    luajitPackages.luarocks
    # luajitPackages.luarocks-nix
    cmake
    gnumake
    github-desktop

  ];

}
