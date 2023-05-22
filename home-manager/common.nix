{ config, lib, pkgs, ... }: {
  programs = {
    git = {
      enable = true;
      delta = { enable = true; };
      userName = "Alan";
      userEmail = "alan.hettinger@proton.me";
    };
    gh = {
      enable = true;
      enableGitCredentialHelper = true;
      settings = { };
    };
    firefox = {
      enable = true;
      profiles = { alan = { isDefault = true; }; };

    };
  };

  home.file = {
    ".mozilla/firefox/alan/chrome/userChrome.css" = {
      source = ./dotfiles/firefox/userChrome.css;
    };
    ".mozilla/firefox/alan/user.js" = {
      source = ./dotfiles/firefox/user.js;
    };
  };

}
