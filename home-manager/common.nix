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
  };

}
