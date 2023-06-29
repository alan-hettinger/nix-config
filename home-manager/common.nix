{ config, lib, pkgs, ... }: {

  nixpkgs = {
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages =
        [ # # TODO this is a temporary fix due to above error, test after update if still present
          "openssl-1.1.1u" # # TODO find who needs this
        ];
    };
  };

  services = {
    gnome-keyring = { enable = true; };
    home-manager = {
      autoUpgrade = {
        enable = true;
        frequency = "weekly";
      };
    };
  };

  gtk.enable = true;

  xdg = {
    userDirs = {
      enable = true;
      createDirectories = true;
    };
    dataFile = {
      scripts = {
        enable = true;
        executable = true;
        recursive = true;
        source = ./scripts;
      };
    };
  };

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
      profiles = {
        alan = {
          isDefault = true;
          userChrome = builtins.readFile ./dotfiles/firefox/userChrome.css;
          extraConfig = builtins.readFile ./dotfiles/firefox/user.js;
          search = {
            engines = {
              "Nix Packages" = {
                urls = [{
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "type";
                      value = "packages";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };

              "Nix Options" = {
                urls = [{
                  template = "https://search.nixos.org/options";
                  params = [
                    {
                      name = "type";
                      value = "options";
                    }
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }];
                icon =
                  "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@no" ];
              };

              "NixOS Wiki" = {
                urls = [{
                  template =
                    "https://nixos.wiki/index.php?search={searchTerms}";
                }];
                iconUpdateURL = "https://nixos.wiki/favicon.png";
                updateInterval = 24 * 60 * 60 * 1000; # every day
                definedAliases = [ "@nw" ];
              };

              "Home Manager" = {
                urls = [{
                  template =
                    "https://mipmip.github.io/home-manager-option-search/?query={searchTerms}";
                }];
                iconUpdateURL =
                  "https://mipmip.github.io/home-manager-option-search/images/favicon.png";
                definedAliases = [ "@nh" "@hm" ];
              };

              "Brave" = {
                urls = [{
                  template = "https://search.brave.com/search?q={searchTerms}";
                }];
                iconUpdateURL =
                  "https://cdn.search.brave.com/serp/v2/_app/immutable/assets/favicon-32x32.86083f5b.png";
                definedAliases = [ "@b" ];
              };

              "Wikipedia".metaData.alias = "@w";

              # Hide unneeded search engines
              "Bing".metaData.hidden = true;
              "Google".metaData.hidden = true;
              "Amazon.com".metaData.hidden = true;
              "eBay".metaData.hidden = true;

            };
            force = true;
            order = [
              "Brave"
              "Wikipedia"
              "Nix Packages"
              "Nix Options"
              "NixOS Wiki"
              "Home Manager"
            ];
            default = "Brave";
          };
        };
      };
    };

    home-manager.enable = true;
  };
  home = {
    sessionPath = [
      "$HOME/bin"
      "$HOME/.local/share/scripts"
      "$HOME/.cargo/bin"
      "$HOME/.emacs.d/bin"
    ];

    sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
      TERM = "alacritty";
      GOPATH = "$HOME/go";
    };

    username = "alan";
    homeDirectory = "/home/alan";

    packages = with pkgs; [
      ripgrep
      fd
      fzf

    ];
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
