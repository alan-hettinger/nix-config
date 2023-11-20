{ pkgs, config, ... }: {

  nixpkgs.overlays = [
    (final: prev: {
      catppuccin-gtk = prev.catppuccin-gtk.override {
        accents = [ "rosewater" ];
        size = "compact"; # standard|compact
        tweaks = [ "rimless" ];
        variant = "macchiato";
      };
    })
  ];

  gtk = {
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-cursor-blink = false;
      gtk-enable-primary-paste = false;
      gtk-decoration-layout = "menu:";
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-cursor-blink = false;
      gtk-enable-primary-paste = false;
      gtk-decoration-layout = "menu:";
    };
  };

  qt = {
    enable = true;
    # style.name = "adwaita-dark";
    platformTheme = "gtk3";
  };

  programs = {

    tmux.plugins = [{
      plugin = pkgs.tmuxPlugins.catppuccin;
      extraConfig = ''
        set -g @catppuccin_flavour 'macchiato'
        set -g @catppuccin_window_tabs_enabled on
      '';
    }];

    neovim.plugins = with pkgs.vimPlugins; [{
      plugin = catppuccin-nvim;
      config = "colorscheme catppuccin-macchiato";
    }];

    starship.settings = {
      palette = "catppuccin_macchiato";
      palettes.catppuccin_macchiato = {
        rosewater = "#f4dbd6";
        flamingo = "#f0c6c6";
        pink = "#f5bde6";
        mauve = "#c6a0f6";
        red = "#ed8796";
        maroon = "#ee99a0";
        peach = "#f5a97f";
        yellow = "#eed49f";
        green = "#a6da95";
        teal = "#8bd5ca";
        sky = "#91d7e3";
        sapphire = "#7dc4e4";
        blue = "#8aadf4";
        lavender = "#b7bdf8";
        text = "#cad3f5";
        subtext1 = "#b8c0e0";
        subtext0 = "#a5adcb";
        overlay2 = "#939ab7";
        overlay1 = "#8087a2";
        overlay0 = "#6e738d";
        surface2 = "#5b6078";
        surface1 = "#494d64";
        surface0 = "#363a4f";
        base = "#24273a";
        mantle = "#1e2030";
        crust = "#181926";
      };
      directory.style = "blue";
      git_branch.style = "yellow";
      git_status.style = "green";
      git_state.style = "blue";
      git_metrics = {
        added_style = "sky";
        deleted_style = "peach";
      };
      cmd_duration.style = "lavender";
      fill.style = "subtext0";
      time.style = "rosewater";
      nix_shell.style = "lavender";
      sudo.style = "mauve";
    };

    alacritty.settings.colors = {
      primary = {
        background = "#24273A"; # base
        foreground = "#CAD3F5"; # text
        dim_foreground = "#CAD3F5"; # text
        bright_foreground = "#CAD3F5"; # text
      };
      hints = {
        start = {
          foreground = "#24273A"; # base
          background = "#EED49F"; # yellow
        };
        end = {
          foreground = "#24273A"; # base
          background = "#A5ADCB"; # subtext0
        };
      };
      selection = {
        text = "#24273A"; # base
        background = "#F4DBD6"; # rosewater
      };
      normal = {
        black = "#494D64"; # surface1
        red = "#ED8796"; # red
        green = "#A6DA95"; # green
        yellow = "#EED49F"; # yellow
        blue = "#8AADF4"; # blue
        magenta = "#F5BDE6"; # pink
        cyan = "#8BD5CA"; # teal
        white = "#B8C0E0"; # subtext1
      };
      bright = {
        black = "#5B6078"; # surface2
        red = "#ED8796"; # red
        green = "#A6DA95"; # green
        yellow = "#EED49F"; # yellow
        blue = "#8AADF4"; # blue
        magenta = "#F5BDE6"; # pink
        cyan = "#8BD5CA"; # teal
        white = "#A5ADCB"; # subtext0
      };
    };
    # rofi.theme = ../dotfiles/rofi/catppuccin-macchiato.rasi; ## FIXME this is how it's supposed to work
    rofi.theme = "catppuccin-macchiato"; # # FIXME depends on hacky fix below
    waybar.style = ./waybar.css;
  };
  home.file.".config/rofi/catppuccin-macchiato.rasi".source =
    ./rofi/catppuccin-macchiato.rasi;
  home.activation.gtk4-fix = ''
    ln -sf ${pkgs.catppuccin-gtk}/share/themes/Catppuccin-*-dark/gtk-4.0/* ${config.home.homeDirectory}/.config/gtk-4.0/
  '';
}
