{ config, pkgs, ... }: {

  home.packages = with pkgs; [
    tmux
    # python311Packages.libtmux
    neovim
    # neovide
    zsh-nix-shell
    zsh-autosuggestions
    zsh-autocomplete
    zsh-autopair
    zsh-syntax-highlighting
    starship
    oh-my-zsh
    exa
    most
    cava
    bat
    ranger
    # python311Packages.pygments
    fzf
    w3m
    bottom
    neofetch
    lm_sensors
    nvtop
    unzip
  ];

  # programs.zsh = {
  #   enable = true;
  #   enableAutosuggestions = true;
  #   enableCompletion = true;
  #   enableSyntaxHighlighting = true;
  #   enableVteIntegration = true;
  # };
  #

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "alacritty";
      window = {
        dynamic_padding = true;
        dynamic_title = true;
        class = {
          instance = "Alacritty";
          general = "Alacritty";
        };
        decorations = "none";
      };
      font = {
        normal = {
          family = "mononoki";
          style = "Regular";
        };
        bold = {
          family = "mononoki";
          style = "Bold";
        };
        italic = {
          family = "mononoki";
          style = "Italic";
        };
        size = 16;
        builtin_box_drawing = true;
      };
      draw_bold_text_with_bright_colors = false;
      colors = {
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
      cursor = {
        blink_interval = 0;
        blink_timeout = 5;
        unfocused_hollow = true;
        style = {
          shape = "Beam";
          blinking = "Off";
        };
        vi_mode_style = {
          shape = "Beam";
          blinking = "Off";
        };
        mouse.hide_when_typing = true;
      };
    };
  };

  programs.lf = {
    ## https://nix-community.github.io/home-manager/options.html#opt-programs.lf.enable
    enable = false;
    settings = { };
    keybindings = { };
    cmdKeybindings = { };
    commands = { };
    extraConfig = " ";
  };

}
