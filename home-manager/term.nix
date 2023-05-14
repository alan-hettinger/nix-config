{ config, lib, pkgs, ... }: {

  home.packages = with pkgs; [
    tmux
    # python311Packages.libtmux
    neovim
    # neovide
    zsh-nix-shell
    zsh-autopair
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
    cbonsai
  ];

  home.shellAliases = {
    ## applies across all shells
    cat = "bat";
    ls = "exa -la";
    catppuccin = "inkcat latte,frappe,macchiato,mocha";
    catppucciny = "inkcat";
    bonsai = "cbonsai -i -l --time=2";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;
    history = { ignoreDups = true; };
    shellAliases = {
      ## note home.shellAliases above
      ## this is for zsh-specific aliases

    };
    sessionVariables = {
      ## envvars
    };
    ## .zshrc extras:
    initExtra = ''
      # Function to cd and ls in one command
      cl () {
          if [ "$#" -eq 0 ]; then
              "cd" || return
          else
              "cd" "$1" || return
          fi
          ls -A --color=auto
      }

      # some functions copied from the fzf github page https://github.com/junegunn/fzf/wiki/Examples
      # fh - repeat history
      fh() {
        print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf --height 50% --reverse --border +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
      }
    '';
    localVariables = {
      ## variables in .zshrc
    };
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      palette = "catppuccin_macchiato";
      add_newline = false;
      format = lib.concatStrings [
        "$directory"
        "$nix_shell"
        "$git_branch"
        "$git_state"
        "$git_status"
        "$git_metrics"
        "$fill"
        "$cmd_duration"
        "$jobs"
        "$sudo"
        "$time"
        "$line_break"
        "$character"
      ];
      directory = {
        style = "blue";
        read_only = "  "; # # formatting looks wrong but this is a lock icon
      };
      character = {
        success_symbol = "[->](green)";
        error_symbol = "[X->](red)";
        vimcmd_symbol = "[<](green)";
      };
      git_branch = {
        format = "[$branch]($style)";
        style = "yellow";
        symbol = " ";
      };
      git_status = {
        format =
          "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style)";
        style = "green";
        conflicted = "​";
        untracked = "​";
        modified = "​";
        staged = "​";
        renamed = "​";
        deleted = "​";
        stashed = "≡";
      };
      git_state = {
        format = "([$state( $progress_current/$progress_total)]($style)) ";
        style = "blue";
      };
      git_metrics = {
        disabled = false;
        format = "([+$added]($added_style) )([-$deleted]($deleted_style) )";
        added_style = "sky";
        deleted_style = "peach";
      };
      cmd_duration = {
        format = "[$duration]($style) ";
        style = "lavender";
      };
      fill = {
        symbol = "·";
        style = "subtext0";
      };
      time = {
        disabled = false;
        use_12hr = true;
        style = "rosewater";
        format = " [$time]($style) ";
      };
      nix_shell = {
        disabled = false;
        format = "[$symbol $state $symbol  ]($style)";
        style = "lavender";
        symbol = "";
        impure_msg = "[ impure nix shell](blue)";
        pure_msg = "[ pure nix shell](green)";
        unknown_msg = "[ nix shell](yellow)";
        heuristic = true;
      };
      sudo = {
        disabled = false;
        symbol = "\\(#\\) ";
        style = "mauve";
        format = "[$symbol]($style)";
      };

      palettes = {
        catppuccin_macchiato = {
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
      };
    };
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
