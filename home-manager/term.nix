{ config, lib, pkgs, ... }: {
  home = {
    packages = with pkgs; [
      tmux
      neovim
      # neovide
      zsh-nix-shell # FIXME what does this do
      zsh-autopair # FIXME not currently sourced
      exa
      most
      cava
      bat
      fzf
      w3m
      bottom
      neofetch
      lm_sensors
      nvtop
      unzip
      cbonsai
    ];
    shellAliases = {
      ## applies across all shells
      cat = "bat";
      ls = "exa -la";
      bonsai = "cbonsai -i -l --time=2";
    };
  };

  programs = {

    zsh = {
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
        lf = "lfcd || lf";
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

        ## cd on exiting lf
        lfcd () {
            tmp="$(mktemp)"
            # `command` is needed in case `lfcd` is aliased to `lf`
            command lf -last-dir-path="$tmp" "$@"
            if [ -f "$tmp" ]; then
                dir="$(cat "$tmp")"
                rm -f "$tmp"
                if [ -d "$dir" ]; then
                    if [ "$dir" != "$(pwd)" ]; then
                        cd "$dir"
                    fi
                fi
            fi
        }
        bindkey -s '^o' 'lfcd\n'  # zsh
      '';
      localVariables = {
        ## variables in .zshrc
      };
    };
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
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
      };
    };
    alacritty = {
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
        draw_bold_text_with_bright_colors = false;
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

    lf = {
      ## https://nix-community.github.io/home-manager/options.html#opt-programs.lf.enable
      enable = true;
      settings = {
        color256 = true;
        drawbox = true;
        hidden = true;
        ignorecase = true;
        number = true;
        preview = true;
        ratios = "2:4:3";
        relativenumber = true;
        scrolloff = 10;
        dirfirst = true;
        # sortby = "";
      };
      keybindings = { };
      cmdKeybindings = { };
      commands = { };
    };
  };
}
