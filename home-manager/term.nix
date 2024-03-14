{ lib, pkgs, ... }: {
  home = {
    packages = with pkgs; [
      # neovide
      zsh-nix-shell # FIXME what does this do
      zsh-autopair # FIXME not currently sourced
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
      ls = "eza -la";
      bonsai = "cbonsai -i -l --time=2";
    };
  };

  programs = {

    nushell = { # just to try out
      enable = true;
    };

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
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
      initExtra = builtins.readFile ./dotfiles/zsh-extra.zsh;
      localVariables = {
        ## variables in .zshrc
      };
    };

    fish = {
      enable = true;
      functions = {

      };
      interactiveShellInit = ''
        fish_vi_key_bindings --no-erase insert
        set fish_cursor_default block
        set fish_cursor_insert line
        set fish_cursor_replace_one underscore
      '';
      # plugins = with pkgs.fishPlugins; [ ## FIXME it's not supposed to be formatted this way
      #   pisces
      #   fzf
      #   autopair
      #   colored-man-pages
      #   sponge
      # ];
    };

    eza = {
      enable = true;
      git = true;
      icons = false;
    };

    starship = {
      ## palette and style are in themes dir
      enable = true;
      enableZshIntegration = true;
      settings = {
        add_newline = false;

        format = lib.concatStrings [
          "$shell"
          "$directory"
          "$nix_shell"
          "$git_branch"
          "$git_state"
          "$git_status"
          "$git_metrics"
          "$cmd_duration"
          "$sudo"
          "$line_break"
          "$character"
        ];

        directory.read_only =
          "  "; # # formatting looks wrong but this is a lock icon
        character = {
          success_symbol = "[->](green)";
          error_symbol = "[X->](red)";
          vimcmd_symbol = "[<](green)";
        };
        shell = { disabled = false; };
        git_branch = {
          format = "[$branch]($style)";
          symbol = " ";
        };
        git_status = {
          format =
            "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style)";
          conflicted = "​";
          untracked = "​";
          modified = "​";
          staged = "​";
          renamed = "​";
          deleted = "​";
          stashed = "≡";
        };
        git_state.format =
          "([$state( $progress_current/$progress_total)]($style)) ";
        git_metrics = {
          disabled = false;
          format = "([+$added]($added_style) )([-$deleted]($deleted_style) )";
        };
        cmd_duration.format = "[$duration]($style) ";
        fill.symbol = "·";
        time = {
          disabled = false;
          use_12hr = true;
          format = " took [$time]($style) ";
        };
        nix_shell = {
          disabled = false;
          format = "[$symbol $state $symbol  ]($style)";
          symbol = "";
          impure_msg = "[ impure nix shell](blue)";
          pure_msg = "[ pure nix shell](green)";
          unknown_msg = "[ nix shell](yellow)";
          heuristic = true;
        };
        sudo = {
          disabled = false;
          symbol = "\\(#\\) ";
          format = "[$symbol]($style)";
        };
      };
    };

    wezterm = {
      enable = true;
      enableZshIntegration = true;
      extraConfig = builtins.readFile ./dotfiles/wezterm.lua;
    };

    alacritty = {
      enable = true;
      settings = {
        env = {
          TERM = "alacritty";
          # prevents weird resizing on different screen resolutions (in x11):
          WINIT_X11_SCALE_FACTOR = "1.0";
        };
        window = {
          dynamic_padding = true;
          dynamic_title = true;
          class = {
            instance = "Alacritty";
            general = "Alacritty";
          };
          decorations = "none";
        };
        colors.draw_bold_text_with_bright_colors = false;
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
          # mouse.hide_when_typing = true;
        };
      };
    };

    tmux = {
      enable = true;
      aggressiveResize = true;
      baseIndex = 1;
      disableConfirmationPrompt = true;
      keyMode = "vi";
      mouse = true;
      newSession = true;
      prefix = "C-Space";
      terminal = "tmux-256color";
      # shortcut = "";

      plugins = with pkgs.tmuxPlugins;
        [
          sensible

        ];
      tmuxinator.enable = false;
      tmuxp.enable = false;

      extraConfig = ''
        # personal keybind changes:
        bind-key b split-window -v -c "#{pane_current_path}"
        bind-key v split-window -h -c "#{pane_current_path}"
        unbind-key n
        bind n new-window
        unbind-key d
        bind d kill-pane
        bind Space copy-mode
        unbind p
        bind p paste-buffer
        bind-key -T copy-mode-vi 'v' send -X begin-selection
        bind-key -T copy-mode-vi 'y' send -X copy-selection
        bind-key -T copy-mode-vi 'Space' send -X halfpage-down
        bind-key -T copy-mode-vi 'Bspace' send -X halfpage-up
        bind r source-file ~/.tmux.conf
        set -g renumber-windows on
        set -g cursor-style bar
        set -g focus-events on
        set -g automatic-rename on
        set-option -g status-interval 5
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        bind -r H resize-pane -L 5
        bind -r J resize-pane -D 5
        bind -r K resize-pane -U 5
        bind -r L resize-pane -R 5
      '';
    };

    lf = {
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

    neovim = {
      enable = true;
      defaultEditor = false;
      viAlias = true;
      vimAlias = true;

      plugins = with pkgs.vimPlugins; [
        nvim-cmp
        # cmp-nvim-lsp
        which-key-nvim
        telescope-nvim
        nvim-treesitter
        nvim-treesitter-parsers.bash
        nvim-treesitter-parsers.lua
        nvim-treesitter-parsers.markdown
        nvim-treesitter-parsers.nix
        nvim-treesitter-parsers.org
        nvim-treesitter-parsers.racket
        nvim-treesitter-parsers.scheme
        lualine-nvim
        nvim-autopairs
      ];
      extraLuaConfig = builtins.readFile ./dotfiles/nvim.lua;
    };
  };
}
