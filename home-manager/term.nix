{ config, pkgs, ... }: {

  home.packages = with pkgs; [
    alacritty
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

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
  };

  # programs.zsh = {
  #   enable = true;
  #   enableAutosuggestions = true;
  #   enableCompletion = true;
  #   enableSyntaxHighlighting = true;
  #   enableVteIntegration = true;
  # };
  #
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
