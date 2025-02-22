{ config, pkgs, ... }:
{
  home.username = "anuvyklack";
  home.homeDirectory = "/home/anuvyklack";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Allow unfree packages.
  nixpkgs.config.allowUnfree = true;
  programs.man.generateCaches = true;

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # link all files in `./scripts` to `~/.config/i3/scripts`
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # link recursively
  #   executable = true;  # make all files executable
  # };

  # encode the file content in nix configuration file directly
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # # set cursor size and dpi for 4k monitor
  # xresources.properties = {
  #   "Xcursor.size" = 16;
  #   "Xft.dpi" = 172;
  # };

  home.packages = with pkgs; [
    # # manpages
    # linux-manual man-pages man-pages-posix

    topgrade
    chezmoi
    atuin
    # nushellFull # nushell
    bat
    ripgrep
    fzf
    bfs
    delta
    zoxide
    jq      # A lightweight and flexible command-line JSON processor
    # yq-go   # yaml processer: https://github.com/mikefarah/yq
    eza     # Maitained exa fork
    yazi # file manager
    ueberzugpp

    # # Auto switch language to English when switch to Normal mode in Vim & Emacs
    # xkb-switch
    # g3kb-switch

    # email
    mu
    emacsPackages.mu4e

    # clang-tools
    ccls
    lua-language-server
    neocmakelsp

    bitwarden
    # bcompare
    floorp  # A fork of Firefox built in Japan
    rambox
    # obsidian

    # teamspeak5_client # Flatpack's TeamSpeak 5 client works better

    # cinnamon.warpinator

    # neofetch

    # # archives
    # zip
    # xz
    # unzip
    # p7zip

    # # networking tools
    # mtr      # A network diagnostic tool
    # iperf3
    # dnsutils # `dig` + `nslookup`
    # ldns     # replacement of `dig`, it provide the command `drill`
    # aria2    # A lightweight multi-protocol & multi-source command-line download utility
    # socat    # replacement of openbsd-netcat
    # nmap     # A utility for network discovery and security auditing
    # ipcalc   # it is a calculator for the IPv4/v6 addresses

    # # misc
    # cowsay
    # file
    # which
    # tree
    # gnused
    # gnutar
    # gawk
    # zstd
    # gnupg

    # # productivity
    # hugo # static site generator

    # btop  # replacement of htop/nmon
    # iotop # io monitoring
    # iftop # network monitoring

    # # system call monitoring
    # strace # system call monitoring
    # ltrace # library call monitoring
    # lsof   # list open files

    # # system tools
    # sysstat
    # lm_sensors # for `sensors` command
    # ethtool
    # pciutils # lspci
    # usbutils # lsusb
  ];
}
