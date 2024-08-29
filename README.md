<div align="right">
 
![main](https://github.com/kylegortych/dotfiles/actions/workflows/main.yml/badge.svg) 

</div>

# My Dotfiles
Based on NixOS file system

<br>
    
<div align="center">
     
### :hammer_and_wrench: Tools :

<table>
  <thead>
    <tr>
      <th>Package Config</th>
      <th>Language</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>nix</td>
      <td>
        <img src="https://img.shields.io/badge/nix-white?style=plastic&logo=nixos" 
             title="nix" 
             alt="nix" 
             height="30"/>
      </td>
    </tr>
    <tr>
      <td>wezterm</td>
      <td>
        <img src="https://img.shields.io/badge/Lua-white?style=plastic&logo=lua&logoColor=blue" 
             title="lua" 
             alt="lua" 
             height="30"/>
      </td>
    </tr>
    <tr>
      <td>starship</td>
      <td>
        <img src="https://img.shields.io/badge/Toml-white?style=plastic&logo=toml&logoColor=black" 
             title="toml" 
             alt="toml" 
             height="30"/>
      </td>
    </tr>
    <tr>
      <td>doom emacs</td>
      <td>
        <img src="https://img.shields.io/badge/Emacs%20Lisp-white?style=plastic&logo=gnu-emacs" 
             title="Emacs Lisp" 
             alt="Emacs Lisp" 
             height="30"/>
      </td>
    </tr>
  </tbody>
</table>
    
</div>

<br>

### system config

<details>
<summary>configuration.nix</summary>

```nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
      ./dotfiles/doom.nix
      ./dotfiles/fish.nix
      ./dotfiles/neovim.nix
      ./dotfiles/starship.nix
      ./dotfiles/wezterm.nix
      ./dotfiles/xplr.nix
    ];

  #nix settings
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  hardware.blank.enableAll = true;
  hardware.bluetooth.enable = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 20;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.network-setup.enable = false;

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.variant = "";

  services.displayManager.sddm.enable = true;
  #services.xserver.desktopManager.plasma6.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.defaultSession = "plasma";


  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  networking.hostName = "blank"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  #sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Set your time zone.
  services.automatic-timezoned.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  environment.sessionVariables = {
    EDITOR = "nvim";
  };

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # virt reduce redundant writes
  #fileSystems."/nix/store".options = [ "noatime" ];

  virtualisation = {
    docker.rootless = {
      enable = true;
      setSocketVariable = true;
    };
    #virtualbox.host = {
    #  enable = true;
    #};
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.blank = {
    isNormalUser = true;
    description = "blank";
    extraGroups = [ "networkmanager" "wheel"];
    shell = pkgs.fish;
    packages = with pkgs; [ 
      #packagnames
    ];
  };

  home-manager.users.blank = { pkgs, ... }: {
    home.stateVersion = "23.11";
    nixpkgs.config.allowUnfree = true;

    programs.git = {
      enable = true;
      #credential.helper=blank
      userName = "blank";
      userEmail = "blank";
    };

    home.packages = with pkgs; [
      #gui
      firefox
      #google-chrome
      thunderbird
      gimp
      libreoffice
      obs-studio
      bitwarden
      kdenlive
      mpv
      freecad
      kicad-small
      logisim-evolution
      insomnia
      jetbrains.idea-community
      blender-hip
      zoom-us
      wezterm
      isoimagewriter

      #emacs
      emacs29-gtk3
      emacsPackages.xwidgets-reuse
      pandoc

      # cli
      fish
      starship
      wl-clipboard
      xclip
      calcurse
      xplr
      yazi
      btop
      ripgrep
      figlet
      ffmpeg-full
      neofetch
      ghostscript
      parallel
      file
      xdg-ninja
      screenkey
      # extract archive types
      unrar
      p7zip
      
      # security
      lynis

      #lang
      verilog
      go
      rustup
      gnu-cobol

      #java
      gradle_7
      #gradle

      #python
      pyright
      (python312.withPackages(ps: with ps; [
           python312Packages.pytz
           python312Packages.datetime
           python312Packages.dateutils
           python312Packages.ptpython
           (buildPythonPackage rec {
             pname = "introcs";
             version = "1.3.1";
             src = fetchPypi {
               inherit pname version;
               sha256 = "";
             };
             doCheck = false;
             propagatedBuildInputs = [
               python312Packages.numpy
               python312Packages.pillow
             ];
           })
      ]))

      #lua
      lua
      lua-language-server

      #nix
      #nil

      #nodejs
      nodejs_20
      #(buildNpmPackage rec {
      #  pname = "dynamodb-admin";
      #  version = "4.6.1";
      #  src = fetchFromGitHub {
      #    owner = "aaronshaf";
      #    repo = "dynamodb-admin";
      #    rev = "v4.6.1";
      #    sha256 = "";
      #  };
      #})

      #C & C++
      clang_16
      cmake

      #temp | kde polonium build
      # look for libsForQt6.polonium
      # delete ~/.local/share/kwin/scripts/polonium
      # delete cloned repo build
      gnumake
      zip

      #Haskell
      #ghc

      #kenzie
      awscli2
      aws-sam-cli

      # MS Azure
      #azure-cli

      #inteliji idea dependency
      graphviz
      plantuml

      #latex
      #texlive.combined.scheme-full
      texliveMinimal

      #hyprland
      #hyprpaper
      #bemenu
    ];
  };

  programs.steam.enable = true;

  #programs.kdeconnect.enable = true;

  programs.java = {
    enable = true;
    package = pkgs.jdk;
  };

  programs.fish = {
    enable = true;
  };

  #programs.nix-ld.enable = true;

  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "ShareTechMono" ]; })
  ];

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
    daemon.settings = {
      OnAccessPrevention = true;
      OnAccessExtraScanning = true;
      OnAccessMountPath = [
        "/home/blank/Downloads"
        "/etc/nixos"
        "/srv"
        "/var/lib"
        "/var/log"
      ];
      OnAccessExcludeUname = "clamav";
      VirusEvent = "mv %f /home/blank/quarantine";
      User = "clamav";
    };
  };

  #services.fail2ban = {
  #  enable = true;
  #};

  #networking.wireguard.interfaces = {
  #
  #};

  programs.gnupg.agent = {
    enable = true;
    #pinentryPackage = pkgs.pinentry-qt;
    #pinentryFlavor = "qt";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.systemPackages = with pkgs; [

  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
```

</details>

### Git Actions

<details>
<summary>main.yml</summary>

```yml
name: Nix Config Lint
on:
  push:
    branches:
      - main
    paths:
      - '*.nix'
      - 'dotfiles/**'

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - uses: cachix/install-nix-action@v26
        with:
          nix_path: nixpkgs=channel:nixos-unstable

      - name: Evaluate Nix configurations
        run: |
          nix-instantiate --eval configuration.nix dotfiles/*.nix

      #- name: Set up Python
      #  uses: actions/setup-python@v4
      #  with:
      #    python-version: '3.x'

      #- name: Extract content from Nix files
      #  run: |
      #    python3 - <<EOF
      #    import os
      #    import re

      #    def extract_content(file_path):
      #        with open(file_path, 'r') as f:
      #            content = f.read()
      #        
      #        # Updated regex pattern to capture content correctly
      #        pattern = r'"([^"]+)".text\s*=\s*\'\'\s*([\s\S]*?)\'\'\s*;\s*'
      #        matches = re.findall(pattern, content, re.DOTALL)
      #        
      #        for file_path, file_content in matches:
      #            file_name = os.path.basename(file_path)
      #            full_path = os.path.join('extracted_files', file_name)
      #            with open(full_path, 'w') as f:
      #                f.write(file_content.strip())  # Strip leading/trailing whitespace

      #    os.makedirs('extracted_files', exist_ok=True)
      #    for root, dirs, files in os.walk('dotfiles'):
      #        for file in files:
      #            if file.endswith('.nix') and file != 'neovim.nix':
      #                extract_content(os.path.join(root, file))
      #    EOF

      #- name: Install linters
      #  run: |
      #    pip install lua-lint fish-lint elisp-lint toml-lint

      #- name: Lint Lua files
      #  run: |
      #    echo "Linting Lua files..."
      #    lua-lint extracted_files/wezterm.lua

      #- name: Lint Fish files
      #  run: |
      #    echo "Linting Fish files..."
      #    fish-lint extracted_files/config.fish
      #    fish-lint extracted_files/nix.fish
      #    fish-lint extracted_files/aliases.fish

      #- name: Lint Emacs Lisp files
      #  run: |
      #    echo "Linting Emacs Lisp files..."
      #    elisp-lint extracted_files/config.el
      #    elisp-lint extracted_files/init.el
      #    elisp-lint extracted_files/packages.el

      #- name: Lint TOML files
      #  run: |
      #    echo "Linting TOML files..."
      #    toml-lint extracted_files/starship.toml
      #    toml-lint extracted_files/yazi.toml
```

</details>

<br>

<a href="your-gmail-link?">:mailbox:</a> How to reach the maintainer

</div>
