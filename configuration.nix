# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  #nix settings
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  #hardware.system76.enableAll = true;
  hardware.bluetooth.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
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
  #time.timeZone = "";
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

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 20;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.network-setup.enable = false;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.displayManager.defaultSession = "plasma";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  networking.hostName = "nixos"; # Define your hostname.
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
  users.users.username = {
    isNormalUser = true;
    description = "name";
    extraGroups = [ "networkmanager" "wheel" "docker" "vboxusers"];
    shell = pkgs.fish;
    packages = with pkgs; [ 
      aws-sam-cli 
    ];
  };

  home-manager.users.username = { pkgs, ... }: {
    home.stateVersion = "23.11";
    nixpkgs.config.allowUnfree = true;

    home.file = {
      ".config/fish/config.fish".text = ''
        if status is-interactive
          set fish_greeting
          fish_vi_key_bindings
          starship init fish | source
        
          set -x NVIM_LOG_FILE /home/kg/.config/nvim/log/.nvimlog
        
          fish_add_path /home/kg/.config/emacs/bin
        
          set -x JAVA_HOME /nix/store/<hash>-adoptopenjdk-hotspot-bin-16.0.2
          set -x PATH $JAVA_HOME/bin $PATH
        end
      '';

      ".config/fish/conf.d/aliases.fish".text = ''
        function backup
          $argv[1] rsync -rgloptuvz --delete --files-from=/home/kg/.config/rsync/conf.txt / $argv[2]
        end
        
        function ls-permissions
          find $argv -maxdepth 1 -printf "%m %u %f\n" | column
        end
        
        function virus-scan
          $argv[1] clamdscan --fdpass -m -i --move=/home/kg/quarantine $argv[2]
        end
        
        function sys-upgrade
          $argv nix-channel --update
          $argv nixos-rebuild switch --upgrade
        end
        
        function nix-ls
          nix-env --list-generations
          #sed
        end
        
        function nix-clean
          $argv nix-env --delete-generations +2
          $argv nix-store --optimise -v -j auto
          $argv nix-store --gc
        end
        
        function git-merge-req
          git push -o merge_request.create -o merge_request.target=$argv[1] origin $argv[2]
        end
        
        function gpg-toggle
          set decrypted_file (echo $argv | sed 's/\.gpg$//')
          set encrypted_file (echo $argv | sed '/\.gpg$/! s/$/.gpg/')
          gpg $argv 2>/dev/null && chmod 600 $decrypted_file  ||
          gpg -c --no-symkey-cache $argv && rm -f $argv &&
          chmod 600 $encrypted_file 2>/dev/null
        end
      '';

      ".config/fish/completions/nix.fish".text = ''
        #test
      '';

      ".config/starship.toml".text = ''
        [character]
        success_symbol = "[❯❯](bold 208) "
        error_symbol = "[ ❯❯](bold red) "
        vimcmd_symbol = "[❮❮](bold 208) "
        
        [cmd_duration]
        #min_time = 500
        format = "took [$duration](bold 208)"
      '';

      ".config/wezterm/wezterm.lua".text = ''
        local wezterm = require("wezterm")
        return {
          font = wezterm.font 'ShureTechMono Nerd Font',
          font_size = 16.0,
          default_cursor_style = 'BlinkingBlock',
          color_scheme = 'Monokai Remastered',
          window_background_opacity = 0.9,
          colors = {
            background = '#191919',
          }
        }
      '';

      ".config/doom/config.el".text = ''
        ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
                
                ;; Place your private configuration here! Remember, you do not need to run 'doom
                ;; sync' after modifying this file!
                
                
                ;; Some functionality uses this to identify you, e.g. GPG configuration, email
                ;; clients, file templates and snippets. It is optional.
                (setq user-full-name "John Doe"
                      user-mail-address "john@doe.com")
                
                ;; Doom exposes five (optional) variables for controlling fonts in Doom:
                ;;
                ;; - `doom-font' -- the primary font to use
                ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
                ;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
                ;;   presentations or streaming.
                ;; - `doom-unicode-font' -- for unicode glyphs
                ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
                ;;
                ;; See 'C-h v doom-font' for documentation and more examples of what they
                ;; accept. For example:
                ;;
                ;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
                ;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
                ;;
                ;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
                ;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
                ;; refresh your font settings. If Emacs still can't find your font, it likely
                ;; wasn't installed correctly. Font issues are rarely Doom issues!
                
                ;; There are two ways to load a theme. Both assume the theme is installed and
                ;; available. You can either set `doom-theme' or manually load a theme with the
                ;; `load-theme' function. This is the default:
                ;;(setq doom-theme 'doom-one)
                (setq doom-theme 'klere)
                
                ;; This determines the style of line numbers in effect. If set to `nil', line
                ;; numbers are disabled. For relative line numbers, set this to `relative'.
                (setq display-line-numbers-type t)
                
                ;; If you use `org' and don't want your org files in the default location below,
                ;; change `org-directory'. It must be set before org loads!
                (setq org-directory "~/org/")
                
                
                ;; Whenever you reconfigure a package, make sure to wrap your config in an
                ;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
                ;;
                ;;   (after! PACKAGE
                ;;     (setq x y))
                ;;
                ;; The exceptions to this rule:
                ;;
                ;;   - Setting file/directory variables (like `org-directory')
                ;;   - Setting variables which explicitly tell you to set them before their
                ;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
                ;;   - Setting doom variables (which start with 'doom-' or '+').
                ;;
                ;; Here are some additional functions/macros that will help you configure Doom.
                ;;
                ;; - `load!' for loading external *.el files relative to this one
                ;; - `use-package!' for configuring packages
                ;; - `after!' for running code after a package has loaded
                ;; - `add-load-path!' for adding directories to the `load-path', relative to
                ;;   this file. Emacs searches the `load-path' when you load packages with
                ;;   `require' or `use-package'.
                ;; - `map!' for binding new keys
                ;;
                ;; To get information about any of these functions/macros, move the cursor over
                ;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
                ;; This will open documentation for it, including demos of how they are used.
                ;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
                ;; etc).
                ;;
                ;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
                ;; they are implemented.
                
                (setq initial-frame-alist '((top . 1) (right . 1) (width . 150) (height . 50)))
                
                ;; font
                (setq doom-font (font-spec :family "ShureTechMono Nerd Font Mono" :size 26))
                
                ;; dashboard
                ;;(setq fancy-splash-image "/Users/kylegortych/Downloads/doom-emacs-bw-light.svg")
                
                (defun skull ()
                  (let* ((banner '("   .o oOOOOOOOo                                            OOOo    "
                                   "   Ob.OOOOOOOo  OOOo.      oOOo.                      .adOOOOOOO   "
                                   "   OboO000000000000.OOo. .oOOOOOo.    OOOo.oOOOOOo..0000000000OO   "
                                   "   OOP.oOOOOOOOOOOO iPOOOOOOOOOOOo.   `iOOOOOOOOOP,OOOOOOOOOOOB'   "
                                   "   `O'OOOO'     `OOOOo'OOOOOOOOOOO` .adOOOOOOOOO'oOOO'    `OOOOo   "
                                   "   .OOOO'            `OOOOOOOOOOOOOOOOOOOOOOOOOO'            `OO   "
                                   "   OOOOO                 'iOOOOOOOOOOOOOOOOi`                oOO   "
                                   "  oOOOOOba.                .adOOOOOOOOOOba               .adOOOOo. "
                                   " oOOOOOOOOOOOOOba.    .adOOOOOOOOOO@^OOOOOOOba.     .adOOOOOOOOOOOO"
                                   "OOOOOOOOOOOOOOOOO.OOOOOOOOOOOOOO'`  ``OOOOOOOOOOOOO.OOOOOOOOOOOOOO "
                                   "`OOOO`       `YOoOOOOMOIONODOO``  .   ``OOROAOPOEOOOoOY`     `OOO` "
                                   "   Y           'OOOOOOOOOOOOOO: .oOOo. :OOOOOOOOOOO?'         :`   "
                                   "   :            .oO%OOOOOOOOOOo.OOOOOO.oOOOOOOOOOOOO?         .    "
                                   "   .            oOOPi%OOOOOOOOoOOOOOOO?oOOOOO?OOOOiOOo             "
                                   "                '%o  OOOO'%OOOO%'%OOOOO'OOOOOO'OOO':               "
                                   "                     `$i  `OOOO' `O'Y ' `OOOO'  o             .    "
                                   "   .                  .     OP'          : o     .                 "
                                   "                             :                                     "
                                   "                             .                                     "
                                   "                                                                   "))
                         (longest-line (apply #'max (mapcar #'length banner))))
                    (put-text-property
                     (point)
                     (dolist (line banner (point))
                       (insert (+doom-dashboard--center
                                +doom-dashboard--width
                                (concat line (make-string (max 0 (- longest-line (length line))) 68)))
                               "\n"))
                     'face 'doom-dashboard-banner)))
                
                (setq +doom-dashboard-ascii-banner-fn #'skull)
                
                (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)
                (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
                
                (add-hook! '+doom-dashboard-functions :append
                  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "config by Kyle Gortych")))
                
                (setq confirm-kill-emacs nil)
                
                ;; (setq dired-listing-switches "-a")
                
                ;; opacity
                (set-frame-parameter nil 'alpha-background 95)
                (add-to-list 'default-frame-alist '(alpha-background . 95))

                ;; markdown preview
                (setq markdown-command "pandoc")

                (require 'markdown-mode)
                (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

                (add-to-list 'load-path "/nix/store/<hash>-emacs-xwidgets-reuse-20200817.147/share/emacs/site-lisp/elpa/xwidgets-reuse-20200817.147/xwidgets-reuse")
                (require 'xwidgets-reuse)
                
                (setq xwidgets-reuse-session t)
                
                (defun my-xwidget-webkit-browse-url (url &rest args)
                  "Open URL in xwidget-webkit."
                  (interactive (browse-url-interactive-arg "URL: "))
                  (let ((xwidget-webkit-enable-plugins t))
                    (xwidgets-reuse-create-session)
                    (xwidgets-reuse-load-url url)))
                
                (setq browse-url-browser-function #'my-xwidget-webkit-browse-url)
      '';

      ".config/doom/init.el".text = ''
        ;;; init.el -*- lexical-binding: t; -*-
                
                ;; This file controls what Doom modules are enabled and what order they load
                ;; in. Remember to run 'doom sync' after modifying it!
                
                ;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
                ;;      documentation. There you'll find a link to Doom's Module Index where all
                ;;      of our modules are listed, including what flags they support.
                
                ;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
                ;;      'C-c c k' for non-vim users) to view its documentation. This works on
                ;;      flags as well (those symbols that start with a plus).
                ;;
                ;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
                ;;      directory (for easy access to its source code).
                
                (doom! :input
                       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
                       ;;chinese
                       ;;japanese
                       ;;layout            ; auie,ctsrnm is the superior home row
                
                       :completion
                       company           ; the ultimate code completion backend
                       ;;helm              ; the *other* search engine for love and life
                       ;;ido               ; the other *other* search engine...
                       ;;ivy               ; a search engine for love and life
                       vertico           ; the search engine of the future
                
                       :ui
                       ;;deft              ; notational velocity for Emacs
                       doom              ; what makes DOOM look the way it does
                       doom-dashboard    ; a nifty splash screen for Emacs
                       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
                       ;;(emoji +unicode)  ; 🙂
                       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
                       ;;hydra
                       ;;indent-guides     ; highlighted indent columns
                       ;;ligatures         ; ligatures and symbols to make your code pretty again
                       ;;minimap           ; show a map of the code on the side
                       modeline          ; snazzy, Atom-inspired modeline, plus API
                       ;;nav-flash         ; blink cursor line after big motions
                       neotree           ; a project drawer, like NERDTree for vim
                       ophints           ; highlight the region an operation acts on
                       (popup +defaults)   ; tame sudden yet inevitable temporary windows
                       ;;tabs              ; a tab bar for Emacs
                       ;;treemacs          ; a project drawer, like neotree but cooler
                       ;;unicode           ; extended unicode support for various languages
                       (vc-gutter +pretty) ; vcs diff in the fringe
                       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
                       ;;window-select     ; visually switch windows
                       workspaces        ; tab emulation, persistence & separate workspaces
                       ;;zen               ; distraction-free coding or writing
                
                       :editor
                       (evil +everywhere); come to the dark side, we have cookies
                       file-templates    ; auto-snippets for empty files
                       fold              ; (nigh) universal code folding
                       ;;(format +onsave)  ; automated prettiness
                       ;;god               ; run Emacs commands without modifier keys
                       ;;lispy             ; vim for lisp, for people who don't like vim
                       ;;multiple-cursors  ; editing in many places at once
                       ;;objed             ; text object editing for the innocent
                       ;;parinfer          ; turn lisp into python, sort of
                       ;;rotate-text       ; cycle region at point between text candidates
                       snippets          ; my elves. They type so I don't have to
                       ;;word-wrap         ; soft wrapping with language-aware indent
                
                       :emacs
                       dired             ; making dired pretty [functional]
                       electric          ; smarter, keyword-based electric-indent
                       ;;ibuffer         ; interactive buffer management
                       undo              ; persistent, smarter undo for your inevitable mistakes
                       vc                ; version-control and Emacs, sitting in a tree
                
                       :term
                       ;;eshell            ; the elisp shell that works everywhere
                       ;;shell             ; simple shell REPL for Emacs
                       ;;term              ; basic terminal emulator for Emacs
                       vterm             ; the best terminal emulation in Emacs
                
                       :checkers
                       syntax              ; tasing you for every semicolon you forget
                       (spell +flyspell) ; tasing you for misspelling mispelling
                       grammar           ; tasing grammar mistake every you make
                
                       :tools
                       ;;ansible
                       ;;biblio            ; Writes a PhD for you (citation needed)
                       ;;debugger          ; FIXME stepping through code, to help you add bugs
                       ;;direnv
                       ;;docker
                       ;;editorconfig      ; let someone else argue about tabs vs spaces
                       ;;ein               ; tame Jupyter notebooks with emacs
                       (eval +overlay)     ; run code, run (also, repls)
                       ;;gist              ; interacting with github gists
                       lookup              ; navigate your code and its documentation
                       lsp               ; M-x vscode
                       magit             ; a git porcelain for Emacs
                       ;;make              ; run make tasks from Emacs
                       ;;pass              ; password manager for nerds
                       ;;pdf               ; pdf enhancements
                       ;;prodigy           ; FIXME managing external services & code builders
                       ;;rgb               ; creating color strings
                       ;;taskrunner        ; taskrunner for all your projects
                       ;;terraform         ; infrastructure as code
                       ;;tmux              ; an API for interacting with tmux
                       ;;tree-sitter       ; syntax and parsing, sitting in a tree...
                       ;;upload            ; map local to remote projects via ssh/ftp
                
                       :os
                       (:if IS-MAC macos)  ; improve compatibility with macOS
                       ;;tty               ; improve the terminal Emacs experience
                
                       :lang
                       ;;agda              ; types of types of types of types...
                       ;;beancount         ; mind the GAAP
                       ;;(cc +lsp)         ; C > C++ == 1
                       ;;clojure           ; java with a lisp
                       ;;common-lisp       ; if you've seen one lisp, you've seen them all
                       ;;coq               ; proofs-as-programs
                       ;;crystal           ; ruby at the speed of c
                       ;;csharp            ; unity, .NET, and mono shenanigans
                       ;;data              ; config/data formats
                       ;;(dart +flutter)   ; paint ui and not much else
                       ;;dhall
                       ;;elixir            ; erlang done right
                       ;;elm               ; care for a cup of TEA?
                       emacs-lisp        ; drown in parentheses
                       ;;erlang            ; an elegant language for a more civilized age
                       ;;ess               ; emacs speaks statistics
                       ;;factor
                       ;;faust             ; dsp, but you get to keep your soul
                       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
                       ;;fsharp            ; ML stands for Microsoft's Language
                       ;;fstar             ; (dependent) types and (monadic) effects and Z3
                       ;;gdscript          ; the language you waited for
                       (go +lsp)         ; the hipster dialect
                       ;;(graphql +lsp)    ; Give queries a REST
                       (haskell +lsp)    ; a language that's lazier than I am
                       ;;hy                ; readability of scheme w/ speed of python
                       ;;idris             ; a language you can depend on
                       json              ; At least it ain't XML
                       (java +lsp)       ; the poster child for carpal tunnel syndrome
                       javascript        ; all(hope(abandon(ye(who(enter(here))))))
                       ;;julia             ; a better, faster MATLAB
                       ;;kotlin            ; a better, slicker Java(Script)
                       latex             ; writing papers in Emacs has never been so fun
                       ;;lean              ; for folks with too much to prove
                       ;;ledger            ; be audit you can be
                       lua               ; one-based indices? one-based indices
                       markdown          ; writing docs for people to ignore
                       ;;nim               ; python + lisp at the speed of c
                       nix               ; I hereby declare "nix geht mehr!"
                       ;;ocaml             ; an objective camel
                       org               ; organize your plain life in plain text
                       php               ; perl's insecure younger brother
                       plantuml          ; diagrams for confusing people more
                       ;;purescript        ; javascript, but functional
                       python            ; beautiful is better than ugly
                       ;;qt                ; the 'cutest' gui framework ever
                       ;;racket            ; a DSL for DSLs
                       ;;raku              ; the artist formerly known as perl6
                       ;;rest              ; Emacs as a REST client
                       ;;rst               ; ReST in peace
                       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
                       (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
                       ;;scala             ; java, but good
                       ;;(scheme +guile)   ; a fully conniving family of lisps
                       sh                ; she sells {ba,z,fi}sh shells on the C xor
                       ;;sml
                       ;;solidity          ; do you need a blockchain? No.
                       ;;swift             ; who asked for emoji variables?
                       ;;terra             ; Earth and Moon in alignment for performance.
                       ;;web               ; the tubes
                       yaml              ; JSON, but readable
                       ;;zig               ; C, but simpler
                
                       :email
                       ;;(mu4e +org +gmail)
                       ;;notmuch
                       ;;(wanderlust +gmail)
                
                       :app
                       ;;calendar
                       ;;emms
                       ;;everywhere        ; *leave* Emacs!? You must be joking
                       ;;irc               ; how neckbeards socialize
                       ;;(rss +org)        ; emacs as an RSS reader
                       ;;twitter           ; twitter client https://twitter.com/vnought
                
                       :config
                       ;;literate
                       (default +bindings +smartparens))
      '';

      ".config/doom/packages.el".text = ''
        ;; -*- no-byte-compile: t; -*-
                ;;; $DOOMDIR/packages.el
                
                ;; To install a package with Doom you must declare them here and run 'doom sync'
                ;; on the command line, then restart Emacs for the changes to take effect -- or
                ;; use 'M-x doom/reload'.
                
                
                ;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                ;(package! some-package)
                
                ;; To install a package directly from a remote git repo, you must specify a
                ;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
                ;; https://github.com/radian-software/straight.el#the-recipe-format
                ;(package! another-package
                ;  :recipe (:host github :repo "username/repo"))
                
                ;; If the package you are trying to install does not contain a PACKAGENAME.el
                ;; file, or is located in a subdirectory of the repo, you'll need to specify
                ;; `:files' in the `:recipe':
                ;(package! this-package
                ;  :recipe (:host github :repo "username/repo"
                ;           :files ("some-file.el" "src/lisp/*.el")))
                
                ;; If you'd like to disable a package included with Doom, you can do so here
                ;; with the `:disable' property:
                ;(package! builtin-package :disable t)
                
                ;; You can override the recipe of a built in package without having to specify
                ;; all the properties for `:recipe'. These will inherit the rest of its recipe
                ;; from Doom or MELPA/ELPA/Emacsmirror:
                ;(package! builtin-package :recipe (:nonrecursive t))
                ;(package! builtin-package-2 :recipe (:repo "myfork/package"))
                
                ;; Specify a `:branch' to install a package from a particular branch or tag.
                ;; This is required for some packages whose default branch isn't 'master' (which
                ;; our package manager can't deal with; see radian-software/straight.el#279)
                ;(package! builtin-package :recipe (:branch "develop"))
                
                ;; Use `:pin' to specify a particular commit to install.
                ;(package! builtin-package :pin "1a2b3c4d5e")
                
                
                ;; Doom's packages are pinned to a specific commit and updated from release to
                ;; release. The `unpin!' macro allows you to unpin single packages...
                ;(unpin! pinned-package)
                ;; ...or multiple packages
                ;(unpin! pinned-package another-pinned-package)
                ;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                ;(unpin! t)
                
                (package! klere-theme)
      '';
    };

    programs.neovim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        #nvim-lspconfig {
        #  plugin = nvim-lspconfig;
        #  #config = "";
        #}
        #nvim-cmp {
        #  plugin = nvim-cmp;
        #  #config = "";
        #}
        pear-tree { 
          plugin = pear-tree; 
          #config = ""; 
        }
        vim-cool { 
          plugin = vim-cool; 
          #config = ""; 
        }
      ];
      extraLuaConfig = ''
        --[[

        local cmp = require('cmp')
        local lsp = require('cmp_nvim_lsp')

        cmp.setup({
          sources = {
            { name = 'nvim_lsp' },
            -- Add other completion sources as needed
          },
          mapping = {
            ['<Tab>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
            ['<S-Tab>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
          },
          snippet = {
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body)
            end,
          },
          documentation = {
            border = 'rounded',
          },
          formatting = {
            format = lsp.formatting(),
          },
        })

        -- Configure language servers using lspconfig
        local lspconfig = require('lspconfig')

        lspconfig.lua.setup({
          cmd = { 'lua-ls' },
          settings = {
            Lua = {
              runtime = {
                version = 'LuaJIT',
                path = vim.split(package.path, ';'),
              },
              diagnostics = {
                globals = { 'vim' },
              },
              workspace = {
                library = {
                  [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                  [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                },
              },
            },
          },
        })

	      --]]

        -- vim.cmd([[
        --   augroup SaveFolds
        --     autocmd!
        --     autocmd BufWinLeave * mkview
        --   augroup END
        -- ]])

        -- vim.cmd([[
        --   augroup LoadFolds
        --     autocmd!
        --     autocmd BufWinEnter * silent! loadview
        --   augroup END
        -- ]])

        vim.cmd [[colorscheme habamax]]
        vim.cmd [[set clipboard+=unnamedplus]]
        vim.cmd [[let g:netrw_winsize = 25]]
        vim.cmd [[let g:netrw_liststyle = 3]]
        vim.opt.number = true
        vim.opt.cursorline = true
        vim.opt.tabstop = 2
        vim.opt.shiftwidth = 2
        vim.opt.expandtab = true
        vim.opt.softtabstop = 2
      '';
    };

    programs.git = {
      enable = true;
      #credential.helper=blank
      #userName = "";
      #userEmail = "";
    };

    home.packages = with pkgs; [
      #gui
      firefox
      thunderbird
      libreoffice
      obs-studio
      bitwarden
      kdenlive
      mpv
      freecad
      kicad
      logisim-evolution
      postman
      jetbrains.idea-community
      blender-hip
      zoom-us
      wezterm

      #emacs
      emacs29-gtk3
      emacsPackages.xwidgets-reuse
      pandoc

      #kde specific
      libsForQt5.kwin-tiling
      libsForQt5.sddm-kcm

      # cli
      fish
      starship
      wl-clipboard
      xclip
      calcurse
      btop
      ripgrep
      figlet
      ffmpeg_6
      neofetch
      parallel
      file
      xdg-ninja
      screenkey
      lynis

      #lang
      verilog
      go
      rustup
      gnu-cobol

      #java
      openjdk16-bootstrap
      gradle

      #python
      python311
      python311Packages.datetime
      python311Packages.ptpython

      #C & C++
      gcc
      cmake
      ghc

      #kenzie
      awscli2
      #aws-sam-cli

      #inteliji idea dependency
      graphviz
      plantuml

      #latex
      texlive.combined.scheme-full
    ];
  };

  #programs.kdeconnect.enable = true;

  programs.fish = {
    enable = true;
  };

  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "ShareTechMono" ]; })
  ];

  services.clamav = {
    daemon.enable = true;
    # updater.enable = true;
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  #environment.systemPackages = with pkgs; [
  #
  #];

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
