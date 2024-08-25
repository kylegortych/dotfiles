{ config, pkgs, ... }:

{
  imports =
    [ 
      <home-manager/nixos>
    ];

  home-manager.users.blank = { pkgs, ... }: {
    home.file = {
      ".config/fish/config.fish".text = ''
        if status is-interactive
          set fish_greeting
          fish_vi_key_bindings
          starship init fish | source

          set -x NVIM_LOG_FILE /home/blank/.config/nvim/log/.nvimlog
        
          fish_add_path /home/blank/.config/emacs/bin
        
          #git
          set -x GITHUB_USERNAME blank
          set -x GITHUB_TOKEN blank
        
          #Kenzie AWS
          set -x blank 
          set -x blank 
          set -x blank 
        end
      '';

      ".config/fish/conf.d/aliases.fish".text = ''
        function backup
          $argv[1] rsync -rgloptuvz --delete --files-from=/home/blank/.config/rsync/conf.txt / $argv[2]
        end
        
        function ls-permissions
          find $argv -maxdepth 1 -printf "%m %u %f\n" | column
        end

        function system-info
          cat ~/system-info.txt
        end

        function system-info-cache
          neofetch > ~/system-info.txt
        end
        
        function virus-scan
          $argv[1] clamdscan --fdpass -m -i --move=/home/blank/quarantine $argv[2]
        end
        
        #function sys-upgrade
        #  $argv nix-channel --update
        #  $argv nixos-rebuild boot --upgrade
        #end
        
        #function nix-ls
        #  nix-env --list-generations
        #  sed
        #end
        
        #function nix-clean
        #  $argv nix-store --optimise -v -j auto &&
        #  $argv nix-collect-garbage -d &&
        #  nix-store --optimise -v -j auto &&
        #  nix-collect-garbage -d &&
        #  $argv nixos-rebuild switch
        #end
        
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

        function file-manager
          cd (xplr --print-pwd-as-result)
        end
      '';

      ".config/fish/completions/nix.fish".text = ''
        #test
      '';
      };
    };
}
