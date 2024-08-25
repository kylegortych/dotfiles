{ config, pkgs, ... }:

{
  imports =
    [ 
      <home-manager/nixos>
    ];

  home-manager.users.blank = { pkgs, ... }: {
    home.file = {
      ".config/starship.toml".text = ''
        [character]
        success_symbol = "[❯❯](bold 208) "
        error_symbol = "[ ❯❯](bold red) "
        vimcmd_symbol = "[❮❮](bold 208) "

        [jobs]
        symbol = "[](bold white) "
        number_threshold = 1
        
        [cmd_duration]
        #min_time = 500
        format = "took [$duration](bold 208)"

        [aws]
        symbol = "[󰤉](bold yellow) "
      '';
    };
  };
}
