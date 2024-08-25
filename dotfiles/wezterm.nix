{ config, pkgs, ... }:

{
  imports =
    [ 
      <home-manager/nixos>
    ];

  home-manager.users.blank = { pkgs, ... }: {
    home.file = {
      ".config/wezterm/wezterm.lua".text = ''
        local wezterm = require("wezterm")

        local config = {}

        config.initial_cols = 100
        config.initial_rows = 35
        config.font = wezterm.font 'ShureTechMono Nerd Font'
        config.font_size = 16.0
        config.default_cursor_style = 'BlinkingBlock'
        config.color_scheme = 'Monokai Remastered'
        config.window_background_opacity = 0.9
        config.enable_tab_bar = false
        config.colors = {
          background = '#191919',
        }

        return config
      '';
    };
  };
}
