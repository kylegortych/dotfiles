# Put system-wide fish configuration entries here
# or in .fish files in conf.d/
# Files in conf.d can be overridden by the user
# by files with the same name in $XDG_CONFIG_HOME/fish/conf.d

# This file is run by all fish instances.
# To include configuration only for login shells, use
# if status is-login
#    ...
# end
# To include configuration only for interactive shells, use
# if status is-interactive
#   ...
# end

# Paths
# fish_add_path /usr/local/sbin

# $PATH added to /.config/fish and .profile?
# resolve fragmented paths

# nvim to neovide.app ?
# fish_add_path /usr/local/bin/brew shellenv

# disable fish greeting
set fish_greeting

# starship prompt
starship init fish | source

# negate fish vi cursor
function fish_vi_cursor
  ;
end

# neofetch
if status is-interactive
  # move script to bin | ps outputs interpreter running?
  # ~/.config/CLI\ Support/sh\ scripts/./info.sh
 
  neofetch --colors --colors 9 2 3 39 15 15 --backend iterm2 --source ~/Downloads/vim2.png --size 20%
  date +"-- %a %m-%d-%Y %I:%M%p --"
  printf '\n'
  ps
end

# aliases

function aliases
  column -t -s '|' < ~/.config/CLI\ Support/aliase\ \&\ script\ support/fish_aliases.txt | tr '*' ' '
end
 
function neo
  neofetch --colors --colors 9 2 3 39 15 15 --backend iterm2 --source ~/Downloads/vim2.png --size 20%
end

function ls-build-sys
  cat ~/.config/CLI\ Support/aliase\ \&\ script\ support/list\ build\ sys.txt | tr '*' ' '
end

function ls-env
  env | column -t -s '='
end

function brew-pkgs-nodepens
  column -t -s '|' < ~/.config/CLI\ Support/aliase\ \&\ script\ support/brew-pkgs.txt | tr '*' ' '
end

function brew-active-pkgs-nodepens
  echo -e '\e[4mPackages no Depens\e[0m' ; brew leaves | column ; echo '' ; echo -e '\e[4mCasks\e[0m' ; brew list --cask
end

function pip3-ls-pkgs
  pip3 list --not-required
end

function npm-env
  echo -e pkgs\n----- && npm list -g --depth=0 && echo -e env\n---- && npm config list
end

function npm-update-self
  npm install npm@latest -g
end

function weather
  curl wttr.in/$argv
end

function open:
  open -a "$argv" && exit
end

function xplr-open
  cd $(xplr --print-pwd-as-result)
end

function apl-run-script
  apl --noSV --noColor --noCIN -q -f $argv
end

function verilog-compile
  iverilog -o $argv
end

function verilog-run
  vvp $argv
end

function doom
  ~/.emacs.d/bin/doom $argv
end

# function kotlin-run-script
#   kotlinc $argv -include-runtime -d $argv && java -jar $argv
# end
