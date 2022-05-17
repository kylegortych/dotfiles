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

# neofetch
if status is-interactive
neofetch --colors --colors 9 2 3 39 15 15 --backend iterm2 --source /Users/kylegortych/Downloads/vim2.png --size 20%
date +"-- %a %m-%d-%Y %I:%M%p -----------------------"
echo -e ' '
ps
end

# aliases

function aliases
    column -t -s '|' < ~/Main\ Directory/CLI\ Support/Aliase\ Support/fish_aliases.txt | tr '*' ' '
end

function list-build-sys
    cat ~/Main\ Directory/CLI\ Support/Aliase\ Support/list\ build\ sys.txt | tr '*' ' '
end

function list-env
    env | column -t -s '='
end

function brew-list-pkgs
    column -t -s '|' < ~/Main\ Directory/CLI\ Support/Aliase\ Support/brew-pkgs.txt | tr '*' ' '
end

function brew-search-active-pkgs
    echo -e '\e[4mPackages no Depens\e[0m' ; brew leaves | column ; echo '' ; echo -e '\e[4mCasks\e[0m' ; brew list --cask
end

function pip3-list-pkgs
    pip3 list --not-required
end

function npm-env
    echo -e pkgs\n----- && npm list -g --depth=0 && echo -e env\n---- && npm config list
end

function open-app
    open -a "$argv" && exit
end

function file-manager
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
#     kotlinc $argv -include-runtime -d $argv && java -jar $argv
# end
