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

# disable fish greeting
set fish_greeting


# starship prompt
starship init fish | source

# neofetch
if status is-login
neofetch --colors --colors 9 2 3 39 15 15 --backend iterm2 --source /Users/kylegortych/Downloads/vim2.png --size 20%
date +"-- %a %m-%d-%Y %I:%M%p -----------------------"
echo -e ' '
ps
end

# nnn
function n --wraps nnn --description 'support nnn quit and change directory'
    # Block nesting of nnn in subshells
    if test -n "$NNNLVL"
        if [ (expr $NNNLVL + 0) -ge 1 ]
            echo "nnn is already running"
            return
        end
    end

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "-x" as in:
    #    set NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
    #    (or, to a custom path: set NNN_TMPFILE "/tmp/.lastd")
    # or, export NNN_TMPFILE after nnn invocation
    if test -n "$XDG_CONFIG_HOME"
        set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
    else
        set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
    end

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn $argv

    if test -e $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end

# $PATH added to /.config/fish and .profile?
# resolve fragmented paths

# aliases

function aliases
    column -t -s '|' < /Users/kylegortych/Main\ Directory/cli\ scripts/list\ aliases/fish_aliases.txt | tr '*' ' '
end

function list-build-sys
    cat /Users/kylegortych/Main\ Directory/cli\ scripts/list\ build\ sys/list\ build\ sys.txt | tr '*' ' '
end

function list-env
    env | column -t -s '='
end

function brew-list-pkgs
    column -t -s '|' < /Users/kylegortych/Main\ Directory/cli\ scripts/list\ brew\ packages/brew-pkgs.txt | tr '*' ' '
end

function brew-search-active-pkgs
    echo -e '\e[4mPackages no Depens\e[0m' ; brew leaves | column ; echo '' ; echo -e '\e[4mCasks\e[0m' ; brew list --cask
end

function pip3-list-pkgs
    pip3 list --not-required
end

function npm-list-pkgs
    npm list -g --depth=0
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

function unfog-toggle-prjc
   unfog context $argv && unfog list ; unfog context
end

# function open-app
#     open -a $argv && exit
# end

# function kotlin-run-script
#     kotlinc $argv -include-runtime -d $argv && java -jar $argv
# end

# an alias to compile files in a project directory in parallel
# given a projects directory there can by multiple langs
# rather than a make file per project | automates
# use & subshell or gnu parallel

# function parallel-compile
#     semantics: compiler_type args file_name& compiler_type args file_name2&
#     syntax: compiler_type args $argv& compiler_type args $argv&
#
#     if $argv is .hs or .js or etc...
#     use compiler_type with args | apl --noSv --noColor -q -f $argv
#
#     if project has mkfiles in lang specific dirctories
#     use mkfile_1& mkfile_2&
# end

# function fn call to cli script sys link xdg
