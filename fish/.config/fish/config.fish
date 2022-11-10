# Vim stuff
alias vim "nvim"
set -x EDITOR nvim
set -x SUDO_EDITOR nvim

# Other things
set -x TERM xterm-256color

# Path
set -x PATH $PATH $HOME/go/bin
set -x PATH $PATH $HOME/Scripts/bin

# Functions
function gittr
    if test (count $argv) -lt 1
        git push -u origin HEAD
    else
        git push -u $argv[1] HEAD
    end
end
