# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '+' '+m:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' menu select=5
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle :compinstall filename '/home/julian/.zshrc'

autoload -Uz compinit
compinit

#
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=1000000
unsetopt beep
# End of lines configured by zsh-newuser-install

#
# Prompt setup
#
autoload -U colors && colors

function git_info {
    local bname="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"
    local statc=""
    if [ -n "$bname" ]; then
        if [ -n "$(git status --porcelain 2> /dev/null)" ]; then
            # statc="%{\e[0;3${MNML_ERR_COLOR}m%}"
            statc="%F{220}*"
        fi
        print "%F{075}(%F{078}$bname$statc%F{075})"
    fi
}

if [ -z "$INNIXENV" ]; then
    MAINCOL="%F{032}"
else
    MAINCOL="%F{076}"
fi

setopt PROMPT_SUBST
PROMPT='$MAINCOL%~$(git_info)$MAINCOL %(!.#.>) %F{255}'

#
# General setting
#
# Same colors for tab completion and ls
eval "$(dircolors)"
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Expand with dots
# https://michael.thegrebs.com/2012/09/04/zsh-completion-waiting-dots/
expand-or-complete-with-dots() {
  echo -n "\e[31m...\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# Shift-tab
zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete

#
# Keybinding
#
bindkey "^P" up-line-or-search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

#
# Env
#
export EDITOR=nvim
export SUDO_EDITOR=$EDITOR
export LANG=en_US.UTF-8
export TERM="xterm-256color"
export PATH=$PATH:$HOME/Scripts/bin

#
# Alias
#
alias vim="nvim"
alias ls='ls --color=auto'

#
# Functions
#
function nixenv {
    export INNIXENV="true"
    SHELL=""
    if [ "$#" -gt 0 ]; then
        SHELL=$HOME/.nix-shells/$1
    fi
    RUN=zsh
    if [ "$#" -gt 1 ]; then
        RUN=$2
    fi
    nix-shell $SHELL --run $RUN
}
function gittr {
	if [ $# -eq 0 ]
	then
		git push -u origin HEAD
		return
	fi
	git push -u $1 HEAD
}

function goto {
    $HOME/Scripts/goto $@
    if [ $? -eq 3 ]; then
        cd $(</tmp/where)
    fi
}

