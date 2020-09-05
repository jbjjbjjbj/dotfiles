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

autoload -Uz vcs_info
autoload -U colors && colors
zstyle ':vcs_info:*' formats '%F{075}(%F{078}%b%u%c%F{075})'
precmd() {vcs_info}

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

setopt PROMPT_SUBST
PROMPT='%F{032}%~$(git_info)%F{032} %(!.#.>) %{$reset_color%}'

bindkey "^P" up-line-or-search
bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward


export EDITOR=nvim
export SUDO_EDITOR=$EDITOR
export LANG=en_US.UTF-8
export TERM="xterm-256color"
export LSCOLORS="Gxfxcxdxbxegedabagacad"

alias vim="nvim"
alias ls='ls --color=auto'

# Expand with dots
# https://michael.thegrebs.com/2012/09/04/zsh-completion-waiting-dots/
expand-or-complete-with-dots() {
  echo -n "\e[31m...\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots
