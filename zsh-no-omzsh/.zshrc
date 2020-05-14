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
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=100000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

PROMPT="$FG[032]%~\
$(git_prompt_info) \
$FG[105]%(!.#.»)%{$reset_color%} "
