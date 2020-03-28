export TERM="xterm-256color"

# Path to your oh-my-zsh installation.
export ZSH=/home/julian/.oh-my-zsh

# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="af-magic"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git sudo pass)

source $ZSH/oh-my-zsh.sh

autoload -Uz compinit
compinit

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
#


PATH=~/go/bin:$PATH

alias ed="ed -p '> '"
alias vim="nvim"
#alias emacs="emacsclient -c"
#alias cmacs="emacsclient -nw -c"
alias sc="systemctl"

alias hej="echo 'Hej, hvordan går det?'"
export EDITOR=nvim

#Setup ssh agent

# Sæt sudo editor
export SUDO_EDITOR=nvim
SUDO_EDITOR=nvim

export ANSIBLE_NOCOWS=1

export PATH=$HOME/Scripts:$PATH

# Creates a new shell in the current shell
alias new="ZSH_NEST=$((ZSH_NEST + 1)) zsh"

# If we are a subshell add it to the PS1
if [ ! -z "$ZSH_NEST" ] 
then
	export PS1=$(echo $PS1 | sed "s:}%:}[$ZSH_NEST]%:")
fi

bindkey "^P" up-line-or-search

function gittr {
	if [ $# -eq 0 ]
	then
		git push -u origin HEAD
		return
	fi
	git push -u $1 HEAD
}
