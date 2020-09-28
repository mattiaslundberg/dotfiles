# If not running interactively, don't do anything
[ -z "$PS1" ] && return

shopt -s histappend

HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias ll='ls -la'
alias la='ls -A'
alias l='ls -l'
alias cd.='cd ..'
alias cd..='cd ..'
alias mkdir='mkdir -pv'

export PATH=$PATH:/sbin
alias vi='vim'
export EDITOR='vim'
unset MAILCHECK

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
	. /etc/bash_completion
fi
