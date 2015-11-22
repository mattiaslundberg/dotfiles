# Set up the prompt
autoload -Uz promptinit
promptinit

autoload -U colors && colors

case $USER in
root)
    PROMPT="%{$fg_bold[red]%}%n%{$reset_color%}@%{$fg_bold[magenta]%}%m %{$fg_no_bold[yellow]%}%3~ %{$reset_color%}"
;;

mattias)
    PROMPT="%{$fg_bold[green]%}m%{$reset_color%}@%{$fg_bold[magenta]%}%m %{$fg_no_bold[yellow]%}%3~ %{$reset_color%}"
;;
*)
    PROMPT="%{$fg_bold[green]%}%n%{$reset_color%}@%{$fg_bold[magenta]%}%m %{$fg_no_bold[yellow]%}%3~ %{$reset_color%}"
;;
esac

setopt histignorealldups sharehistory
setopt autocd
set -o shwordsplit

# Keep 3000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=3000
SAVEHIST=3000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

if [ -f /usr/bin/nvim ] ; then
    alias vi='nvim'
    export EDITOR='nvim'
else
    alias vi='vim'
    export EDITOR='vim'
fi
unset MAILCHECK

envoy -t ssh-agent
source <(envoy -p)

# Keybindings.
bindkey -v
bindkey '^R' history-incremental-search-backward

# Show current git branch.
source ~/.zshgit/zshrc.sh
precmd () {
    RPROMPT="(...)"
    asyncprompt &!
}

function asyncprompt() {
    P=$(git_super_status)
    printf "%s" $P > /tmp/prompt.$$
    kill -s USR2 $$
}

function TRAPUSR2 {
    RPROMPT=$(cat "/tmp/prompt.$$")
    zle && zle reset-prompt
}

# Load external files
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi
if [ -f ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi
if [ -f ~/.colormap ]; then
    . ~/.colormap
fi

case $USER in
  root)
    # Since we always run the users zshrc we must set this to root so we don't overwrite something important.
    export HOME="/root"
  ;;
  *)

  ;;
esac

export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin:/home/mattias/.bin:/home/mattias/.gem/ruby/2.1.0/bin"
