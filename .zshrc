# Set up the prompt
autoload -Uz promptinit
autoload -U colors && colors
promptinit

PROMPT="%{$fg_bold[green]%}%n%{$reset_color%}@%{$fg_bold[magenta]%}%m %{$fg_no_bold[yellow]%}%3~ %{$reset_color%}"

setopt histignorealldups sharehistory
setopt autocd
set -o shwordsplit

HISTSIZE=3000
SAVEHIST=3000
HISTFILE=~/.zsh_history

autoload -U colors && colors
if [ -f /usr/bin/dircolors ] ; then
    eval "$(dircolors -b)"
fi

# Use modern completion system
autoload -Uz compinit
compinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"


if [ -f /usr/bin/nvim -o -f /usr/local/bin/nvim ] ; then
    alias vi='nvim'
    export EDITOR='nvim'
    export DIFFPROG='nvim -d'
else
    alias vi='vim'
    export EDITOR='vim'
fi
unset MAILCHECK

if [ -f /usr/bin/keychain ] ; then
    eval $(keychain --eval --agents ssh id_rsa id_ed25519)
fi

# Keybindings.
bindkey -v
bindkey '^R' history-incremental-search-backward

# Show current git branch.
source ~/.zsh/zsh-git-prompt.git/zshrc.sh
precmd () {
    RPROMPT="$(git_super_status)"
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
if [ -f /usr/bin/virtualenvwrapper.sh ]; then
    . /usr/bin/virtualenvwrapper.sh
fi

case $USER in
  root)
    export HOME="/root"
  ;;
  *)

  ;;
esac

export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin:~/bin"

if [ -f ~/.zsh/zsh-syntax-highlighting.git/zsh-syntax-highlighting.zsh ] ; then
    . ~/.zsh/zsh-syntax-highlighting.git/zsh-syntax-highlighting.zsh
fi
