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

if [ -f /usr/bin/nvim -o -f /usr/local/bin/nvim ] ; then
    alias vi='nvim'
    export EDITOR='nvim'
else
    alias vi='vim'
    export EDITOR='vim'
fi
unset MAILCHECK

if [ -f /usr/bin/keychain ] ; then
    eval $(keychain --eval --agents ssh id_rsa id_ed25519 &> /dev/null)
fi

# Keybindings.
bindkey -v
bindkey '^R' history-incremental-search-backward

# Show current git branch.
source ~/.zshgit/zshrc.sh
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
