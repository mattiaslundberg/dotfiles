# zmodload zsh/datetime
# setopt PROMPT_SUBST
# PS4='+$EPOCHREALTIME %N:%i> '
# logfile=$(mktemp zsh_profile.XXXXXXXX)
# echo "Logging to $logfile"
# exec 3>&2 2>$logfile
# setopt XTRACE

autoload -Uz promptinit
autoload -U colors && colors
promptinit

if [ -f /usr/local/share/antigen/antigen.zsh ] ; then
	source /usr/local/share/antigen/antigen.zsh
fi
if [ -f /usr/share/zsh/share/antigen.zsh ] ; then
	source /usr/share/zsh/share/antigen.zsh
fi
if [ -f ~/.antigen/antigen.zsh ] ; then
    source ~/antigen/.antigen.zsh
fi

antigen bundle mafredri/zsh-async
antigen bundle sindresorhus/pure
antigen bundle djui/alias-tips
antigen bundle mdumitru/fancy-ctrl-z
antigen bundle zsh-users/zsh-autosuggestions

setopt COMPLETE_ALIASES
setopt histignorealldups sharehistory
setopt autocd
set -o shwordsplit

HISTSIZE=30000
SAVEHIST=30000
HISTFILE=~/.zsh_history

# Show <status> <time> in right prompt
export RPROMPT="%? %*"

autoload -U colors && colors
if [ -f /usr/bin/dircolors ] ; then
    eval "$(dircolors -b)"
fi

# Use modern completion system
fpath=(~/.zsh/functions $fpath)
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

export COPY_EXTENDED_ATTRIBUTES_DISABLE=true
export COPYFILE_DISABLE=true

# Enable persistent history for elixir
export ERL_AFLAGS="-kernel shell_history enabled"

unset MAILCHECK

alias emd="(LC_ALL=en_US.utf-8 emacs --daemon && emacsclient -nqc .) &"
alias em="emacsclient -nq"
alias em.="emacsclient -nq ."
alias emc="emacsclient -nqc"
alias emc.="emacsclient -nqc ."
alias vi='vim'
export EDITOR="vi"

alias mex="iex -S mix"

addToPath () {
    case ":$PATH:" in
        *":${1}:"*) :;; # already there
        *) export PATH="${1}:$PATH";;
    esac
}

if [ -f /usr/bin/keychain ] ; then
    eval $(keychain --eval --agents ssh id_rsa id_ed25519)
fi

# Keybindings.
bindkey -v
bindkey '^R' history-incremental-search-backward

# Fastjump
j() { cd $(fastjump $1) }
chpwd() {
    fastjump --save-visit $PWD
}

# Load external files
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi
if [ -e ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi

addToPath /sbin
addToPath /usr/sbin
addToPath /usr/local/sbin
addToPath ~/bin

if [ -f ~/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
fi
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

if [ -f /usr/local/bin/brew ]; then
    . /usr/local/opt/asdf/asdf.sh
    . /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash
fi
if [ -f ~/.asdf/asdf.sh ]; then
    . ~/.asdf/asdf.sh
    . ~/.asdf/completions/asdf.bash
fi

addToPath ~/.cargo/bin


eval "$(pipenv --completion)"

if [ -f /usr/share/fzf/key-bindings.zsh ]; then
    . /usr/share/fzf/key-bindings.zsh
fi
if [ -f /usr/local/opt/fzf/shell/key-bindings.zsh ]; then
    . /usr/local/opt/fzf/shell/key-bindings.zsh
fi

if [ -f ~/.fzf.zsh ]; then
    . ~/.fzf.zsh
fi


case $USER in
  root)
    export HOME="/root"
  ;;
  *)

  ;;
esac

antigen bundle zsh-users/zsh-syntax-highlighting
antigen apply

# unsetopt XTRACE
# exec 2>&3 3>&-
