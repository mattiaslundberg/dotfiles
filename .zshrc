autoload -Uz promptinit
autoload -U colors && colors
promptinit

if [ -f ~/.zi/bin/zi.zsh ] ; then
  . ~/.zi/bin/zi.zsh
fi

zi light mafredri/zsh-async
zi light sindresorhus/pure
zi light djui/alias-tips
zi light mdumitru/fancy-ctrl-z

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

autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
# zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# Keybindings.
bindkey -v

if [ -f /opt/homebrew/opt/asdf/asdf.sh ]; then
    . /opt/homebrew/opt/asdf/asdf.sh
    . /opt/homebrew/opt/asdf/etc/bash_completion.d/asdf.bash
fi
if [ -f ~/.asdf/asdf.sh ]; then
    . ~/.asdf/asdf.sh
    . ~/.asdf/completions/asdf.bash
fi

if [ -f ~/.pyenv/bin/pyenv ]; then
	eval "$(pyenv init --path)"
	eval "$(pyenv init - --no-rehash)"
	eval "$(pyenv virtualenv-init -)"
fi

# Load external files
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi
if [ -e ~/.zshrc.local ]; then
    . ~/.zshrc.local
fi
if [ -e ~/.zshrc.local.gpg ]; then
    eval "$(gpg --decrypt ~/.zshrc.local.gpg 2>/dev/null)"
fi

pipenv-docker() {
    docker build -f ~/.dotfiles/.dockerfiles/Dockerfile.pipenv -t mattiaslundberg/docker-pipenv .
    docker run -it -v $(pwd):/usr/repo --rm mattiaslundberg/docker-pipenv 
}

export PATH="~/.cargo/bin:${PATH}"

export FZF_DEFAULT_COMMAND='fd --type f'
if [ -f /opt/homebrew/opt/fzf/shell/key-bindings.zsh ]; then
    . /opt/homebrew/opt/fzf/shell/key-bindings.zsh
fi


if [ -f '/Users/mattias/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/mattias/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/Users/mattias/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/mattias/google-cloud-sdk/completion.zsh.inc'; fi

zi light zsh-users/zsh-syntax-highlighting
