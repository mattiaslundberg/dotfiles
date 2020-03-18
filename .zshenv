unset MAILCHECK
export EDITOR="vim"

export PIPENV_IGNORE_VIRTUALENVS=1

export COPY_EXTENDED_ATTRIBUTES_DISABLE=true
export COPYFILE_DISABLE=true

addToPath () {
    case ":$PATH:" in
        *":${1}:"*) :;; # already there
        *) export PATH="${1}:$PATH";;
    esac
}

addToPath /sbin
addToPath /usr/sbin
addToPath /usr/local/sbin
addToPath /usr/local/bin
addToPath ~/bin
addToPath ~/.emacs.d/bin/
addToPath ~/go/bin

export PYENV_ROOT="$HOME/.pyenv"
addToPath "$PYENV_ROOT/bin"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
