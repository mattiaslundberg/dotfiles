unset MAILCHECK
export EDITOR="vim"

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
addToPath ~/bin

export PYENV_ROOT="$HOME/.pyenv"
addToPath "$PYENV_ROOT/bin"
