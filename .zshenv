unset MAILCHECK
export EDITOR="emacs"

export PIPENV_IGNORE_VIRTUALENVS=1
export MICROPIPENV_NO_LOCKFILE_WRITE=1

export COPY_EXTENDED_ATTRIBUTES_DISABLE=true
export COPYFILE_DISABLE=true

export ERL_AFLAGS="-kernel shell_history enabled"
export KERL_CONFIGURE_OPTIONS="--without-javac"

addToPath() {
	case ":$PATH:" in
	*":${1}:"*) : ;; # already there
	*) export PATH="${1}:$PATH" ;;
	esac
}

addToPath /sbin
addToPath /usr/sbin
addToPath /usr/local/sbin
addToPath /usr/local/bin
addToPath ~/bin
addToPath ~/.emacs.d/bin/
addToPath ~/go/bin
addToPath ~/.elixir-ls/bin

export PYENV_ROOT="$HOME/.pyenv"
addToPath "$PYENV_ROOT/bin"
