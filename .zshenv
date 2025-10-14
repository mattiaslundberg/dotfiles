unset MAILCHECK
export EDITOR="emacs"

export LANG=en_US.UTF-8

export PIPENV_IGNORE_VIRTUALENVS=1
export MICROPIPENV_NO_LOCKFILE_WRITE=1

export COPY_EXTENDED_ATTRIBUTES_DISABLE=true
export COPYFILE_DISABLE=true

export ERL_AFLAGS="-kernel shell_history enabled"
export KERL_CONFIGURE_OPTIONS="--without-javac"

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_INSECURE_REDIRECT=1
export HOMEBREW_NO_ENV_HINTS=1
export HOMEBREW_CASK_OPTS=--require-sha
export HOMEBREW_CACHE=/opt/HomebrewCache
export HOMEBREW_DOWNLOAD_CONCURRENCY=auto

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
addToPath /opt/homebrew/bin
addToPath ~/.bin
addToPath ~/.emacs.d/bin/
addToPath ~/go/bin
addToPath ~/.cargo/bin
addToPath ~/.local/bin
