# If not running interactively, don't do anything
[ -z "$PS1" ] && return

shopt -s histappend

HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ `whoami` == "mattias" ]; then
    PS1='\[\e[0;36m\]m\[\e[m\]@\[\e[1;35m\]\h\[\e[m\]:\w '
else
    PS1='\[\e[0;31m\]\u\[\e[m\]@\[\e[1;35m\]\h\[\e[m\]:\w '
fi

case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;\u@\h: \w\a\]$PS1"
    trap 'echo -ne "\033]2;$(history 1 | sed "s/^[ ]*[0-9]*[ ]*//g") ($(whoami)@$(hostname))\007"' DEBUG
    ;;
*)
    ;;
esac

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# standard aliases
alias ll='ls -la'
alias la='ls -A'
alias l='ls -l'
alias cd.='cd ..'
alias cd..='cd ..'
alias mkdir='mkdir -pv'
cl() { cd $1 && la ; }

# apt aliases
alias install='sudo apt-get install'
alias update='sudo apt-get update'
alias upgrade='sudo apt-get update && sudo apt-get upgrade'
alias dist-upgrade='sudo apt-get update && sudo apt-get dist-upgrade'
alias autoremove='sudo apt-get autoremove'
alias uninstall='sudo apt-get remove'
alias search='sudo apt-cache search'
alias showpkg='apt-cache show'

# Some other aliases
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias 'du.'='du -hsx * | sort -rh | head -10'
alias reload='cp /home/mattias/Dropbox/settings/.bashrc ~/.bashrc ; . ~/.bashrc'
alias cleanflash='rm -rf ${HOME}/.macromedia/* ${HOME}/.adobe/*'
alias cleantemp='find $HOME -type f -name "*~" -print -exec rm {} \;'
alias cleanlatex='rm -f dia/*.{pdf,eps} {.,sections,appendices,chapters,chp,latex}/*.{out,aux,blg,toc,log,bbl,fdb_latexmk,synctex.gz,eps,dvi,nav,vrb,snm}'

alias ports='sudo netstat -tulpan'
alias :q="echo \"This is not vim. You're tired, go to bed.\""
alias :wq=':q'
alias myip='python -c "from pyquery import PyQuery as pq; dom=pq(url=\"http://checkip.dyndns.org:8245/\"); print dom(\"body\")[0].text.split(\" \")[-1]"'
alias temp='python -c "from pyquery import PyQuery as pq; dom=pq(url=\"http://marge.campus.ltu.se/temp/\"); print dom(\"td.px13>pre\")[0].text.split(\"\n\")[3].replace(\"|\",\"\").strip()"'
alias su='sudo -E -s'
alias root='sudo -E -s'
alias sudo='sudo ' # Make sudo and aliases compatible

diaconvert() { echo $1 | awk -F '.' '{ print $1 }' | awk '{ print "dia -e " $f ".eps -t eps " $f ".dia ; epstopdf " $f ".eps " }' | sh ;}

wsharkremote() { wireshark -k -i <(ssh $1 "if [ $(whoami) != \"root\" ]; then sudo /usr/sbin/tcpdump -i $2 -U -s0 -w - $3 ; else tcpdump -i $2 -U -s0 -w - $3; fi"); }

# copy and paste from the command line - http://www.shell-fu.org/lister.php?id=177  
ccopy(){ cp $1 /tmp/ccopy.$1; }
alias cpaste="ls /tmp/ccopy* | sed 's|[^\.]*.\.||' | xargs -I % mv /tmp/ccopy.% ./%"

export PATH=$PATH:/sbin
alias vi='vim'
export EDITOR='vim'
unset MAILCHECK

if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
