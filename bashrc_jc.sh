# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTIGNORE='&:ls:[bf]g:exit'
export PATH="/usr/bin:$PATH:$HOME/go/bin"
export GOPATH=$HOME/go
# from a combination of http://tldp.org/HOWTO/Xterm-Title-4.html 
#                   and http://bashrcgenerator.com/ (2018-11-02)
CLPART="\[$(tput bold)\][\[$(tput sgr0)\]\[$(tput setaf 3)\]\h\[$(tput setaf 15)\]: \[$(tput bold)\]\[$(tput setaf 2)\]\W\[$(tput setaf 7)\]]\\$ \[$(tput sgr0)\]"
PS1=$CLPART
title() {
    if [ -z "$1" ]; then
        echo "title required"
    else
        export PS1="\[\033]0;$1 \h\007\]${CLPART}"
    fi
}
if [ "$USER" != "vagrant" ]; then
  alias emacs="emacsclient -n"
  alias emacs-start="/usr/bin/emacs &> /dev/null &"
fi

# === START: git ===
export GIT_EDITOR="emacsclient"
gitdiffjc ()
{
    T='origin/master'
    if [ -n "$1" ]; then
        T="$1"
    fi
    CMD='git log --left-right --boundary --pretty="format:%C(auto)%m %h %<(14)%cr %<(20,trunc)%ae %d %s" ${T}...HEAD'
    echo $CMD
    eval $CMD
}
# git config --global format.pretty "format:%C(auto)%m %H %<(14)%cr %<(20,trunc)%ae %d %s "
# === END: git ===

export EDITOR=vi
if [[ ":$PATH:" != *":/sbin:"* ]]; then
    export PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
fi
if [[ ":$PATH:" != *":/usr/bin:"* ]]; then
    export PATH="$PATH:/usr/bin"
fi
if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
    export PATH="$PATH:$HOME/bin"
fi

# map gcal to cal3
type gcal > /dev/null 2>&1
if [ "$?" == "0"  ]; then
    alias cal3='gcal .'
else
    alias cal3='cal'
fi

# TODO: 2018-09-28 consider deprecating this
#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/bin:$PATH"
#if command -v pyenv 1>/dev/null 2>&1;
#then
#    eval "$(pyenv init -)"
#fi

if command -v colordiff 1>/dev/null 2>&1;
then
    alias diff='colordiff -wu'
fi

alias less='less -r'

test -f ~/.git-completion.bash && source ~/.git-completion.bash

jc.help() {
    declare -F | egrep "(\bjc|jc\b)"
}

echo "bashrc_jc.sh"
