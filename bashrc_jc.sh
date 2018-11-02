# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTIGNORE='&:ls:[bf]g:exit'
export PATH="/usr/bin:$PATH:$HOME/go/bin"
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
alias emacs="emacsclient -n"
alias emacs-start="/usr/bin/emacs &> /dev/null &"

# git
export GIT_EDITOR="emacs -nw -q"
gitdiffjc ()
{
    T='origin/master'
    if [ -n "$1" ]; then
        T="$1"
    fi
    git log --left-right --boundary --pretty="format:%C(auto)%m %h %<(14)%cr %d %s" ${T}...HEAD
}
gitopenjc ()
{
    if [ -z "$1" ]; then
        echo "usage: jge <file>"
        return
    fi
    target="$1"
    o=`git ls-files | grep $target`
    if [ "$?" != 0 ]; then
        echo "$o"
        return
    fi
    if [ `echo "$o" | wc -l` -eq 1 ]; then
        $GIT_EDITOR "$o"
    elif [ -n "$2" ]; then
        $GIT_EDITOR `echo "$o"|sed "$2q;d"`
    else
        echo "multiple candidates:"
        echo "$o" | awk '{print NR":", $0}'
    fi
}
# /git

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
jf ()
{
  /usr/bin/find . -iname "*${1}.*"
}

jfe ()
{
  /usr/bin/find . -name "*.${1}"
}

# map gcal to cal3
type gcal > /dev/null 2>&1
if [ "$?" == "0"  ]; then
    alias cal3='gcal .'
else
    alias cal3='cal'
fi

# TODO: 2018-09-28 consider deprecating this
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1;
then
    eval "$(pyenv init -)"
fi

if command -v colordiff 1>/dev/null 2>&1;
then
    alias diff='colordiff -wu'
fi

source ~/.git-completion.bash

echo "bashrc_jc.sh"
