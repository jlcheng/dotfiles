# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTIGNORE='&:ls:[bf]g:exit'
export IFS=$' \t\n'

alias emacs="emacsclient -n"
alias emacs-start="/usr/bin/emacs &> /dev/null &"

export PS1="\W\\$ " # default prompt

# git
export GIT_EDITOR="emacs"
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
if [[ ":$PATH:" != *":~/bin:"* ]]; then
    export PATH="$PATH:~/bin"
fi

jf ()
{
  /usr/bin/find . -iname "*${1}.*"
}

jfe ()
{
  /usr/bin/find . -name "*.${1}"
}

# title_management
# jc_tab_max: controls the max length of a tab title
# jc_tab_title <name>: appends a custom label to the tab
export jc_tab_max=20
PROMPT_COMMAND='
if [ -n "$jc_tab_title" ]; then
  title__="$PWD ($jc_tab_title)"
  pstitle__="\[$(tput setaf 6)\]$jc_tab_title\[$(tput setaf 7)\]: \W"
else
  title__=$PWD
  pstitle__="\W"
fi
if [ ${#title__} -gt $jc_tab_max ]; then
  title__="...`echo $title__ | tail -c $jc_tab_max`"
fi
echo -n -e "\033]0;$title__\007"
export PS1="\[$(tput bold)\][$pstitle__]\\$ \[$(tput sgr0)\]"
'
titlejc ()
{
  if [ -n "$1" ]; then
     export jc_tab_title=$1
  fi
}
# /title management

# map gcal to cal3
type gcal > /dev/null 2>&1
if [ "$?" == "0"  ]; then
    alias cal3='gcal .'
else
    alias cal3='cal'
fi

echo "bashrc_jc.sh"
