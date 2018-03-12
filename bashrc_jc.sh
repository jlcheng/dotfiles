# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTIGNORE='&:ls:[bf]g:exit'
export IFS=$' \t\n'
export PS1="\w\\$ " # don't colorize this, it screws up ctrl-r command history
alias emacs="emacsclient -n"
alias emacs-start="/usr/bin/emacs &> /dev/null &"

# git
export GIT_EDITOR="emacs"
gitdiffj ()
{
    T='origin/master'
    if [ -n "$1" ]; then
        T="$1"
    fi
    git log --left-right --boundary --pretty="format:%C(auto)%m %h %<(14)%cr %d %s" ${T}...HEAD
}
gitopenj ()
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

# ssh
SSH_TMP=~/.ssh-agent.tmp
if [ -f $SSH_TMP ]; then
    SSH_PID=`cat ~/.ssh-agent.tmp | sed  -rn 's/.+pid ([0-9]+);/\1/p'`
    if [ "$?" == "0" ]; then
        ps -p $SSH_PID > /dev/null
        if [ "$?" == "0" ]; then
            . $SSH_TMP
        fi
    fi
fi
if [ -z "$SSH_AGENT_PID" ]; then
    ssh-agent > $SSH_TMP
    . $SSH_TMP
fi
alias ssh="ssh -q"
# /ssh

# title_management
# jc_tab_max: controls the max length of a tab title
# jc_tab_title <name>: appends a custom label to the tab
export jc_tab_max=20
PROMPT_COMMAND='
if [ -n "$jc_tab_title" ]; then
  title__="$PWD ($jc_tab_title)"
else
  title__=$PWD
fi
if [ ${#title__} -gt $jc_tab_max ]; then
  title__="...`echo $title__ | tail -c $jc_tab_max`"
fi
echo -n -e "\033]0;$title__\007"
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
