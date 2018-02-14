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
jgdiff ()
{
    T='origin/master'
    if [ -n "$1" ]; then
        T="$1"
    fi
    git log --left-right --boundary --pretty="format:%C(auto)%m %h %<(14)%cr %d %s" ${T}...HEAD
}
jge ()
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
        emacsclient -n "$o"
    elif [ -n "$2" ]; then
        emacsclient -n `echo "$o"|sed "$2q;d"`
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

jf ()
{
  /usr/bin/find . -iname "*${1}.*"
}

jfe ()
{
  /usr/bin/find . -name "*.${1}"
}

# ssh
alias ssh="ssh -q"
alias skey='ssh-add ~/.ssh/jackrabbit_rsa'
jsa ()
{
    ssh-add -qL &> /dev/null
    if [ "$?" == "2" ]; then
        eval `ssh-agent`
    fi
    if [ -n "$1" ]; then
        ssh-add $1
    fi
}
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
jtitle ()
{
  if [ -n "$1" ]; then
     export jc_tab_title=$1
  fi
}
# /title management


echo "bashrc_jc.sh"
