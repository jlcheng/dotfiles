# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTFILESIZE=500
export HISTIGNORE='&:ls:[bf]g:exit'
export IFS=$' \t\n'
export PATH='/usr/local/bin:/usr/local/sbin:/usr/local/share/python:/usr/bin:/bin:/usr/sbin:/sbin'
export PS1='\w$ '
alias emacs="emacsclient -n"
alias emacs-start="/usr/bin/emacs &> /dev/null &"
export EDITOR=vi

f ()
{
  find . -iname "*${1}.*"
}

fe ()
{
    find . -name "*.${1}"
}

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
jc_tab_title()
{
  if [ -n "$1" ]; then
     export jc_tab_title=$1
  fi
}
# /title management

SSHAGENT="ssh-agent"
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
    # use eval to take the output of sshagent and set SSH_AUTH_SOCK and SSH_AGENT_ID
    eval `$SSHAGENT`
fi

echo "bashrc_jc.sh"
