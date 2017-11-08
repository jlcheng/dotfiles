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
export EDITOR=vi

f ()
{
  find . -iname "*${1}.*"
}

fe ()
{
    find . -name "*.${1}"
}

echo "bashrc_jc.sh"
