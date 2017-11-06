# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILESIZE=500
export HISTIGNORE='&:ls:[bf]g:exit'
export IFS=$' \t\n'
export PATH='/usr/local/bin:/usr/local/sbin:/usr/local/share/python:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:'
export PS1='\w$ '
alias emacs="/usr/bin/emacsclient -n"
export EDITOR=vi

# export PIP_REQUIRE_VIRTUALENV=true
# export WORKON_HOME=$HOME/.virtualenvs
# export PROJECT_HOME=$HOME/Devel
# source /usr/local/share/python/virtualenvwrapper.sh
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home
# export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

f ()
{
  find . -iname "*${1}.*"
}

fe ()
{
    find . -name "*.${1}"
}

echo "bashrc_jc.sh"
