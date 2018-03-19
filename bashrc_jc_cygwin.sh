# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc_cygwin.sh" >> ~/.bashrc

# Go
if [ -d /d/Go/bin ]; then
    export PATH="$PATH:/d/Go/bin"
elif [ -d /c/Go/bin ]; then
    export PATH="$PATH:/c/Go/bin"
fi

if [ -d /progfiles/Git ]; then
    export PATH="$PATH:/progfiles/Git/cmd"
fi

if [ -d $HOME/bin ]; then
    export PATH="$PATH:$HOME/bin"
fi

alias find=/usr/bin/find
export GIT_EDITOR="emacs-nox"

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

echo "bashrc_jc_cygwin.sh"
