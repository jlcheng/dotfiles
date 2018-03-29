# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc_cygwin.sh" >> ~/.bashrc

# Go is either installed udner /d/ or /c/
if [ -d /d/Go/bin ]; then
    export PATH="$PATH:/d/Go/bin"
elif [ -d /c/Go/bin ]; then
    export PATH="$PATH:/c/Go/bin"
fi

if [ -d $HOME/bin ]; then
    export PATH="$PATH:$HOME/bin"
fi

# remove /x/Progrma Files/Git/Cmd from path so we use cygwin's git under cygin without affecting powershell
PATH=`echo -n ${PATH} | awk -v RS=: -v ORS=: '/Program\ Files\/Git\/cmd/ {next} {print'}`

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
