# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc_cygwin.sh" >> ~/.bashrc

# Go is either installed udner /d/ or /c/
if [ -d /d/Go/bin ]; then
    export PATH="$PATH:/d/Go/bin"
elif [ -d /c/Go/bin ]; then
    export PATH="$PATH:/c/Go/bin"
fi

if [ -d $HOME/bin ]; then
    export PATH="$PATH:$HOME/bin"
fi
if [ -d $HOME/software/hadoop/bin ]; then
    export PATH="$PATH:$HOME/software/hadoop/bin"
fi

if [ -d $HOME/software/spark/bin ]; then
    export PATH="$PATH:$HOME/software/spark/bin"
fi

if [ -d $HOME/software/spark/bin ]; then
    export PATH="$PATH:$HOME/software/sbt/bin"
fi

export JAVA_HOME=$HOME/software/jdk
export PATH="$JAVA_HOME/bin:$PATH"

# remove /x/Program Files/Git/Cmd from path so we use cygwin's git under cygin without affecting powershell
PATH=`echo -n ${PATH} | awk -v RS=: -v ORS=: '/Program\ Files\/Git\/cmd/ {next} {print'}`

# remove /x/WINDOWS/System32/OpenSSH from path to use cygwin ssh tools
PATH=`echo -n ${PATH} | awk -v RS=: -v ORS=: '/cygdrive\/c\/WINDOWS\/System32\/OpenSSH/ {next} {print'}`

# remove /x/WINDOWS/System32 from path to use cygwin tar and find
PATH=`echo -n ${PATH} | awk -v RS=: -v ORS=: '/cygdrive\/c\/WINDOWS\/system32/ {next} {print'}`

alias find=/usr/bin/find
export GIT_EDITOR="emacs-nox -Q"

# === START: ssh ===
SSH_TMP=~/.ssh-agent.tmp
if [ -f $SSH_TMP ]; then
    SSH_PID=`cat ~/.ssh-agent.tmp | sed  -rn 's/.+pid ([0-9]+);/\1/p'`
    if [ "$?" == "0" ] && [ -n "$SSH_PID" ]; then
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
# === END: ssh ===

# vagrant
jc.v.grs () {
    cd $HOME/privprjs/playground/vagrant-go
    echo "checking if VM is up..."
    status=$(vagrant status | egrep ^go\\b | awk '{print $2'})
    if [ $status == "running" ]; then
        echo "VM running. Connecting..."
        vagrant ssh
    else
	echo "VM not running. Running 'vagrant up'..."
        vagrant up && vagrant ssh
    fi
}

echo "bashrc_jc_cygwin.sh"
