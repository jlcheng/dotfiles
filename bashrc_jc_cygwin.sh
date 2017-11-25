# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc_cygwin.sh" >> ~/.bashrc

SSHAGENT="ssh-agent"
if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
    # use eval to take the output of sshagent and set SSH_AUTH_SOCK and SSH_AGENT_ID
    eval `$SSHAGENT`
    # kill ssh-agent on exit
    trap "kill $SSH_AGENT_PID" 0
fi
