# === START: ssh ===
# If $SSH_AUTH_SOCK is not set, create a new agent
if [[ -z $SSH_AUTH_SOCK ]] ; then
    eval `ssh-agent -s`
    trap 'test -n "$SSH_AGENT_PID" && eval `/usr/bin/ssh-agent -k`' 0
    return
fi
# === END: ssh ===


echo "bashrc_jc_linux.sh"
