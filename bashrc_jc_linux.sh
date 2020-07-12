# === START: ssh ===
# If $SSH_AUTH_SOCK is not set, create a new agent
if [[ -z $SSH_AUTH_SOCK ]] ; then
    eval `ssh-agent -s`
    trap 'test -n "$SSH_AGENT_PID" && eval `/usr/bin/ssh-agent -k`' 0
    return
fi
# === END: ssh ===

# === START: terraform ===
# Setting TF_DATA_DIR is an experiment. The documentation suggests this is not needed
# but it will keep the size of my repositories down
export TF_DATA_DIR=$HOME/.terraform/v12
# === END: terraform ===

echo "bashrc_jc_linux.sh"
