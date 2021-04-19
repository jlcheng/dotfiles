# === START: ssh ===
# If $SSH_AUTH_SOCK is not set, create a new agent
if [[ -z $SSH_AUTH_SOCK ]] ; then
    eval `ssh-agent -s`
    trap 'test -n "$SSH_AGENT_PID" && eval `/usr/bin/ssh-agent -k`' 0
    return
fi
# === END: ssh ===

# === START: kde ===
if [[ -f /usr/bin/kde-open5 ]]; then
    alias open=/usr/bin/kde-open5
else
    alias open=false
fi
# === END: kde ===

# === START: terraform ===
# Setting TF_DATA_DIR is a bad idea. It forces all terraform workspaces to use the same terraform version
# in terraform.required_version file
# export TF_DATA_DIR=$HOME/.terraform/v12
# === END: terraform ===


echo "bashrc_jc_linux.sh"
