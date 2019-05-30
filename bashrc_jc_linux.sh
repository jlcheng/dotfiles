# === START: ssh ===
jc.ssh.sock() {
    socket_file=~/.ssh/agent_socket_tmp    
    # If $SSH_AUTH_SOCK is not set, try to find it within $socket_file
    if [[ -z "SSH_AUTH_SOCK" ]]; then
	if [[ -f $socket_file ]]; then
	    . $socket_file
	fi
    fi

    do_agent=0
    # Now we have a $SSH_AUTH_SOCK that is possibly invalid
    if [[ ! -S $SSH_AUTH_SOCK ]] ; then

    fi
}
# === END: ssh ===

# === START: aws ===

# === END: aws ===
echo "bashrc_jc_linux.sh"
