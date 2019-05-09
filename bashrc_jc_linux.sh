# === START: ssh ===
if [ -z "SSH_AUTH_SOCK" ]; then
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
fi
# === END: ssh ===

# === START: aws ===

# === END: aws ===
echo "bashrc_jc_linux.sh"
