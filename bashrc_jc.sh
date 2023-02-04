# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc.sh" >> ~/.bashrc

export HISTCONTROL=ignoredups
export HISTFILE=$HOME/.bash_history
export HISTIGNORE='&:ls:[bf]g:exit'
export HISTSIZE=100000
export HISTFILESIZE=100000
export PATH="/usr/bin:$PATH:$HOME/go/bin"
export GOPATH=$HOME/go

# from a combination of http://tldp.org/HOWTO/Xterm-Title-4.html 
#                   and http://bashrcgenerator.com/ (2018-11-02)
export PS1="\[\033[38;5;11m\]\h\[$(tput sgr0)\]: \[$(tput sgr0)\]\[\033[38;5;10m\]\W\[$(tput sgr0)\]\n\\$ \[$(tput sgr0)\]"
# from https://github.com/magicmonty/bash-git-prompt
if [ -f "$HOME/.bash-git-prompt/gitprompt.sh" ]; then
    GIT_PROMPT_ONLY_IN_REPO=1
    source $HOME/.bash-git-prompt/gitprompt.sh
fi

if [ "$USER" != "vagrant" ] && [ ! -f /etc/ec2_version ] ; then
  alias emacs="emacsclient -n"
  alias emacs-start="/usr/bin/emacs &> /dev/null &"
fi

# === START: git ===
export GIT_EDITOR="emacsclient"
gitdiffjc ()
{
    T='origin/master'
    if [ -n "$1" ]; then
        T="$1"
    fi
    CMD='git log --left-right --boundary --pretty="format:%C(auto)%m %h %<(14)%cr %<(20,trunc)%ae %d %s" ${T}...HEAD'
    echo $CMD
    eval $CMD
}

jc.gco()
{
    git checkout $(git branch --format="%(refname:lstrip=2)" | fzf)
}

jc.cd()
{
    cd $(ls | fzf)
}


jc.gm()
{
    git merge $(git branch --format="%(refname:lstrip=2)" | fzf)
}
# git config --global pretty.j "format:%C(auto)%h %<(14)%cr %<(20,trunc)%ae %s%d"
# git config --global format.pretty "format:%C(auto)%h %<(14)%cr %<(20,trunc)%ae %s%d"
# === END: git ===

# === START: fzf ===
if [ -f "$HOME/pubprjs/fzf/shell/completion.bash" ]; then
    source $HOME/pubprjs/fzf/shell/completion.bash
    source $HOME/pubprjs/fzf/shell/key-bindings.bash
fi
# === END: fzf ===

export EDITOR=vi
if [[ ":$PATH:" != *":/sbin:"* ]]; then
    export PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
fi
if [[ ":$PATH:" != *":/usr/bin:"* ]]; then
    export PATH="$PATH:/usr/bin"
fi
if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
    export PATH="$PATH:$HOME/bin"
fi

# map gcal to cal3
type gcal > /dev/null 2>&1
if [ "$?" == "0"  ]; then
    alias cal3='gcal .'
else
    alias cal3='cal'
fi

if command -v colordiff 1>/dev/null 2>&1;
then
    alias diff='colordiff -wu'
fi

alias less='less -r'
alias r4='4gt qc'
alias tma='tmux attach-session -d'
alias rt='[[ -f rt.py ]] && ./rt.py'

git_completion=~/.git-completion.bash
if [[ -f $git_completion ]]; then
    source ~/.git-completion.bash
elif command -v curl 1>/dev/null 2>&1; then
    echo "installing git-completion.bash"
    curl https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o $git_completion
    source $git_completion
fi

jc.help() {
    declare -F | egrep "(\bjc|jc\b)"
}

jc.e() {
    pc=cat
    which bat > dev/null
    if [[ $? == 0 ]]; then
        pc="bat --color=always"
    fi
    
    which fzf > /dev/null
    if [[ $? == 0 ]]; then
        emacs $(fzf --preview "$pc {}")
    else
        echo "fzf is not installed."
    fi 
}

# === START: privmono ===
if [[ -f $HOME/privprjs/privmono/bash/venv.sh ]]; then
    source $HOME/privprjs/privmono/bash/venv.sh
fi

export APP_CONFIG_TOML=$HOME/.privmono.d/local.toml

# === END: privmono ===


# === START: pyenv ===
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
# === END: pyenv ===

echo "bashrc_jc.sh"
