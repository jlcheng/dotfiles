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

echo "bashrc_jc_cygwin.sh"
