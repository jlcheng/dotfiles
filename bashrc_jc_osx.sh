# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc_osx.sh" >> ~/.bashrc

export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"


# https://emacsformacosx.com/
if [ -d "/Applications/Emacs.app" ]; then 
  PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
  alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
  GIT_EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -q -a vim"
fi

if [ -d "/opt/homebrew/bin" ]; then
    PATH=$PATH:/opt/homebrew/bin
fi    

# 1) force ls to have color; 2) optimize lscolors for black background
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

echo "bashrc_jc_osx.sh"

