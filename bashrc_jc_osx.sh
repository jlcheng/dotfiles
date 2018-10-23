# Installation ---
#  echo ". ~/privprjs/dotfiles/bashrc_jc_osx.sh" >> ~/.bashrc

export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/python:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/Users/jcheng/.rvm/bin:/Users/jcheng/software/play-2:/Users/jcheng/bin:/Applications/Sublime Text 2.app/Contents/SharedSupport/bin"
export PATH="$PATH:/usr/local/go/bin"


# https://emacsformacosx.com/
if [ -d "/Applications/Emacs.app" ]; then 
  PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
  alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
fi

export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_172.jdk/Contents/Home"

# 1) force ls to have color; 2) optimize lscolors for black background
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

echo "bashrc_jc_osx.sh"
