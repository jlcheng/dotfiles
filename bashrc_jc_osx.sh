# Installation ---
#  echo ". ~/github/dotfiles/bashrc_jc_osx.sh" >> ~/.bashrc

export PATH='/usr/local/bin:/usr/local/sbin:/usr/local/share/python:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/Users/jcheng/.rvm/bin:/Users/jcheng/software/play-2:/Users/jcheng/bin:/Applications/Sublime Text 2.app/Contents/SharedSupport/bin'

# https://emacsformacosx.com/
if [ -d "/Applications/Emacs.app" ]; then 
  PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
  alias emacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
fi

export JAVA_HOME=`ls -d /Library/Java/JavaVirtualMachines/*/Contents/Home | tail -n 1`

echo "bashrc_jc_osx.sh"
