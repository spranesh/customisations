#======================================================================
# Aliases
#======================================================================
# append default options to the following commands
alias man='LC_ALL=C LANG=C man'
alias cp="cp -v"
alias mv="mv -v"
alias grep="grep --color -n"
alias tar="tar -k"

# Create short forms for ls
alias ls="ls --color"
alias l='ls -lFh' #size,show type,human readable
alias la='ls -lAFh' #long list,show almost all,show type,human readable
alias lr='ls -tRFh' #sorted by date,recursive,show type,human readable
alias lt='ls -ltFh' #long list,sorted by date,show type,human readable
alias ll='ls -l' #long list
alias ldot='ls -ld .*'
alias lS='ls -1FSsh'
alias lart='ls -1Fcart'
alias lrt='ls -1Fcrt'

#Some shortforms
alias ack="ack-grep"
alias hgrep="fc -El 0 | grep"
alias j='jobs'
alias p='ps -f'
alias v="vim"
alias gv="gvim"
alias git_turn_off_pre_commit_hooks="sudo chmod a-x .git/hooks/pre-commit"

alias combine_pdf="gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=bind.pdf"
alias skype_v4l="LD_PRELOAD=/usr/lib/libv4l/v4l1compat.so skype" 
alias decrypt="openssl enc -d -aes-256-cbc -a"
alias usbmode='sudo usb_modeswitch -c /etc/usb_modeswitch.d/12d1:1446'
alias keeptrack='sudo checkinstall -D --fstrans=no'

#======================================================================
# Functions
#======================================================================
function gvim()
{
    if [ "$1" ] ; then
        /usr/bin/env gvim --remote-silent $*
    else
        /usr/bin/env gvim $*
    fi
}

extract () {
  if [ -f $1 ]; then
    case $1 in
      *.tar.bz2)  tar -jxvf $1        ;;
      *.tar.gz)   tar -zxvf $1        ;;
      *.bz2)      bzip2 -d $1         ;;
      *.gz)       gunzip -d $1        ;;
      *.tar)      tar -xvf $1         ;;
      *.tgz)      tar -zxvf $1        ;;
      *.zip)      unzip $1            ;;
      *.Z)        uncompress $1       ;;
      *.rar)      unrar x $1            ;;
      *)          echo "'$1' Error. Please go away" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}


