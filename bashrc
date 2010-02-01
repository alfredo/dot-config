
# Check for an interactive session
[ -z "$PS1" ] && return


#PS1='[\u@\h \W]\$ '


# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
#export HISTCONTROL=ignoreboth
# increase history size
export HISTSIZE=10000
export HISTFILESIZE=10000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set a fancy prompt (non-color, unless we know we "want" color)
#case "$TERM" in
#    xterm-color) color_prompt=yes;;
#esac



##################################################
# Fancy PWD display function
##################################################
# The home directory (HOME) is replaced with a ~
# The last pwdmaxlen characters of the PWD are displayed
# Leading partial directory names are striped off
# /home/me/stuff          -> ~/stuff               if USER=me
# /usr/share/big_dir_name -> ../share/big_dir_name if pwdmaxlen=20
##################################################
bash_prompt_command() {
    # How many characters of the $PWD should be kept
    local pwdmaxlen=25
    # Indicate that there has been dir truncation
    local trunc_symbol=".."
    local dir=${PWD##*/}
    pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
    NEW_PWD=${PWD/#$HOME/\~}
    local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
    if [ ${pwdoffset} -gt "0" ]
    then
        NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
        NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
    fi
}

bash_prompt() {
    case $TERM in
     xterm*|rxvt*)
         local TITLEBAR='\[\033]0;\h: \W\007\]'
          ;;
     *)
         local TITLEBAR=""
          ;;
    esac
    local NONE="\[\033[0m\]"    # unsets color to term's fg color
    
    # regular colors
    local K="\[\033[0;30m\]"    # black
    local R="\[\033[0;31m\]"    # red
    local G="\[\033[0;32m\]"    # green
    local Y="\[\033[0;33m\]"    # yellow
    local B="\[\033[0;34m\]"    # blue
    local M="\[\033[0;35m\]"    # magenta
    local C="\[\033[0;36m\]"    # cyan
    local W="\[\033[0;37m\]"    # white
    
    # emphasized (bolded) colors
    local EMK="\[\033[1;30m\]"
    local EMR="\[\033[1;31m\]"
    local EMG="\[\033[1;32m\]"
    local EMY="\[\033[1;33m\]"
    local EMB="\[\033[1;34m\]"
    local EMM="\[\033[1;35m\]"
    local EMC="\[\033[1;36m\]"
    local EMW="\[\033[1;37m\]"
    
    # background colors
    local BGK="\[\033[40m\]"
    local BGR="\[\033[41m\]"
    local BGG="\[\033[42m\]"
    local BGY="\[\033[43m\]"
    local BGB="\[\033[44m\]"
    local BGM="\[\033[45m\]"
    local BGC="\[\033[46m\]"
    local BGW="\[\033[47m\]"
    
    local UC=$W                 # user's color
    [ $UID -eq "0" ] && UC=$R   # root's color
    
    PS1="$TITLEBAR${W}[${EMG}\u${EMC}@${EMB}\h${EMC}\${NEW_PWD}${W}]${UC}\\$ ${NONE}"
    # without colors: PS1="[\u@\h \${NEW_PWD}]\\$ "
    # extra backslash in front of \$ to make bash colorize the prompt
}

PROMPT_COMMAND=bash_prompt_command
bash_prompt
unset bash_prompt

set show-all-if-ambiguous on

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

alias ll="ls -lh"
alias la="ls -a"
alias exit="clear; exit"

# Lets you search through all available packages simply using 'pacsearch packagename'
alias pacsearch="pacman -Sl | cut -d' ' -f2 | grep "

# sudo pacman -Syu by typing pacup (sudo must be installed and configured first)
alias pacup="sudo pacman -Syu"

# sudo pacman -S by typing pac
alias pac="sudo pacman -S"


# colorized pacman output with pacs alias:
alias pacs="pacsearch"
pacsearch() {
   echo -e "$(pacman -Ss "$@" | sed \
     -e 's#^core/.*#\\033[1;31m&\\033[0;37m#g' \
     -e 's#^extra/.*#\\033[0;32m&\\033[0;37m#g' \
     -e 's#^community/.*#\\033[1;35m&\\033[0;37m#g' \
     -e 's#^.*/.* [0-9].*#\\033[0;36m&\\033[0;37m#g' ) \
     \033[0m"
}

alias y='yaourt'
alias p='pacman'

export WORKON_HOME=$HOME/.virtualenvs
source /usr/bin/virtualenvwrapper_bashrc


_virtualenvs ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=( $(compgen -W "`ls $WORKON_HOME`" -- ${cur}) )
}
alias ls='ls --color=auto'
complete -o default -o nospace -F _virtualenvs workon
complete -o default -o nospace -F _virtualenvs rmvirtualenv


alias pym='python manage.py'

export EDITOR=emacs
alias n='nautilus'

alias delpyc='find . -iname \*pyc -delete'

export PATH=$PATH:$HOME/.gem/ruby/1.9.1/bin:$HOME/bin

alias ec="emacsclient -t"
alias en="emacs -nw"
