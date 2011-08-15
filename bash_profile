# Check for an interactive session
[ -z "$PS1" ] && return

# Ignore these commands
export HISTIGNORE="&:ls:[bf]g:exit:pwd:[ \t]:screen"
# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth
# increase history size
export HISTSIZE=100000
export HISTFILESIZE=100000

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize


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

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

_virtualenvs ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=( $(compgen -W "`ls $WORKON_HOME`" -- ${cur}) )
}

complete -o default -o nospace -F _virtualenvs workon
complete -o default -o nospace -F _virtualenvs rmvirtualenv


alias pym='python manage.py'
alias pyd='../../bin/django'
pyml ()
{
  python manage.py $1 --settings=settings_local
}
alias djgraph='python manage.py graph_models -a -g -o'

export EDITOR='vim'
alias n='nautilus'

alias delpyc='find . -iname \*pyc -delete'

export PATH=$PATH:$HOME/bin:/usr/local/mysql/bin:$HOME/.gems/bin/
export GEM_HOME=$HOME/.gems

# Find a file with a pattern in name:
function ff() { find . -type f -iname '*'$*'*' -ls ; }

# flex
export FLEX_HOME=/opt/adobe-flex-sdk
export PATH=$PATH:$FLEX_HOME/bin

# groovy
export GROOVY_HOME=/opt/groovy/
export PATH=$PATH:$GROOVY_HOME/bin
#grails
export GRAILS_HOME=$HOME/packages/grails-1.2.1
export PATH=$PATH:$GRAILS_HOME/bin

export PATH=/usr/lib/cw:/usr/local/bin/:$HOME/.gem/ruby/1.9.1/bin/:$PATH

export ACK_OPTIONS=--type-add=php=.php,.module,.inc,.install:--type-add=java=.groovy:--type-add=html=.gsp

_grailsscripts() {
    SCRIPT_DIRS="$GRAILS_HOME/scripts ./scripts ~/.grails/scripts"
    if [ -d plugins ]
       then for PLUGIN_DIR in $(ls -d plugins/*/scripts 2> /dev/null); do
       SCRIPT_DIRS="$SCRIPT_DIRS $PLUGIN_DIR"
       done
    fi
    for D in $SCRIPT_DIRS; do
        if [ -d $D ]
	   then ls -1 $D/*.groovy 2> /dev/null | sed -E 's/(.*)\/(.*)\.groovy/\2/' | sed -E 's/([A-Z])/-\1/g' | sed -E 's/^-//' | tr "[:upper:]" "[:lower:]"
	fi
    done | sort | uniq | grep -vE "^_"
}

_grails() {
    COMPREPLY=( $(compgen -W "$(_grailsscripts)" -- ${COMP_WORDS[COMP_CWORD]}) )
}

complete -F _grails grails

export VDT_HOME=$HOME/projects/vagrant
##
# Your previous /Users/alfredoaguirre/.bash_profile file was backed up as /Users/alfredoaguirre/.bash_profile.macports-saved_2011-08-10_at_17:27:24
##

# MacPorts Installer addition on 2011-08-10_at_17:27:24: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

source $HOME/configs/git-flow-completion.bash