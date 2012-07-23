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
export HISTSIZE=1000000
export HISTFILESIZE=1000000

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
    local pwdmaxlen=30
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


function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/:\1$(parse_git_dirty)/"
}

git_info() {
  # colors
  local     RESET="\033[0m"
  local      GRAY="\033[1;30m"
  local       RED="\033[1;31m"
  local     GREEN="\033[1;32m"
  local      BLUE="\033[1;34m"
  local      PINK="\033[1;35m"
  local      CYAN="\033[1;36m"

  local      GOOD="\xe2\x88\x99"              # ∙   circle
  local     AHEAD="\xe2\x86\x91"              # ↑   up arrow
  local    BEHIND="\xe2\x86\x93"              # ↓   down arrow
  local   CHANGED="\xcf\x9f"                  # ϟ   lightning
  local  DIVERGED="\xe2\x86\x95"              # ↕   up-down
  local UNTRACKED="\xe0\xb2\xa0_\xe0\xb2\xa0" # ಠ_ಠ disapproval

  git_place() {
    local place=$(git status 2> /dev/null | head -n2 | tail -n1)
    if [[ $( echo $place | grep "Your branch is ahead of" ) != "" ]]; then
      echo -e "$PINK$AHEAD"
    elif [[ $( echo $place | grep "Your branch is behind" ) != "" ]]; then
      echo -e "$BLUE$BEHIND"
    elif [[ $( echo $place | grep "Your branch and .* have diverged" ) != "" ]]; then
      echo -e "$CYAN$DIVERGED"
    else
      echo -e "$GREEN$GOOD"
    fi
  }

  git_status() {
    local status=$(git status 2> /dev/null | tail -n1)
    if [[ $status != "nothing to commit (working directory clean)" ]]; then
      if [[ $status == 'nothing added to commit but untracked files present (use "git add" to track)' ]]; then
        echo -e "$GRAY$UNTRACKED"
      else
        echo -e "$RED$CHANGED"
      fi
    fi
  }

  local branch=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/")

  if [[ $branch != "" ]]; then
    echo -e ":$branch$(git_place)$(git_status)$RESET"
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
    # PS1="[$TITLEBAR${EMG}\u${EMC}@${EMB}\h ${EMC}\${NEW_PWD}${EMY}\$(parse_git_branch)${UC}]\n\\$ ${NONE}"
    PS1="[$TITLEBAR${EMG}\u${EMC}@${EMB}\h ${EMC}\${NEW_PWD}${EMY}\$(git_info)${UC}]\n\\$ ${NONE}"
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



# Environment variables
export EDITOR='vim'
export ACK_OPTIONS=--type-add=php=.php,.module,.inc,.install:--type-add=java=.groovy:--type-add=html=.gsp

# Aliases
alias pym='python manage.py'
alias dj='django-admin.py'
alias djgraph='django-admin.py graph_models -a -g -o'
alias delpyc='find . -iname \*pyc -delete'
alias webshare='python -m "SimpleHTTPServer 8080"'
alias pretty-json='python -mjson.tool'
alias v='vagrant'

# Find a file with a pattern in name:
function ff() { find . -type f -iname '*'$*'*' -ls ; }

# adds ~/.ssh/config to the ssh autocomplete
ssh_load_autocomplete()
{
	complete -W "$(awk '/^\s*Host\s*/ { sub(/^\s*Host /, ""); print; }' ~/.ssh/config)" ssh
}

# adds ~/.ssh/config to the ssh autocomplete
ssh_load_autocomplete

cdp () {
    if [ ! -n "$1" ]
    then
        echo "Usage: cdp modulename"
    else
        cd "$(python -c "import os.path as _, ${1}; print _.dirname(_.realpath(${1}.__file__[:-1]))")"
    fi
}

