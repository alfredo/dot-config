# common stuff
source $HOME/projects/configs/common.bash

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

_virtualenvs ()
{
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=( $(compgen -W "`ls $WORKON_HOME`" -- ${cur}) )
}

complete -o default -o nospace -F _virtualenvs workon
complete -o default -o nospace -F _virtualenvs rmvirtualenv

export GEM_HOME=$HOME/.gems

export VDT_HOME=$HOME/projects/vagrant

export PATH=/opt/local/bin:/opt/local/sbin:$PATH

export PATH=/usr/local/bin:$HOME/projects/local-bin:$PATH

source $HOME/projects/configs/git-flow-completion.bash

alias ls="ls -G"
export CLICOLORS=1
export LSCOLORS=dxfxcxdxbxegedabagacad
# export LSCOLORS=GxFxCxDxBxegedabagaced
alias clean_dir='find . -name ".DS_Store" -depth -exec rm {} \;'a


export VISUAL=vim