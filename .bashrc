# load `git status`-enabled prompt
[[ -s $HOME/.bash_scripts/bashprompt_git_status.sh ]] && source "$HOME/.bash_scripts/bashprompt_git_status.sh"

# load Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source "$HOME/.rvm/scripts/rvm"

# load Node.js Version Manager
[[ -s $HOME/.nvm/nvm.sh ]] && source "$HOME/.nvm/nvm.sh"

# enable bash completion in interactive shells
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

alias ls="ls -Gp"
alias grep="grep --color=auto"
alias top="htop"
alias ec="emacsclient -t"

export TERM="screen-256color"
export EDITOR="emacsclient -t"

<<<<<<< HEAD
export PATH="$HOME/.bin:$PATH"
=======
export PATH="$HOME/.bin:/usr/local/bin:$PATH"
>>>>>>> a31a14462c0a10d1b824a4e3d616ec1f022398a8
