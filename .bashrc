# load `git status`-enabled prompt
[[ -s $HOME/.bash_scripts/bashprompt_git_status.sh ]] && source "$HOME/.bash_scripts/bashprompt_git_status.sh"

# load Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source "$HOME/.rvm/scripts/rvm"

# load Node.js Version Manager
[[ -s $HOME/.nvm/nvm.sh ]] && source "$HOME/.nvm/nvm.sh"

# load bash completion in interactive shells
[[ $OSTYPE == darwin* ]] && [[ -f $(brew --prefix)/etc/bash_completion ]] && . $(brew --prefix)/etc/bash_completion

# Command Line aliases
alias ln="ln -v"
alias ls="ls --color=auto -Gp"
alias grep="grep --color=auto"
alias top="htop"
alias ec="emacsclient -t"

# Bundler
alias be="bundle exec"

export TERM="screen-256color"
export EDITOR="emacsclient -t"

export DISTUTILSPATH="/usr/local/share/python"
export PATH="$HOME/.bin:/usr/local/bin:$DISTUTILSPATH:$PATH"

# load Python virtualenvwrapper
export WORKON_HOME="$HOME/.virtualenv"
[[ $OSTYPE == darwin* ]] && [[ -f $(brew --prefix)/share/python/virtualenvwrapper.sh ]] && . $(brew --prefix)/share/python/virtualenvwrapper.sh
[[ $OSTYPE == linux-gnu ]] && [[ -f /usr/local/bin/virtualenvwrapper.sh ]] && . /usr/local/bin/virtualenvwrapper.sh
