# load `git status`-enabled prompt
[[ -s $HOME/.bash_scripts/bashprompt_git_status.sh ]] && source "$HOME/.bash_scripts/bashprompt_git_status.sh"

# load Ruby Version Manager
[[ -s $HOME/.rvm/scripts/rvm ]] && source "$HOME/.rvm/scripts/rvm"

# load Node.js Version Manager
[[ $OSTYPE == darwin* ]] && [[ -f $(brew --prefix nvm)/nvm.sh ]] && source "$(brew --prefix nvm)/nvm.sh"

# load bash completion in interactive shells
[[ $OSTYPE == darwin* ]] && [[ -f $(brew --prefix)/etc/bash_completion ]] && . $(brew --prefix)/etc/bash_completion

# Command Line aliases
alias ln="ln -v"
[[ $OSTYPE == darwin* ]]   && alias ls="ls -Gp"
[[ $OSTYPE == linux-gnu ]] && alias ls="ls --color=auto -p"
alias grep="grep --color=auto"
alias  top="htop"
alias   ec="emacsclient -t"

# Bundler
alias be="bundle exec"

export       TERM="screen-256color"
export     EDITOR="emacsclient -t"
export GIT_EDITOR="emacsclient -t"
export  HG_EDITOR="emacsclient -t"

export  CHROME_BIN=`which chromium-browser`
export FIREFOX_BIN=`which firefox`

export CABAL_SANDBOX_PATH="./.cabal-sandbox/bin"
export         CABAL_PATH="$HOME/.cabal/bin"
export           RVM_PATH="$HOME/.rvm/bin"
export    NODE_LOCAL_PATH="./node_modules/.bin"
export         LANGS_PATH="$CABAL_SANDBOX_PATH:$CABAL_PATH:$NPM_LOCAL_PATH:$RVM_PATH"

export PATH="$LANGS_PATH:$HOME/.bin:/opt/bin:/usr/local/sbin:/usr/local/bin:$PATH"
