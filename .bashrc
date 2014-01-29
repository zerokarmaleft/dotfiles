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
[[ $OSTYPE == darwin* ]]   && alias ls="ls -Gp"
[[ $OSTYPE == linux-gnu ]] && alias ls="ls --color=auto -p"
alias grep="grep --color=auto"
alias top="htop"
alias ec="emacsclient -t"

# Bundler
alias be="bundle exec"

export TERM="screen-256color"
export EDITOR="emacsclient -t"
export GIT_EDITOR="emacsclient -t"
export HG_EDITOR="emacsclient -t"

export CHROME_BIN=`which chromium-browser`
export FIREFOX_BIN=`which firefox`

export CABAL_PATH="$HOME/.cabal/bin"

export PLAY_VERSION="2.2.1"
export PLAY_PATH="$HOME/.bin/play-$PLAY_VERSION"

export RVMPATH="$HOME/.rvm/bin"

export DISTUTILSPATH="/usr/local/share/python"

export LOCALNODEBIN_PATH="./node_modules/.bin"

export PATH="$HOME/.bin:$CABAL_PATH:/usr/local/sbin:/usr/local/bin:$LOCALNODEBIN_PATH:$RVMPATH:$DISTUTILSPATH:$PLAY_PATH:$PATH"

# load Python virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON="$(brew --prefix)/bin/python3"
export WORKON_HOME="$HOME/.virtualenvs"
. $(which virtualenvwrapper.sh)

