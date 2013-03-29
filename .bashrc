[[ -s "$HOME/.bash_scripts/bashprompt_git_status.sh" ]] && source "$HOME/.bash_scripts/bashprompt_git_status.sh"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "$HOME/.nvm/nvm.sh" ]] && source "$HOME/.nvm/nvm.sh"
[[ -f $(brew --prefix)/etc/bash_completion ]] && . $(brew --prefix)/etc/bash_completion

alias ls="ls -Gp"
alias grep="grep --color=auto"
alias top="htop"
alias ec="emacsclient -t"

export TERM="screen-256color"
export EDITOR="emacsclient -t"

export PATH="$HOME/.bin:/usr/local/bin:$PATH"
