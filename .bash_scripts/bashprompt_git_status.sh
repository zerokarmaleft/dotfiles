        RED="\[\033[0;31m\]"
     YELLOW="\[\033[0;33m\]"
      GREEN="\[\033[0;32m\]"
       BLUE="\[\033[0;34m\]"
  LIGHT_RED="\[\033[1;31m\]"
LIGHT_GREEN="\[\033[1;32m\]"
      WHITE="\[\033[1;37m\]"
 LIGHT_GRAY="\[\033[0;37m\]"
 LIGHT_BLUE="\[\033[1;36m\]"
 COLOR_NONE="\[\e[0m\]"

  CHAR_BOLT=`echo -e "\xe2\x9a\xa1"`
    CHAR_UP=`echo -e "\xe2\x86\x91"`
  CHAR_DOWN=`echo -e "\xe2\x86\x93"`
   CHAR_DIV=`echo -e "\xe2\x86\x95"`

function parse_git_branch {
    git rev-parse --gir-dir &> /dev/null
    git_status="$(git status 2> /dev/null)"
    branch_pattern="^# On branch ([^${IFS}]*)"
    remote_pattern="# Your branch is (.*) of"
    diverge_pattern="# Your branch and (.*) have diverged"

    if [[ ! ${git_status} =~ "working directory clean" ]]; then
        state="${LIGHT_RED}${CHAR_BOLT}"
    fi

    if [[ ${git_status} =~ ${remote_pattern} ]]; then
        if [[ ${BASH_REMATCH[1]} == "ahead" ]]; then
            remote="${YELLOW}${CHAR_UP}"
        else
            remote="${YELLOW}${CHAR_DOWN}"
        fi
    fi

    if [[ ${git_status} =~ ${diverge_pattern} ]]; then
        remote="${YELLOW}${CHAR_DIV}"
    fi

    if [[ ${git_status} =~ ${branch_pattern} ]]; then
        branch=${BASH_REMATCH[1]}
        echo " (${branch})${remote}${state}"
    fi
}

function prompt_func() {
    previous_return_value=$?;

    if [ "$TERM" != "linux" -a -z "$EMACS" ]
    then
        TITLEBAR="\[\e]2;\u@\h:\w\a\]"
    else
        TITLEBAR=""
    fi

    PSI=`echo -e "\xe2\x88\xb4"`
    input_prompt="\n${LIGHT_BLUE}${PSI} >"
    prompt="${LIGHT_RED}\w${GREEN}$(parse_git_branch)${COLOR_NONE}"
    if test $previous_return_value -eq 0
    then
        PS1="${prompt}${GREEN} ${input_prompt}${COLOR_NONE} "
    else
        PS1="${prompt}${LIGHT_RED} ${input_prompt}${COLOR_NONE} "
    fi
}

PROMPT_COMMAND=prompt_func
