################################################################################
# NAME:		    .bash_aliases
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is just some useful stuff.
#
# CREATED:	    10/23/2017
#
# LAST EDITED:	    10/23/2017
###

alias ls='ls -A'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias python='python3'
alias emacs='/usr/bin/emacs24 -nw'
alias repo-check='/Users/ethantwardy/Git/Tools/Git-Tools/post.sh'
alias screen='screen -c /Users/ethantwardy/.screenrc'
alias svn='svn --no-auth-cache'
alias t='python ~/t/t.py --task-dir ~/Git/Tools/tasks/ --list tasks'
alias b='python ~/t/t.py --task-dir . --list bugs --delete-if-empty'

function b {
    GIT="."
    while [[ ! -e "$GIT/.git" ]] && [[ `ls $GIT` != `ls /` ]]; do
	GIT="$GIT/.."
    done

    if [[ ! -e "$GIT/.git" ]]; then
	echo >&2 "fatal: Not a git repository (or any of the parent " \
		 "directories): .git"
	return 1
    fi
    
    T="--task-dir . --list bugs --delete-if-empty"
    if [[ $1 == "update" ]]; then
	BUGS=`awk -F'TODO:? ' '/(# |\/* )TODO:? /{print FILENAME": "$2}' \`find $GIT\``
	IFS=$(echo -e "\n\b")
	for f in $BUGS; do
	    eval python ~/Git/not-mine/t/t.py "$T \"$f\""
	done
    elif [[ $1 == "" ]]; then
	eval python ~/Git/not-mine/t/t.py $T
    else
	echo >&2 "fatal: command not understood"
    fi
}

################################################################################
