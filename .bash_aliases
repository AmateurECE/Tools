################################################################################
# NAME:		    .bash_aliases
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is just some useful stuff. You may notice some
#		    references to $MY_GIT in this file. MY-GIT contains the path
#		    of the directory containing all of my git repos, so that I
#		    can just run this file as a shell script from my actual
#		    .bash_aliases file in both my home and work machines.
#
# CREATED:	    10/23/2017
#
# LAST EDITED:	    12/23/2017
###

alias ls='ls -A'
alias ll='ls -lA'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias python='python3'
alias repo-check='$MY_GIT/Tools/Git-Tools/post.sh'
alias screen='screen -c ~/.screenrc'
alias svn='svn --no-auth-cache'
alias t="python $MY_GIT/not-mine/t/t.py --task-dir $MY_GIT/Tools/tasks/ \
--list tasks"
alias p="python $MY_GIT/not-mine/t/t.py --task-dir $MY_GIT/Tools/tasks/ \
--list projects"

function latex-template {
    GITHUB='https://raw.githubusercontent.com'
    curl -s "$GITHUB/AmateurECE/Tools/master/LaTeX/template.tex" > template.tex
    if [ $? = "0" ]; then
	echo Done!
    else
	echo Something went wrong!
    fi
}

# TODO: Update b to except certain files (maybe a .bignore file?)
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
	if [ -e $PWD/bugs ]; then
	    rm -f $PWD/bugs
	fi
	LIST=`find $GIT -type f | grep -v '.git/'`
	BUGS=`awk -F'TODO:? ' '/(# |\/* )TODO:? /{print FILENAME": "$2}' $LIST`
	IFS=$(echo -e "\n\b")
	for f in $BUGS; do
	    eval python $MY_GIT/not-mine/t/t.py "$T \"$f\""
	done
    elif [[ $1 == "" ]]; then
	eval python $MY_GIT/not-mine/t/t.py $T
    else
	echo >&2 "fatal: command not understood"
    fi
}

################################################################################
