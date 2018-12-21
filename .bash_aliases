#!/bin/bash
###############################################################################
# NAME:		    .bash_aliases
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is just some useful stuff. You may notice some
#		    references to $MY_GIT in this file. MY-GIT contains the
#		    path of the directory containing all of my git repos, so
#		    that I can just run this file as a shell script from my
#		    actual .bash_aliases file on all of my machines.
#
# CREATED:	    10/23/2017
#
# LAST EDITED:	    12/21/2018
###

# Standard aliases
alias ls='ls -A'
alias ll='ls -lA'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias screen='screen -c ~/.screenrc'
alias svn='svn --no-auth-cache'

# repo-check is an old tool, about to be EOL'd
# TODO: EOL repo-check
alias repo-check='$MY_GIT/Tools/Git-Tools/post.sh'

# Setup for bits, another tool
# TODO: Get my .gitconfig under version control.
# TODO: Get my .bash_profile under version control.
alias bits="$MY_GIT/Tools/bits/bits"

LANG="en_US.UTF-8"
export RECIPES="$MY_GIT/Doc/Recipes"

# Colors!
export C_NC="\033[0m"
export C_RED="\033[0;31m"
export C_RED_BOLD="\033[2;31m"

# Setup for Sysgit
alias sysgit='$MY_GIT/Tools/Sysgit/sysgit.py'
export SYSGIT_PATH="~/Git"

# TODO: Add docs for these functions
function latex-template {
    GITHUB='https://raw.githubusercontent.com'
    T=$1
    if [[ "x$T" = "x" ]]; then
	T="template"
    fi
    curl -s "$GITHUB/AmateurECE/Tools/master/LaTeX/template.tex" > $T.tex
    if [[ $? = "0" ]]; then
	echo Done!
    else
	echo Something went wrong!
    fi
}

function plist-template {
    GITHUB='https://raw.githubusercontent.com'
    T=$1
    if [[ "x$T" = "x" ]]; then
	T="template"
    fi
    curl -s "$GITHUB/AmateurECE/Tools/master/template.plist" > $T.plist
    if [[ $? = "0" ]]; then
	echo Done!
    else
	echo Something went wrong!
    fi
}

function join-by {
    local d=$1
    local SAVE=$IFS
    IFS=""
    shift
    echo -n "$1"
    shift
    printf "%s" "${@/#/$d}"
    IFS=$SAVE
}

# TODO: b should not save bugs from anything in .gitignore
# TODO: b is still having trouble with quotes in todo statements
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
    
    T="--task-dir . --list $GIT/bugs --delete-if-empty"
    if [[ $1 == "update" ]]; then
	if [ -e $GIT/bugs ]; then
	    rm -f $GIT/bugs
	fi
	# Read in the .bignore file.
	REGX=""
	if [ -f "$GIT/.bignore" ]; then
	    IGNORE=`cat "$GIT/.bignore" | sed -e 's/#.*$//' -e '/^\s*$/d'`
	    IGNORE=`echo $IGNORE | sed -e 's/\n/ /'`
	    REGX=`join-by '\|' ".git/" $IGNORE`
	fi
	if [ "x$REGX" == "x" ]; then
	    REGX='.git/'
	fi
	REGX=${REGX}"\|~"
	LIST=`find $GIT -type f | grep -v "$REGX"`
	BUGS=`awk -F'TODO:? ' '/(# |\/* )TODO:? /{print FILENAME": "$2}' $LIST`
	IFS=$(echo -e "\n\b")
	for f in $BUGS; do
	    f=`echo $f | grep -v '[^[:space:]]\+: \*/' | sed -Ee 's|[.]+/||g'`
	    f=`echo $f | sed -e 's#\*/.*##'`
	    if [ "x$f" = "x" ]; then
		continue
	    fi

	    f=`echo $f | sed -e 's/\`/\\\\\`/g'`
	    eval python $MY_GIT/Tools/t/t.py "$T" "\"$f\""
	done
    elif [[ $1 == "" ]]; then
	eval python $MY_GIT/Tools/t/t.py $T
    else
	echo >&2 "fatal: command not understood"
    fi
}

recipes() {
    if [ "x$1" = "x" ]; then
	ls $RECIPES
	return
    fi

    if [ -f "$RECIPES/$1.7" ]; then
	man $RECIPES/$1.7
    else
	echo >&2 "No recipe found for $1"
    fi
}

# Calculate the number of system lines of code in all files in this and all
# subdirectories which match the regular expression given.
sloc() {
    if [[ "x$1" = "x" ]]; then
	REG='\.[ch]'
    else
	REG=$1
    fi
    total=0
    # Calculate the lines in each individual file
    for f in `find . | grep $REG`; do
	lines=`cat $f | sed -e '/^\s*$/d' | wc -l \
		   | awk -F'[[:blank:]]+' '{print $2;}'`
	echo "$f: $lines"
	total=$(expr $total + $lines)
    done
    echo "-------------"
    echo "Total: $total"
}

# Used to locate the plist files for launchd services which match the first
# argument given. Taken from here (and slightly modified):
# https://stackoverflow.com/questions/18502705
launchctlFind () {
    LaunchctlPATHS=( \
        ~/Library/LaunchAgents \
        /Library/LaunchAgents \
        /Library/LaunchDaemons \
        /System/Library/LaunchAgents \
        /System/Library/LaunchDaemons \
    )

    for curPATH in "${LaunchctlPATHS[@]}"
    do
        grep -rl "$curPATH" -e "$1"
    done
    return 0;
}

# TODO: `notify` wrapper. Usage: `notify grep -r int main` -- create an alert
# when `grep -r 'int main'` finishes execution.

###############################################################################
