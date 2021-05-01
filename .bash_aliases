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
# LAST EDITED:	    04/30/2021
###

# Standard aliases
alias ls='ls -A'
alias ll='ls -lA'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias svn='svn --no-auth-cache'
alias tmux="tmux -f $MY_GIT/Tools/.tmux.conf"
alias hopto="ssh -p 5000 edtwardy@192.168.1.60"

export LANG="en_US.UTF-8"

# Colors!
export C_NC="\033[0m"
export C_RED="\033[0;31m"
export C_RED_BOLD="\033[2;31m"

# Setup for Sysgit
export SYSGIT_PATH="$MY_GIT"
export SYSGIT_IGNORE="$MY_GIT/not-mine"

# DEB* - Used by various debian package maintainer scripts
# GIT_* - Used by git commands
# EMAIL - Also used by other git commands
export EMAIL=ethan.twardy@gmail.com
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_EMAIL=$EMAIL
export DEBEMAIL=$EMAIL

export GIT_AUTHOR_NAME="Ethan D. Twardy"
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
export DEBFULLNAME=$GIT_AUTHOR_NAME

# TODO: Add docs for these functions
latex-template() {
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

# On OSX, creates an alert in the foreground displaying the text $1. If $1
# is '', display 'An alert has been dispatched by process $$!', where $$ is
# replaced with the PID of the shell. This function does nothing on other
# systems.
alert() {
    if [ `uname` != "Darwin" ]; then
	return
    fi

    message="$1"
    if [ "x$message" = "x" ]; then
	message="An alert has been dispatched by process $$"'!'
    fi

    # Generate the alert
    osascript -e "tell app \"System Events\" to display dialog \"$message\""
}

# Used to locate the plist files for launchd services which match the first
# argument given. Taken from here (and slightly modified):
# https://stackoverflow.com/questions/18502705
launchctlFind() {
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

###############################################################################
