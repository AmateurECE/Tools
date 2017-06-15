#!/bin/sh
################################################################################
# NAME:		    difftool.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Traverses the directory provided and determines if there
#		    are any commits that need to be pushed.
#
# CREATED:	    06/15/2017
#
# LAST EDITED:	    06/15/2017
###

DIRS=`ls $1`

for dir in $DIRS; do
    if [ -e "$1/$dir/.git" ]; then
	REMOTE=`cat "$1/$dir/.git/refs/remotes/origin/master"`
	LOCAL=`cat "$1/$dir/.git/refs/heads/master"`
	if [ $REMOTE != $LOCAL ]; then
	    echo "$dir needs an update!"
	fi
    fi
done
	     
