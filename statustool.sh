#!/bin/bash
################################################################################
# NAME:		    statustool.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Determines if the working directory is clean in a repo.
#
# CREATED:	    06/22/2017
#
# LAST EDITED:	    06/22/2017
###

DIRS=`ls $1`

for dir in $DIRS; do
    if [ -d "$1/$dir/.git" ]; then
	cd "$1/$dir"
	if [[ "`git status`" =~ "(" ]]; then
	    echo "$dir has uncommitted changes!"
	fi
    fi
done
