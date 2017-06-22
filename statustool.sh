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

HM="$PWD"
DIRS=`ls`

for dir in $DIRS; do
    if [ -d "$HM/$dir/.git" ]; then
	cd "$HM/$dir"
	if [[ "`git status`" =~ "(" ]]; then
	    echo "$dir has uncommitted changes!"
	fi
    fi
done
