#!/bin/sh
################################################################################
# NAME:		    pulltool.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Pulls changes down from remote repositories, so that I don't
#		    have to bother keeping up with it!
#
# CREATED:	    06/22/2017
#
# LAST EDITED:	    06/22/2017
###

DIRS=`ls $1`

for dir in $DIRS; do
    if [ -d "$1/$dir/.git" ]; then
	cd "$1/$dir"
	echo "$dir:"
	git pull
	echo
    fi
done
