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

HM=$PWD
DIRS=`ls`

for dir in $DIRS; do
    if [ -d "$HM/$dir/.git" ]; then
	cd "$HM/$dir"
	echo "$dir:"
	git pull
	echo
    fi
done
