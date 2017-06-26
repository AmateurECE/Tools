#!/bin/sh
################################################################################
# NAME:		    pushtool.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Is able to tell the user if any of his/her repositories have
#		    uncommitted changes
#
# CREATED:	    06/26/2017
#
# LAST EDITED:	    06/26/2017
###

for rep in `ls $1`
do
    if [ ! -d "$1/$rep" ] || [ "$rep" = "" ]
    then
	continue
    fi
    
    if [ -f "$1/$rep/.git/refs/heads/master" ] && [ -f "$1/$rep/.git/refs/remotes/origin/master" ]
    then
	LOC_HASH=`cat $1/$rep/.git/refs/heads/master`
	REM_HASH=`cat $1/$rep/.git/refs/remotes/origin/master`

	if [ "$LOC_HASH" != "$REM_HASH" ]
	then
	    echo "$rep has at least one unpushed commit!"
	fi
    fi
done
