#!/bin/sh

for rep in `ls $PWS`
do
    if [ ! -d "$rep" -o "$rep" = "" ]
    then
	continue
    fi
    
    if [ -f "$PWD/$rep/.git/refs/heads/master" -a -f "$PWD/$rep/.git/refs/remotes/origin/master" ]
    then
	LOC_HASH=`cat $PWD/$rep/.git/refs/heads/master`
	REM_HASH=`cat $PWD/$rep/.git/refs/remotes/origin/master`

	if [ "$LOC_HASH" != "$REM_HASH" ]
	then
	    echo "$rep has at least one unpushed commit"
	fi
    fi
done