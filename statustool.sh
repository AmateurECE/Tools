#!/bin/bash

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
