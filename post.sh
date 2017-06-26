#!/bin/bash
################################################################################
# NAME:		    post.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Script I run in the afternoon before closing up to make sure
#		    that any changes made throughout the day are fixed.
#
# CREATED:	    06/26/2017
#
# LAST EDITED:	    06/26/2017
###

RED='\033[0;31m'    # Set color to red
NC='\033[0m'	    # Set to no color

echo -e "${RED}STATUS:${NC}"
ret=`./statustool.sh $1`
if [ "$ret" == "" ]; then
    echo None.
else
    echo "$ret"
fi

echo -e "${RED}UNSTAGED COMMITS:${NC}"
ret=`./pushtool.sh $1`
if [ "$ret" == "" ]; then
    echo None.
else
    echo "$ret"
fi

echo "I am about to pull updates from remote repositories."
echo "Would you like me to continue? [y/N]"
read ans

if [ "$ans" == "y" ]; then
    ./pulltool.sh $1
fi

echo "Check complete."
