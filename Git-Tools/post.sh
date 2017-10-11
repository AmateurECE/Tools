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

################################################################################
# Functions
###

################################################################################
# FUNCTION:	    set_ignore_var
#
# DESCRIPTION:	    Sets the variable IGNORE_FILE to be the location of the
#		    .ignore file, if it exists in the same directory as this
#		    file.
#
# ARGUMENTS:	    the name of this file.
#
# RETURN:	    0 if successful, 1 otherwise.
#
# NOTES:	    none.
###
function set_ignore_var {

    # This should always exist, but never say never.
    if [[ ! -e $1 ]]; then
	return 1
    fi

    this_dir=`echo $(dirname $1) |
perl -p000e 's|(?<!\.)\.(?![\.\w])|$ENV{'PWD'}|' |
sed -e "s|\.\.|$(dirname $PWD)|"`

    if [[ -e "$PWD/$this_dir/.ignore" ]]; then
	export IGNORE_FILE="$PWD/$this_dir/.ignore"
	return 0
    elif [[ -e "$this_dir/.ignore" ]]; then
	export IGNORE_FILE="$this_dir/.ignore"
	return 0
    else
	return 1
    fi
}

################################################################################
# Main
###

RED='\033[0;31m'    # Set color to red
NC='\033[0m'	    # Set to no color
file_dir="$(dirname $0)"

set_ignore_var $0
if [[ $? != 0 ]]; then
    unset IGNORE_FILE
    echo No ignore file found. Checking all repositories instead.
fi

if [[ "$1" = "" ]]; then
    echo "No arguments given. Aborting."
    exit
fi

echo -e "${RED}STATUS:${NC}"
ret=`$file_dir/statustool.sh $1`
if [ "$ret" == "" ]; then
    echo None.
else
    echo "$ret"
fi

echo
echo -e "${RED}UNPUSHED COMMITS:${NC}"
ret=`$file_dir/pushtool.sh $1`
if [ "$ret" == "" ]; then
    echo None.
else
    echo "$ret"
fi

echo
echo "I am about to pull updates from remote repositories."
echo "Would you like me to continue? [y/N]"
read ans

if [ "$ans" == "y" ]; then
    $file_dir/pulltool.sh $1
fi

echo "Check complete."
unset IGNORE_FILE

################################################################################
