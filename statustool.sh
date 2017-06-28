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

################################################################################
# Functions
###

################################################################################
# FUNCTION:	    perl_get_dir
#
# DESCRIPTION:	    Invoke Perl with a here document to do some parsing for us.
#
# ARGUMENTS:	    None.
#
# RETURN:	    None.
#
# NOTES:	    
###
function perl_get_dir {

    export statustool_dir=`perl - <<'EOF'
    my $dir = $ENV{'statustool_dir'};
    chomp $dir;
    chop $dir;
    my @locs = split("/", $dir);
    print "$locs[$#locs - 1]";
    EOF`

    if [[ $statustool_dir = "." ]]; then
	export statustool_dir="$PWD";
    fi
}

################################################################################
# Main
###

DIRS=`find $1 -type d -name ".git"`
CURR_PWD=$PWD

for dir in $DIRS; do

    if [[ "$dir" =~ ".." ]]; then
	dir=`echo $dir | sed -e 's|\.\.|'"$(dirname $CURR_PWD)"'|'`
    elif [[ "$dir" =~ "." ]]; then
	dir=`echo $dir | sed -e 's|\.|'"$CURR_PWD"'|'`
    fi
    
    if [ -d "$dir" ]; then
	export statustool_dir="$dir"
	dir=`echo $dir | sed 's|/.git||'`
	cd "$dir"
	if [[ "`git status`" =~ "Changes" ]]; then
	    perl_get_dir
	    echo "$statustool_dir has uncommitted changes!"
	    unset statustool_dir
	fi
    fi

done

################################################################################