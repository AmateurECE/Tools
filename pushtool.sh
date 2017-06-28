#!/bin/bash
################################################################################
# NAME:		    pushtool.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    Traverses the directory provided and determines if there
#		    are any commits that need to be pushed.
#
# CREATED:	    06/15/2017
#
# LAST EDITED:	    06/15/2017
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

    export pushtool_dir=`perl - <<'EOF'
    my $dir = $ENV{'pushtool_dir'};
    chomp $dir;
    chop $dir;
    my @locs = split("/", $dir);
    print "$locs[$#locs - 1]";
    EOF`
    
    if [[ $pushtool_dir = "." ]]; then
	export pushtool_dir="$PWD";
    fi
}

################################################################################
# Main
###

DIRS=`find $1 -type d -name ".git"`

for dir in $DIRS; do
    if [ -d "$dir" ]; then
	if [ -f "$dir/refs/remotes/origin/master" ] \
	    && [ -f "$dir/refs/heads/master" ]; then
	    REMOTE=`cat "$dir/refs/remotes/origin/master"`
	    LOCAL=`cat "$dir/refs/heads/master"`
	    if [ $REMOTE != $LOCAL ]; then
		export pushtool_dir="$dir"
		perl_get_dir
		echo "$pushtool_dir needs a commit!"
		unset pushtool_dir
	    fi
	fi
    fi
done
	     
################################################################################