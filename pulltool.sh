#!/bin/bash
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

    export pulltool_dir=`perl - <<'EOF'
my $dir = $ENV{'pulltool_dir'};
chomp $dir;
chop $dir;
my @locs = split("/", $dir);
print "$locs[$#locs - 1]";
EOF`

    if [[ $pulltool_dir = "." ]]; then
	export pulltool_dir="$PWD"
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

    if [ -d "$dir" ] && [ -e "$dir/refs/remotes/origin/master" ]; then
	export pulltool_dir="$dir"
	dir=`echo $dir | sed -e 's|/.git||'`
	cd "$dir"
	perl_get_dir
	echo "$pulltool_dir:"
	unset pulltool_dir
	git pull
	echo
    fi
done

################################################################################
