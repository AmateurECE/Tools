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
# FUNCTION:	    filter_repos
#
# DESCRIPTION:	    Return a value dependent on whether or not the repository
#		    is to be ignored. Returns 0 for the go ahead, nonzero else.
#
# ARGUMENTS:	    $1: a filename to check.
#
# RETURN:	    0 if the file is not to be ignored, >0 otherwise.
#
# NOTES:	    none.
###
function filter_repos {

    file=`echo $(dirname $1) | perl -p000e 's/.*\/(.*)$/$1/'`
    if [[ -e $IGNORE_FILE ]]; then
	to_ignore=`cat "$IGNORE_FILE" | sed -e '/#.*/d' -e '/^\s*$/d'`
	for line in $to_ignore; do
	    ok=`echo $file | grep "$line"`
	    if [[ $ok == "" ]]; then
		return 0
	    else
		return 1
	    fi
	done
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

    filter_repos $dir
    if [[ $? == "0" ]]; then

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
    
    fi
done

################################################################################
