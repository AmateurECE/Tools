#!/bin/sh
###############################################################################
# NAME:		    pregit.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    This is where the PREGIT script lives. If
#		    PREGIT_ALLOW_SCRIPTS is set to 1, NEVER run git as root,
#		    or you risk compromising your system.
#
# CREATED:	    06/11/2018
#
# LAST EDITED:	    06/11/2018
###

###############################################################################
# FUNCTIONS
###

warn() {
    YELLOW="\033[33;1m"
    NC="\033[0m"
    if [[ "x$PREGIT_SILENT" = "x" ]]; then
	>&2 echo $YELLOW"WARN"$NC": $@"
    fi
}

###############################################################################
# MAIN
###

# Warn if one of our env variables is unset
if [[ "x$PREGIT_PREFIX" = "x" ]]; then
    warn "PREGIT_PREFIX is unset"
fi
if [[ "x$PREGIT_ALLOW_DIR" = "x" ]]; then
    warn "PREGIT_ALLOW_DIR is unset"
fi
if [[ "x$PREGIT_ALLOW_SCRIPTS" = "x" ]]; then
    warn "PREGIT_ALLOW_SCRIPTS is unset. pregit will not run or check for" \
	 "scripts."
fi
if [[ "x$PREGIT_GIT_LOCATION" = "x" ]]; then
    PREGIT_GIT_LOCATION=`which git`
    warn "PREGIT_GIT_LOCATION is unset. pregit may not be able to find the" \
	 "location of your git executable."
fi

# Find the path of the git directory.
GIT="."
while [[ ! -e "$GIT/.git" ]] && [[ `ls $GIT` != `ls /` ]]; do
    GIT="$GIT/.."
done

# Maybe execute the script
if [[ "x$PREGIT_ALLOW_SCRIPTS" = "x1" ]]; then
    if [[ -f "${GIT}/${PREGIT_PREFIX}${1}" ]]; then
	. "${GIT}/${PREGIT_PREFIX}${1}"
    elif [[ "x$PREGIT_ALLOW_DIR" = "x1" &&
		-f "${GIT}/${PREGIT_PREFIX}/${1}" ]]; then
	. "${GIT}/${PREGIT_PREFIX}/${1}"
    fi
fi

# Invoke git to do the thing we wanted.
$PREGIT_GIT_LOCATION "$@"

###############################################################################
