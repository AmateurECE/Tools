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
# LAST EDITED:	    06/12/2018
###

###############################################################################
# FUNCTIONS
###

error() {
    RED="\033[31;1m"
    NC="\033[0m"
    if [[ "x$PREGIT_SILENT" = "x" ]]; then
	>&2 echo $RED"ERROR"$NC": $@"
    fi
    exit
}

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
if [[ "x$PREGIT_ALLOW_SCRIPTS" = "x" ]]; then
    warn "PREGIT_ALLOW_SCRIPTS is unset. pregit will not run or check for" \
	 "scripts."
fi
if [[ "x$PREGIT_GIT_LOCATION" = "x" ]]; then
    PREGIT_GIT_LOCATION=`which git`
    warn "PREGIT_GIT_LOCATION is unset. pregit may not be able to find the" \
	 "location of your git executable."
fi
# Warn if the user wants to be in test mode.
if [[ "x$PREGIT_TEST_MODE" = "x1" ]]; then
    warn "Entering test mode."
fi
if [[ "x$PREGIT_CHECK_RET" = "x" ]]; then
    warn "PREGIT_CHECK_RET is unset."
fi

# Find the path of the git directory.
GIT="."
while [[ ! -e "$GIT/.git" ]] && [[ `ls $GIT` != `ls /` ]]; do
    GIT="$GIT/.."
done

# Maybe execute the script
RETC=0
SCRIPTFN=""
if [[ "x$PREGIT_ALLOW_SCRIPTS" = "x1" ]]; then
    if [[ -f "${GIT}/${PREGIT_PREFIX}${1}" ]]; then
	SCRIPTFN="${GIT}/${PREGIT_PREFIX}${1}"
    elif [[ "x$PREGIT_ALLOW_DIR" = "x1" &&
		-f "${GIT}/${PREGIT_PREFIX}/${1}" ]]; then
	SCRIPTFN="${GIT}/${PREGIT_PREFIX}/${1}"
    fi

    if [[ "x$SCRIPTFN" != "x" ]]; then
	. $SCRIPTFN
	RETC=$?
    fi
fi

if [[ "x$PREGIT_CHECK_RET" = "x1" && "x$RETC" != "x0" ]]; then
    error "Non-zero return code from user script: $SCRIPTFN"
fi

# Invoke git to do the thing we wanted.
if [[ "x$PREGIT_TEST_MODE" != "x1" ]]; then
    $PREGIT_GIT_LOCATION "$@"
else
    echo $PREGIT_GIT_LOCATION "$@"
fi

###############################################################################
