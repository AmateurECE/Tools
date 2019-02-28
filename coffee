#!/usr/local/bin/bash
###############################################################################
# NAME:		    disablesleep-tool.sh
#
# AUTHOR:	    Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:	    This script prevents the Mac from sleeping when the lid is
#		    shut for 30 minutes.
#
# CREATED:	    12/30/2018
#
# LAST EDITED:	    02/28/2019
###

usage() {
    printf '%s\n' \
	   "Usage: coffee <numSeconds>" \
	   "" \
	   "Disables sleep for <numSeconds> seconds."
}

resetParams() {
    pmset -b sleep 60
    pmset -b disablesleep 0
}

if [ `whoami` != "root" ]; then
    >&2 echo "This script must be run as root."
    exit
fi

if [ "x$1" = "x" ]; then
    usage
    exit 1
fi

trap resetParams EXIT

pmset -b sleep 0
pmset -b disablesleep 1
sleep $1 # Process sleeps for <numSeconds>

##############################################################################
