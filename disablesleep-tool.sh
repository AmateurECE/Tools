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
# LAST EDITED:	    01/03/2019
###

if [ `whoami` != "root" ]; then
    >&2 echo "This script must be run as root."
    exit
fi

resetParams() {
    pmset -b sleep 60
    pmset -b disablesleep 0
}

trap resetParams EXIT

pmset -b sleep 0
pmset -b disablesleep 1
sleep 1800 # Process sleeps for 30 minutes

##############################################################################
