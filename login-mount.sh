#!/bin/bash
###############################################################################
# NAME:		    login-mount.sh
#
# AUTHOR:	    Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:	    This script is a login item which runs when I initially
#		    log in to OS X. It simply mounts the sparse image that
#		    contains my arm-linux toolchain. Apple has EOL'd fstab and
#		    replaced it with autofs, which does not work correctly on
#		    the current release of OS X.
#
#		    Originally, this script worked by some 'login item' utility
#		    that's standard on OS X, but I plan to update it to a
#		    launchctl job whenever I get around to it. Currently it is
#		    in an unusable state.
#
# CREATED:	    09/10/2018
#
# LAST EDITED:	    11/22/2018
###

# TODO: Update to use launchctl.
name=arm-linux-toolchain
if [[ ! -d /Volumes/$name ]]; then
    open /Users/ethantwardy/Git/$name.sparseimage
fi

##############################################################################
