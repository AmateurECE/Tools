#!/bin/sh
###############################################################################
# NAME:		    etwardy-nfs.sh
#
# AUTHOR:	    Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:	    Mounts my at home NFS server. I know it's only two
#		    commands, but my laziness knows no bounds.
#
# CREATED:	    07/12/2018
#
# LAST EDITED:	    07/12/2018
###

if [ `whoami` != "root" ]; then
    >&2 echo "This script must be run as root."
    exit 1
fi

serverIp=192.168.0.29
mountPoint=/Volumes/Passport

mkdir $mountPoint
mount -o resvport $serverIp:/export/Passport $mountPoint

###############################################################################