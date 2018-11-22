#!/bin/sh
###############################################################################
# NAME:		    wifi.sh
#
# AUTHOR:	    Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:	    This script uses the networksetup tool on OS X to connect
#		    to a WiFi network, then leases a new DHCP address from the
#		    router.
#
#		    Earlier this Fall I was experiencing a bug with OS X that
#		    prevented my MacBook from connecting to any new WiFi
#		    network. This is the fix I developed. One day, I restarted
#		    my computer, and I haven't encountered the bug since.
#		    I'm keeping this script around in case it comes back again.
#
# CREATED:	    08/25/2018
#
# LAST EDITED:	    11/22/2018
###

if [ `whoami` != "root" ]; then
    echo >&2 "Error: This script must be run as root"
fi

# Get the network information
read $networkName "SSID: "
stty -echo
read $password "WPA2 Password: "
stty echo
interface=en0

# Connect to the network
networksetup -setairportnetwork $interface $networkName $password
ipconfig set $interface DHCP

##############################################################################
