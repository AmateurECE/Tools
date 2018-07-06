###############################################################################
# NAME:		    lousbrew.sh
#
# AUTHOR:	    Ethan D. Twardy
#
# DESCRIPTION:	    The cafe I like going to on the weekends changes their
#		    network password daily, but they use an obvious pattern.
#		    This is just a little brute force script so that I don't
#		    have to stand in line just to ask their WiFi password.
#
#		    The call to networksetup can take quite a while to return,
#		    and we're running it 100 times, so it might be prudent to
#		    leave it running in the background. For this purpose, I've
#		    written it to show an alert in the foreground when we
#		    complete with success.
#
# CREATED:	    06/23/2018
#
# LAST EDITED:	    06/23/2018
###

netName='LousBrew Network'
prefix='lousbrew'
dev='en0'
dialog="Connected to network successfully!"
for n in `seq 100`; do
    ret=`networksetup -setairportnetwork $dev "$netName" ${prefix}${n}`
    if echo $ret | grep -qv 'LousBrew'; then
	osascript -e "tell app \"System Events\" to display dialog \"$dialog\""
	exit 0
    fi
done

failure="Could not connect to network :("
osascript -e "tell app \"System Events\" to display dialog \"$failure\""

###############################################################################
