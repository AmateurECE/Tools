#!/bin/sh
###############################################################################
# NAME:             FlacTrackChecker.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      This shell script checks all the FLAC files in all
#                   subdirectories of this directory to ensure that the
#                   TRACKNUMBER field is correctly set.
#
# CREATED:          03/31/2020
#
# LAST EDITED:      03/31/2020
###

die()
{
    printf '%s\n' "$1"
    exit 1
}

if [[ "x$1" = "x-h" || "x$1" = "x--help" ]]; then
    die "Usage: FlacTrackChecker.sh"
fi

fileGlob="*.flac"
IFS=$'\n'
for f in `ls $fileGlob`; do
    title=`metaflac --list "$f" | grep 'TITLE' | cut -d'=' -f2`
    printf 'Checking "%s"...' "$title"
    trackNumber=`metaflac --list "$f" | grep 'TRACKNUMBER' | cut -d'=' -f2`
    if [[ $trackNumber == *"/"* ]]; then
        printf '%s\n' "Fixing"
        trackNumber=${trackNumber%%/*}
        metaflac --remove-tag=TRACKNUMBER "$f" \
            && metaflac --set-tag=TRACKNUMBER="$trackNumber" "$f"
    else
        printf '%s\n' "Correct"
    fi
done


###############################################################################
