#!/bin/sh
###############################################################################
# NAME:             SetDiscNumber.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Sets the Disc Total field in the files.
#
# CREATED:          01/23/2020
#
# LAST EDITED:      03/31/2020
###

die()
{
    printf '%s\n' "$1"
    exit 1
}

if [[ "x$1" = "x" || "x$1" = "-h" || "x$2" = "x" || "x$1" = "--help" ]]; then
    die "Usage: SetDiscNumber.sh <FLAC|MP3> <DiscTotal>"
fi

fileGlob="*.mp3"
if [[ "$1" = "FLAC" ]]; then
    fileGlob="*.flac"
fi

IFS=$'\n'
for f in `ls $fileGlob`; do
    printf '%s\n' "$f"
    case "$1" in
        FLAC)
            metaflac --set-tag="TOTALDISCS=$2" "$f"
        ;;
        MP3)
            disc=$(mid3v2 --list "$f" | grep 'TPOS' \
                       | sed -e 's/TPOS=\([0-9]\{1,\}\).*/\1/')
            mid3v2 --delete-frames=TPOS "$f"
            mid3v2 --TPOS="$disc/$2" "$f"
        ;;
    esac
done

###############################################################################
