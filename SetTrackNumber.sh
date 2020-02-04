#!/bin/sh
###############################################################################
# NAME:             SetTrackNumber.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Sets the Track Total field in the files.
#
# CREATED:          01/23/2020
#
# LAST EDITED:      01/23/2020
###

die()
{
    printf '%s\n' "$1"
    exit 1
}

if [[ "x$1" = "x" || "x$1" = "-h" || "x$2" = "x" || "x$1" = "--help" ]]; then
    die "Usage: SetTrackNumber.sh <FLAC|MP3> <TrackTotal>"
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
            track=$(metaflac --show-tag=TRACKNUMBER "$f" \
                        | grep -o -m1 '[0-9]\+')
            metaflac --remove-tag=TRACKNUMBER "$f"
            metaflac --set-tag="TRACKNUMBER=$track/$2" "$f"
        ;;
        MP3)
            track=$(mid3v2 --list "$f" | grep 'TRCK' \
                        | sed -e 's/TRCK=\([0-9]\{1,\}\).*/\1/')
            mid3v2 --delete-frames=TRCK "$f"
            mid3v2 --TRCK="$track/$2" "$f"
        ;;
    esac
done

###############################################################################
