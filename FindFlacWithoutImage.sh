#!/bin/sh
###############################################################################
# NAME:             FindFlacWithoutImage.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Finds files in the current directory that don't have cover
#                   art.
#
# CREATED:          06/09/2020
#
# LAST EDITED:      06/09/2020
###

IFS=$'\n'
for f in `find . -name '*.flac'`; do
    metaflac --export-picture-to=./folder.jpg "$f" 2>/dev/null
    if [ $? -eq "1" ]; then
        printf '%s\n' "$f"
    fi
done

###############################################################################
