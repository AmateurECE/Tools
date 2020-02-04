#!/bin/bash
###############################################################################
# NAME:             retag.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Use this script to rename and retag audio files in the
#                   current directory.
#
# CREATED:          07/16/2019
#
# LAST EDITED:      01/23/2020
###

usage()
{
    printf '%s\n' \
           "retag.sh <directory path>" \
           "" \
           "This script will use the program AudioQueueOutput in Git/Apple/"\
           "to assist you in renaming and retagging audiobook files. Invoke"\
           "using the path of a directory containing audiobook files and"\
           "simply follow the prompts."
}

if [[ "x$1" = "x" || "$1" = "-h" || "$1" = "--help" ]]; then
    usage
    exit
fi

play="$HOME/Git/Apple/CoreAudioExperiments/AudioQueueOutput/build/Release/\
AudioQueueOutput"

# Currently, the script only supports mp3 files. Also, it's closely coupled to
# the naming convention used by ~/Git/Tools/chapter.sh.
IFS=$'\n'
numberChapters=$(ls *.mp3 | sort | awk 'BEGIN{RS="";FS="\n"}{print $NF}' \
                     | sed -e 's/Chapter-\([0-9]\{1,\}\)\.mp3/\1/')
numberChapters=$(expr "$numberChapters" + 1)
for f in `ls *.mp3`; do
    chapter=$(echo $f | sed -e 's/Chapter-\([0-9]\{1,\}\)\.mp3/\1/')
    chapter=$(printf '%02g' $(expr $chapter + 1))
    printf 'Chapter: %s\n' "$chapter"

    if [[ "x$1" = "xNormal" ]]; then
        title="Chapter ${chapter}"
    else
        trap "printf '%s' 'Title: '" INT
        eval "($play $f)"
        trap - INT
        read title
        if [[ "x$title" = "x" ]]; then
            title="Chapter ${chapter}"
        fi
    fi

    mid3v2 --TIT2 "$title" --TRCK "${chapter}/${numberChapters}" "$f"
    mid3v2 --delete-frames=CHAP "$f"
done

###############################################################################
