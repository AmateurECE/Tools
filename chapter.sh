#!/bin/bash
###############################################################################
# NAME:             chapter.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Clips audio books into separate files, each containing one
#                   chapter of the audiobook.
#
# CREATED:          05/30/2019
#
# LAST EDITED:      12/05/2020
###

die()
{
    printf '%s\n' "$1"
    exit 1
}

exitFn() {
    rm -f "$tempFiles"
    exit 0
}

trap exitFn INT
trap exitFn EXIT

inputFile="$1"
outputFileBase="$2"

if [[ "x$1" = "x" || "x$2" = "x" ]]; then
    die "Usage: chapter.sh <input> <outputBase>"
fi

printf 'Input: %s\nOutput: %s\n\n' "$inputFile" "$outputFileBase"

# Set up the log file
logFile="./chapterScript.log"
errorMessage="Error: ffmpeg failed. See the log for more information"
tempFiles=""

# First, get the album artwork
artworkFile=$(mktemp -q /tmp/$(basename $0).XXXXXX)
tempFiles="${tempFiles}${IFS}${artworkFile}"

printf '%s\n' "Obtaining artwork from source file..."
copyArtwork="no" # Update: Artwork is copied from source automatically
ffmpeg -i "$inputFile" "${artworkFile}.jpg" > "$logFile" 2>&1
if [[ $? != "0" ]]; then
    printf '%s\n' "Unable to obtain artwork. No artwork will be copied."
    copyArtwork="no"
fi

inputExt=$(echo $1 | awk -F'.' '{print $NF}')
intermediateFile=$(mktemp -q /tmp/$(basename $0).XXXXXX.${inputExt})
tempFiles="${tempFiles}${IFS}${intermediateFile}"

IFS=$'\n'
chapters=`ffmpeg -i "$inputFile" 2>&1 | grep 'Chapter #'`
lastChapter=$(echo "$chapters" | awk 'BEGIN{RS="";FS="\n"}{print $NF}' \
                  | awk -F':' '{print $2}')
for chapter in $chapters; do
    start=`echo $chapter | awk '{print $4}' | sed -e 's/,//'`
    end=`echo $chapter | awk '{print $6}'`
    number=$(printf '%02d' `echo $chapter | awk -F':' '{print $2}'`)

    # Get duration
    duration=`python -c "print($end - $start)"`

    printf 'Chapter %s/%s:\n' "$number" "$lastChapter"
    printf 'Start: %s; End: %s; Duration: %s\n' "$start" "$end" "$duration"
    outputFile="${outputFileBase}Chapter-${number}.mp3"
    printf 'OutputFile: %s\n' "$outputFile"

    # Get the chapter
    printf '%s\n' "Copying chapter to file..."
    ffmpeg -y -ss "$start" -t "$duration" -vsync 2 -i "$inputFile" \
           "$outputFile" > "$logFile" 2>&1 || die "$errorMessage"

    # Do the album artwork (maybe)
    if [[ $copyArtwork = "yes" ]]; then
        printf '%s\n' "Fixing up artwork..."
        ffmpeg -y -i "$outputFile" -i "${artworkFile}.jpg" -map 0:0 -map 1:0 \
               -c copy -id3v2_version 3 -metadata:s:v title="Album cover" \
               -metadata:s:v comment="Cover (front)" \
               "${intermediateFile}.${inputExt}" \
               > "$logFile" 2>&1 || die "$errorMessage"
        cp -f "${intermediateFile}.${inputExt}" "$outputFile"
    fi
done

###############################################################################
