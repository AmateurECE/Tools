#!/bin/sh
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
# LAST EDITED:      07/11/2019
###

inputFile="$1"
outputFileBase="$2"

printf 'Input: %s\nOutput: %s\n\n' "$inputFile" "$outputFileBase"

IFS=$'\n'
chapters=`ffmpeg -i "$inputFile" 2>&1 | grep 'Chapter #'`
for chapter in $chapters; do
    start=`echo $chapter | awk '{print $4}' | sed -e 's/,//'`
    end=`echo $chapter | awk '{print $6}'`
    number=`echo $chapter | awk -F':' '{print $2}'`

    # Get duration
    duration=`python -c "print($end - $start)"`

    printf 'Chapter %s:' "$number"
    printf 'Start: %s; End: %s; Duration: %s\n' "$start" "$end" "$duration"
    outputFile="${outputFileBase}-ch${number}.mp3"
    printf 'OutputFile: %s\n' "$outputFile"

    # Get the chapter
    ffmpeg -ss "$start" -t "$duration" -i "$inputFile" "$outputFile"
done

###############################################################################

