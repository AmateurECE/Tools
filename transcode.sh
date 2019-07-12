###############################################################################
# NAME:             transcode.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Transcode FLAC files to MP3 files.
#
# CREATED:          05/04/2019
#
# LAST EDITED:      05/09/2019
###

# TODO: does this actually work? Check to make sure.

IFS=$'\n'
ALBUMS=""
for f in `find . -name '*.flac'`; do
    EXISTS="false"
    for d in $ALBUMS; do
        if [ $d = `dirname $f` ]; then
            EXISTS="true"
        fi
    done
    dir=`dirname $f`
    dir="${dir%\\n}"
    if [ $EXISTS = "false" ]; then
        echo "DIR: $dir"
        mkdir -p "$dir-mp3"
        ALBUMS="$ALBUMS
$dir"
    fi
    output="${f/%flac/mp3}"
    output="`echo $output | sed -e \"s#$dir##\"`"
    ffmpeg -i $f -ab 320k -map_metadata 0 -id3v2_version 3 $dir-mp3/$output
done

###############################################################################
