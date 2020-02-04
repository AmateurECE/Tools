###############################################################################
# NAME:             transcode.sh
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Replace FLAC versions of files with mp3 versions.
#
# CREATED:          05/04/2019
#
# LAST EDITED:      08/31/2019
###

IFS=$'\n'
for f in `find . -name '*.flac'`; do
    output="${f/%flac/mp3}"
    ffmpeg -i "$f" -ab 320k -map_metadata 0 -id3v2_version 3 "$output" \
           && rm -f "$f"
done

###############################################################################
