###############################################################################
# NAME:             SortUp.py
#
# AUTHOR:           Ethan D. Twardy <edtwardy@mtu.edu>
#
# DESCRIPTION:      Sorts a particular album.
#
# CREATED:          10/16/2019
#
# LAST EDITED:      10/10/2020
###

import argparse
import os
import re
import subprocess
import selectors
import sys

###############################################################################
# Functions
###

def parseDate(dateString):
    """
    Parse the date string in format %M:%S:%D, where M is minutes, S is seconds,
    and D is decimal fractions of a second.
    """
    parts = dateString.split(':')
    minutes = int(parts[0])
    seconds = int(parts[1]) + (int(parts[2]) / 100.0)
    return str((minutes * 60) + seconds)

def readTrack(cueFile, fileName, trackLine):
    """Reads a track segment from the cue file"""
    trackData = {}
    trackData['TRACKNUMBER'] = int(trackLine.split()[1])
    trackData['TITLE'] = ''.join(list(' '.join(
        cueFile.readline().split()[1:]))[1:-1])
    cueFile.readline() # Throwaway for PERFORMER line
    trackData['INDEX'] = parseDate(cueFile.readline().split()[2])
    trackData['FILE'] = ''.join(list(' '.join(fileName.split()[1:-1]))[1:-1])
    return trackData

def readCue(cueFileName):
    """Parses a .cue file to get track information."""
    with open(cueFileName, 'r') as cueFile:
        line = cueFile.readline()
        albumData = []
        artist = None
        album = None
        while line:
            while "FILE" not in line and line:
                if "PERFORMER" in line:
                    artist = ' '.join(line.split()[1:])[1:-1]
                if "TITLE" in line:
                    album = ' '.join(line.split()[1:])[1:-1]
                line = cueFile.readline()
            if line:
                fileName = line
                line = cueFile.readline()
                while "TRACK" in line:
                    albumData.append(readTrack(cueFile, fileName, line))
                    line = cueFile.readline()
        return artist, album, albumData

def getSampleRate(filename):
    """Obtain the sample rate of the source"""
    if '"' in filename:
        filename = filename.replace('"', '\\"')
    command = 'ffprobe "' + filename + '"'
    subp = execPipeToMemory(command)
    pattern = re.compile(r"^\s+Stream #\d:\d: Audio\D*(\d+) Hz")
    for line in subp.stderr:
        match = re.match(pattern, line.decode('utf-8'))
        if match:
            return match.group(1)

def obtainSegment(trackName, sampleRate, fromFile, start, length):
    """Obtain the segment of audio from `fromFile`"""
    if length:
        command = ("ffmpeg -y -i '{}' -ss {} -t {} "
                   .format(fromFile, start, length))
    else:
        command = "ffmpeg -y -i '{}' -ss {} ".format(fromFile, start)
    # Don't include video streams; set audio sample rate to 192kHz
    otherOptions = "-vn -ar " + sampleRate + " "
    if '"' in trackName:
        trackName = trackName.replace('"', '\\"')
    command += otherOptions + '"' + trackName + '.flac' + '"'
    print("{}".format(command), file=sys.stderr)
    execPipeToMemory(command)
    return trackName + '.flac'

def populateMetadata(inputFile, dataMap):
    """Sets tags in the file."""
    command = "metaflac "
    command += createSetTagPair(dataMap, 'ARTIST')
    command += createSetTagPair(dataMap, 'ALBUM')
    command += createSetTagPair(dataMap, 'TRACKNUMBER')
    command += createSetTagPair(dataMap, 'TOTALTRACKS')
    command += createSetTagPair(dataMap, 'TITLE')
    command += createSetTagPair(dataMap, 'GENRE')
    command += createSetTagPair(dataMap, 'DATE')

    if '"' in inputFile:
        inputFile = inputFile.replace('"', '\\"')
    if 'IMAGE' in dataMap.keys():
        command += ('--import-picture-from="{}" '.format(dataMap['IMAGE']))
    command += '"' + inputFile + '"'
    execPipeToMemory(command)

def createSetTagPair(dataMap, tagName):
    """
    If tagName is a member of dataMap, add a command pair to remove the
    previous value of tagName and set new value to the value in dataMap.
    """
    if tagName in dataMap.keys():
        return ('--remove-tag={0} --set-tag="{0}={1}" '
                .format(tagName, dataMap[tagName]))
    return ""

def execPipeToMemory(command):
    """
    Executes the command, and only prints any output if there was an
    error.
    """
    subp = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    sel = selectors.DefaultSelector()
    sel.register(subp.stdout, selectors.EVENT_READ)
    sel.register(subp.stderr, selectors.EVENT_READ)
    subp.wait()
    if subp.returncode != 0:
        for key, _ in sel.select():
            data = key.fileobj.read1().decode()
            if not data:
                break
            if key.fileobj is subp.stdout:
                print(data, end="")
            else:
                print(data, end="", file=sys.stderr)
    return subp

###############################################################################
# Main
###

def main():
    """Implements main logic"""
    parser = argparse.ArgumentParser()
    parser.add_argument("cueFile", help='The path of the cue file')
    args = parser.parse_args()

    artistName, albumName, albumInfo = readCue(args.cueFile)
    fileName = os.path.join(os.path.dirname(args.cueFile),
                            albumInfo[0]['FILE'])
    sampleRate = getSampleRate(fileName)
    totalTracks = len(albumInfo)
    for index in range(0, len(albumInfo)):
        album = albumInfo[index]
        length = 0
        if index < len(albumInfo) - 1 and (albumInfo[index + 1]['FILE']
                                           == album['FILE']):
            length = (float(albumInfo[index + 1]['INDEX'])
                      - float(album['INDEX']))
        print('Obtaining "{}"...'.format(album['TITLE']), end="", flush=True)
        trackName = obtainSegment(
            str(album['TRACKNUMBER']) + ' ' + album['TITLE'],
            sampleRate, fileName,
            float(album['INDEX']), length)
        dataMap = {
            'TRACKNUMBER': str(album['TRACKNUMBER']),
            'TITLE': album['TITLE'],
            'TOTALTRACKS': totalTracks,
        }
        if artistName is not None:
            dataMap['ARTIST'] = artistName
        if albumName is not None:
            dataMap['ALBUM'] = albumName

        print('Populating Metadata...', end="", flush=True)
        populateMetadata(trackName, dataMap)
        print()

if __name__ == '__main__':
    main()

###############################################################################
