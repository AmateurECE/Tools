#!/bin/sh
i=0
while [ 1 ]; do
    printf '%s\r' "i is currently: $i"
    i=$(expr $i + 1)
    sleep 1
done
