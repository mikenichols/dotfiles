#!/usr/bin/env bash

sudo umount /mnt/e
sudo mount -t drvfs e: /mnt/e

rsync -vr --size-only /mnt/e/Music/ /mnt/c/Users/mike/Music/iTunes/iTunes\ Media/Music 