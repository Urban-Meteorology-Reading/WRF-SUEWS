#!/usr/bin/env python
import os


def getFilelist(dir, suffiex):
    filelist = [file for file in os.listdir(dir) if file.endswith(suffiex)]
    return filelist


f95Files = getFilelist('./', 'f95')
for filename in f95Files:
    print 'Processing file %s...' % filename
    cmd = 'mv ' + filename + ' ' + filename[0:-4] + '.F'
    os.system(cmd)
