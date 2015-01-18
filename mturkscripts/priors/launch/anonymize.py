# Replaces workerIDs with integers from 1 to n

import sys, re, string

f = open(sys.argv[1], "r")

firstline = 0
index = 0
workerID = 1
for l in f:
    l = l.strip()
    if firstline == 0:
        fields = l.split("\t")
        index = fields.index('"workerid"')
        firstline = 1
        print "\t".join(fields)
    else:
        fields = l.split("\t")
        fields[index] = str(workerID)
        workerID = workerID + 1
        print "\t".join(fields)
