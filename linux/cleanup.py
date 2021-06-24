# single.py
#
# Will Zywiec

import sys
from os import system

newOutput = open(sys.argv[2], 'w')
oldOutput = open(sys.argv[1])
oldStr = sys.argv[1]

for line in oldOutput:

    if 'final result' in line:

        newStr = sys.argv[1]
        newStr += ', ' + line.split(' ')[16]
        newStr += ', ' + line.split(' ')[25] + '\n'

        newOutput.write(newStr)
        newOutput.close()

        system('rm ' + oldStr)
        system('rm ' + oldStr[:-2] + '.i')
        system('rm ' + oldStr[:-2] + '.srctp')
        system('rm ' + oldStr[:-2] + '.runtpe')
