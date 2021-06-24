# combine.py
#
# Will Zywiec

import numpy as np

from os import getcwd, listdir, system

path = getcwd()

seq = np.arange(25, 4025, 25).tolist()

with open(path + '/output.txt', 'w') as output:

    for s in seq:

        files = [f for f in listdir(path + '/' + str(s)) if '.txt' in f]
        files.sort()

        for f in files:
            with open(path + '/' + str(s) + '/' + f) as lines:
                for l in lines:
                    output.write(l)

output.close()
