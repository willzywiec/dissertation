# copy.py
#
# Will Zywiec

import numpy as np

from os import getcwd, listdir, system

# custom function
from volley import *

path = getcwd()

seq = np.arange(25, 4025, 25).tolist()

for s in seq:
    system('cp single.py ' + str(s) + '/single.py')
    system('cp volley.py ' + str(s) + '/volley.py')
    volley(path + '/' + str(s))
