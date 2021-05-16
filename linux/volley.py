# volley.py
#
# refactored for Slurm

# 1. comment the last 3 lines
# 2. run this script
# 3. make sure the pbatch files are formatted correctly
# 4. uncomment the last 3 lines
# 5. run this script again

from os import getcwd, listdir, system

path = getcwd()

files = [f for f in listdir(path) if '.i' in f]
files = [f for f in files if '#' not in f]

files.sort()
# files.reverse()

sbatchText1 = '#!/bin/bash\n#SBATCH -t 24:00:00\n#SBATCH -p pbatch\n#SBATCH -A wbronze\n#SBATCH -N 1\n#SBATCH -n 16'
sbatchText2 = '\n\ndate\ncd '
sbatchText3 = '\nsrun -N 1 -n 16 /usr/apps/mcnp/bin/mcnp6.2.mpi inp='

sbatchIndex = []

def chunks(l, n):
	for i in range(0, len(l), n):
		yield l[i:i + n]

files = chunks(files, 500)

system('rm *.o')
system('rm *.srctp')
system('rm *.runtpe')
system('rm pbatch*')
system('rm quartz*')
system('rm slurm*')

i = 0

for file in files:
	fileName = 'pbatch' + str(i)
	sbatchIndex.append(fileName)
	sbatchFile = open(fileName, 'w')
	sbatchFile.write(sbatchText1 + sbatchText2 + path)
	for f in file:
		sbatchFile.write(sbatchText3 + f + ' out=' + f[:-2] + '.o srctp=' + f[:-2] + '.srctp runtpe=' + f[:-2] + '.runtpe tasks 16')
		sbatchFile.write('\npython single.py ' + f[:-2] + '.o ' + f[:-2] + '.txt')
		sbatchFile.write('\nrm slurm*')
	i += 1
	sbatchFile.close()

for s in range(0, 101):
        command = 'sbatch pbatch' + str(s)
        system(command)
