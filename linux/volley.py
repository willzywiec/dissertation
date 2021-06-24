# volley.py
#
# Will Zywiec

def volley(path):

    from os import getcwd, listdir, system

    # get all MCNP input decks
    files = [f for f in listdir(path) if '.i' in f]
    files = [f for f in files if '#' not in f]

    files.sort()
    # files.reverse() # option to reverse files (recommended if reruns are needed)

    print(path)

    # format pbatch text file
    pbatchText1 = '#!/bin/bash\n#SBATCH -t 24:00:00\n#SBATCH -p pbatch\n#SBATCH -A wbronze\n#SBATCH -N 1\n#SBATCH -n 16'
    pbatchText2 = '\n\ndate\ncd '
    pbatchText3 = '\nsrun -N 1 -n 16 /usr/apps/mcnp/bin/mcnp6.2.mpi inp='

    pbatchIndex = []

    # function to split up MCNP input decks into chunks (requires understanding of trade-off between wall-time and max job time limit)
    def chunks(l, n):
        for i in range(0, len(l), n):
            yield l[i:i + n]

    # 1 chunk = 350 MCNP input decks
    files = chunks(files, 350)

    # remove garbage
    system('rm ' + path + '/pbatch*')
    system('rm ' + path + '/quartz*')
    system('rm slurm*')

    i = 0

    # write pbatch files
    for f in files:
        fileName = 'pbatch' + str(i)
        pbatchIndex.append(fileName)
        pbatchFile = open(path + '/' + fileName, 'w')
        pbatchFile.write(pbatchText1 + pbatchText2 + path)
        for g in f:
            pbatchFile.write(pbatchText3 + g + ' out=' + g[:-2] + '.o srctp=' + g[:-2] + '.srctp runtpe=' + g[:-2] + '.runtpe tasks 16')
            pbatchFile.write('\npython cleanup.py ' + g[:-2] + '.o ' + g[:-2] + '.txt')
            # pbatchFile.write('\nrm slurm*')
        i += 1
        pbatchFile.close()

    # get all pbatch files
    pbatchFiles = [f for f in listdir(path) if 'pbatch' in f]

    # submit batch jobs
    for p in pbatchFiles:
        command = 'sbatch ' + path + '/' + str(p)
        system(command)
