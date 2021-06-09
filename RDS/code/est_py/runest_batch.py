import multiprocessing
import sys, os, os.path
import time
# Seems to need these imported here
import attdist, simrdclass_collisions as rds, recact, reccho, graphgen as gg, graphmod as gm #RDS-dependent modules
import time, sys, random, math, string, subprocess as sub, networkx as nx, os, numpy as np #general modules


# Run this file using 
# $ python runSim_batch.py '/Path/to/directory_with_options_files'


def execute_file(file):
    try:
        print( file )
        # Code only run on windows
        if os.name == 'nt':
            foo = sub.Popen(["C:\\Program Files\\R\\R-2.13.0\\bin\\x64\\Rscript.exe", "C:\\RDS\\code\\net_est_cjc73_newCode_arith.r",'fileName="%s"' % file,  ])
            foo.wait()
        
        return 1
        
    except Exception as detail:
        print( detail)
        raise

if __name__ == '__main__':
    # Create a MP pool of size multiprocessing.cpu_count() == # of cpu cores
    pool = multiprocessing.Pool(None)
    
    # Take the directory path passed in as an arg (assumes full path)
    # Get a list of all files
    # Filter list so only '.txt' files are considered 'tasks' and sent as args to execute_file
    taskDir = sys.argv[1]
    files = ( os.listdir(taskDir) )
    tasks = [ f for f in files if os.path.splitext(f)[1] in ['.txt']]   
    
    print( tasks )
    # MP magic happens here
    results = []
    r = pool.map_async(execute_file, tasks, callback=results.append)
    r.wait() # Wait on the results
    
    # Results is a list of 1's. Could be more useful!
    # No idea what happens if there is an error!
    
    print results
    
