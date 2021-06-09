import multiprocessing
import subprocess as sub
import sys
import os

# Run this file using 
# $ python runSim_batch.py '/Path/to/directory_with_options_files'


def execute_file(opt_file):
    try:
        print(opt_file)
        run_sim_sub = sub.Popen([
            "python", 
            "R:\\RDS\\code\\runsim\\runsim_cjc73_facebook_class.py",
            opt_file,  
        ])
        run_sim_sub.wait()
        return 1
            
    except Exception as detail:
        print(detail)
        raise

if __name__ == '__main__':
    # Create a MP pool of size multiprocessing.cpu_count() == # of cpu cores
    pool = multiprocessing.Pool(8)
    
    # Take the directory path passed in as an arg (assumes full path)
    # Get a list of all files
    # Filter list so only '.py' files are considered 'tasks' and sent as args to execute_file
    taskDir = sys.argv[1]
    files = ( os.listdir(taskDir) )
    tasks = [os.path.join(taskDir,f) for f in files if os.path.splitext(f)[1] in set(('.py',))]  
    tasks = [x for x in tasks if os.path.split(x)[1][0] != '_']
    
    print(tasks)
    # MP magic happens here
    results = []
    r = pool.map_async(execute_file, tasks, callback=results.append)
    r.wait() # Wait on the results
    
    # Results is a list of 1's. Could be more useful!
    # No idea what happens if there is an error!
    
    print(results)
    
