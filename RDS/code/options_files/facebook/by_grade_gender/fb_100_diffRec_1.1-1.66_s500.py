# -*- coding: utf-8 -*-
import networkx as nx
import os

# --------------------------------------------------
# load shared settings
shared_config_path = os.path.split(__file__)[0]
shared_config_name = "_fb_shared.py"
sh_conf = os.path.join(shared_config_path, shared_config_name)
sh_cfig_dict = runpy.run_path(sh_conf, locals())

# set up shared
sampling_params = sh_cfig_dict.get('sampling_params')
sim_params = sh_cfig_dict.get('sim_params')
if 'r_commands' in sh_cfig_dict:
    r_commands = sh_cfig_dict.get('r_commands')


sim_params['rec_parameters'] = [ 0,
        [('0','1'), ([1,1,1,1,1,1,1,1,1,2], [1,1,1,1,1,1,1,2,2,2,2,2,2,3,3])]]

# --------------------------------------------------

name = "fb_100_diffRec_1.1-1.66_s500.txt"
print( 'Executing Options: {}'.format(name))

outfile_name = sh_cfig_dict['out_file_namer'](name)
        
# sampling parameter values
not_batch_mode=True

rdsim_iter = sh_cfig_dict['rdsim_iter']
networks = sh_cfig_dict['networks']
# Create a generator to yield configured rdsim objects        
sims = rdsim_iter(networks, sim_params)

#No Char or newlines past here