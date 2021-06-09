# -*- coding: utf-8 -*-

## %python runSim_cjc73_facebook.py  options_file.py

#general modules
import time
import sys
import random
import math
import string
import subprocess as sub
import networkx as nx
import os
import numpy as np
import runpy
import argparse
import regex as re
import itertools as itr

if os.name == 'nt':
    sys.path.insert(0, os.path.normpath('R:/RDS/code/rdsim'))
#RDS-dependent modules
import attdist
import simrdclass_collisions_recactByNode as rds
import recact
import reccho
import seedchoice
import graphgen as gg
import graphmod as gm

print('\n')
report = False  #Should this script tweet an error?


def distTrans(x,a=None,b=None):
    if a==None: a=min(x)
    if b==None: b=max(x)
    mult = (b-a)/(max(x)-float(min(x)))
    offset = float(a) - min(x)*mult
    return x*mult + offset


def write_commented( in_file, out_file ):
    for line in in_file:
        out_file.write( "#  {}".format(line) )
    out_file.write('\n')


def write_R_commands(att_levels, r_commands, out_file):
    # Write the R commands to create attribute and level data
    #   Keys from attLevels into a list
    #   Values from attLevels into a list
    out_file.write( "-{}\n".format("attVals = list()"))
    # for atrib in reversed(attLevels):   # Reverse no longer necessary?
    for atrib in att_levels:
        line = ''.join(["attVals$", atrib[0], " = c('", "', '".join(atrib[1]),"')\n" ])
        out_file.write( "-{}".format(line) )
        
    for r_command in r_commands:
        out_file.write( "-{}".format(r_command) )
    # Absolutely must have exactly one blank line before putting in data
    out_file.write("\n")


def run_rdsim_with_params( rds_sim, sim_params, sim_data, out_file):
    '''Produce num_samples simulation runs
    Intended to be used with itertools.product to produce 
    combinations of parameter values.
    '''
    replacement_level = sim_params['replacement_level']
    #seed_count = sim_params['seed_count']
    sample_frac = sim_params['sample_frac']
    num_sam = sim_params['num_samples']
    if hasattr(num_sam, '__getitem__') and len(num_sam) == 1:
        num_samples = num_sam[0]
    elif isinstance(num_sam, int):
        num_samples = num_sam
    else:
        msg = "Parameter num_samples not int or container of len 1"
        raise RuntimeError(msg)
    
    trial_set = sim_data['trial_set']
    rewire_level = sim_data['rewire_level']
    actual_rewired = sim_data['actual_rewired']
    pop = sim_data['pop']
    tie = sim_data['tie']
    
    var_name = sim_data['var_name']

    diam = sim_data['diam']
    trans = sim_data['trans']
    net_size = sim_data['net_size']

    if sample_frac <= 1:
        sample_size = int(sample_frac*net_size)
    else:
        sample_size = sample_frac
        
    rds_sim.seeds = sim_params['seed_count']
    
    for i in range(num_samples):
        if replacement_level == 0:
            [ID, RID, atts, degree, wave, rec_count] = rds_sim.Simulate(
                                                        reclimit=sample_size, 
                                                        limit_sample_size=True, 
                                                        add_seeds=True) 
        else:
            [ID, RID, atts, degree, wave, rec_count] = rds_sim.Simulate(
                                                        reclimit=sample_size, 
                                                        replace="yes", 
                                                        limit_sample_size=True, 
                                                        add_seeds=True) 

        # Report Homophily, transitivity, diameter, simResults to out_file, print delimiter
        # 
        out_file.write(
            '{}\t{}\t{}\t'.format('trialSet', 'w_replace', 'target_sampFract'))
        out_file.write(
            '{}\t{}\t{}\t{}\t'.format(
                'target_rewire', 'swaps', 'diameter', 'trans'))
        #print "pop columns: ", pop.keys()
        if var_name:
            out_file.write('var_name\t')
        for popkey in rds_sim.recstrings:
            out_file.write('num{}\t'.format(popkey))
        # print( atts )
        for tiekey in rds_sim.rectemplate:
            out_file.write('stub{}\t'.format('_'.join(tiekey)))
            
        out_file.write('{}\t{}\t'.format('id', 'RID'))
            
        for attrib in ( x[0] for x in rds_sim.att_levels ):
            out_file.write('{}\t'.format(attrib))

        out_file.write('{}\t{}\t{}\n'.format('degree', 'wave', 'rec_count'))

        # print('%s\t%s\t%s\t%s\t%s\t%s\n' % (len(ID), len(RID), len(atts), len(atts2), len(degree), len(wave)) )
        for x in range(len(ID)):
            out_file.write('{}\t'.format(trial_set))
            out_file.write('{}\t'.format(replacement_level))
            out_file.write('{}\t'.format(sample_frac))
            out_file.write('{}\t'.format(rewire_level))
            out_file.write('{}\t'.format(actual_rewired))
            out_file.write('{}\t{}\t'.format(diam, trans))
            if var_name:
                 out_file.write('{}\t'.format(var_name))
            for popkey in rds_sim.recstrings:
                out_file.write('{}\t'.format(pop[popkey]))
            for tiekey in rds_sim.rectemplate:
                out_file.write('{}\t'.format(tie[tiekey]))
            #hack - string joining the atts output. This doesn't work if any attval > 1 character
            
            out_file.write('{}\t{}\t'.format(ID[x], RID[x]))

            for attrib in atts[x]:
                out_file.write('{}\t'.format(attrib))
                
            out_file.write(
                '{}\t{}\t{}\n'.format(degree[x], wave[x], rec_count[x]))
                    
        out_file.write('----\t\n')


def execute_sims_with_params(sims, sampling_param_space, out_file):
    ''' Take a set of rdsim objects and parameter space and 
    run simulations over the rdsims and param space
    '''
    trial_set = 0
    for sim in sims:
        sim_data = {}
        #--------------------------
        # Network specific values
        net_name = sim.network.name
        sim_data['net_size'] = sim.network.order()
        sim_data['pop'] = sim.pop_report_complex()
        
        # Name trial after the school ID num
        sim_data['trial_set'] = re.search('\w+?(\d+)', net_name).groups()[0]
        sim_data['rewire_level'] = 0
        
        # Would rewire here
        sim_data['actual_rewired'] = 0
        sim.resetNet() # Ensure network is pristine (needed?)
        
        # Measure Homophily, diameter, clustering
        #  if nx.is_connected(rds_sim.network): diam = nx.diameter(rds_sim.network)           
        sim_data['diam'] = 'NA'
        #  trans = nx.transitivity(rds_sim.network)
        sim_data['trans'] = 'NA' 
        sim_data['tie'] = sim.tie_report_complex()
        sim_data['var_name'] = sim.var_name
        
        #-----------------------------------
        # print("using replacement_level: {}".format(sampling_param_space['replacement_levels']))
        # Expand param space
        param_sets = itr.product(
            sampling_param_space['sample_size'],
            sampling_param_space['seed_count'],
            sampling_param_space['replacement_levels'],
            (sampling_param_space['num_samples'],)
        )
        
        for param_set in param_sets:
            keys = ('sample_frac', 'seed_count', 
                    'replacement_level', 'num_samples')
            sim_params = dict(zip(keys, param_set))

            # Run trials with parameters on this rds_sim
            run_rdsim_with_params(sim, sim_params, sim_data, out_file)

# Run Sims
try: 
    # load options from file passed as argument
    #   options file creates the output files
    not_batch_mode = False
    r_commands=[]

    if not "config_dict" in locals():
        config_path = os.path.abspath(sys.argv[1])
        not_batch_mode = True
        print(config_path)
        cfig_dict = runpy.run_path(config_path, locals())

    # print("outfile_name in cfig_dict: {}".format('outfile_name' in cfig_dict))
    # Enable dot-notation access to vals
    cf = argparse.Namespace(**cfig_dict)
    
    if "not_batch_mode" in cfig_dict:
        not_batch_mode = cf.not_batch_mode

    # Start sims
    with open(cf.outfile_name , 'w') as out_file:
        with open(config_path) as config:
            write_commented(config, out_file)
        if 'sh_conf' in cf:
            with open(cf.sh_conf) as sh_config:
                write_commented(sh_config, out_file)
            
        write_R_commands([('var', ('0','1'))], cf.r_commands, out_file)
        execute_sims_with_params(cf.sims, cf.sampling_params, out_file)
    
    est_with_r = sub.Popen([
        "C:\\Program Files\\R\\R-3.3.3\\bin\\Rscript.exe", 
        "R:\\RDS\\code\\est_r\\net_est_ss.r",
        "fileName='{}'".format(cf.name),  
    ])
    
    est_with_r.wait()  
    
except:
    raise
