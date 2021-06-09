import attdist, simrdclass_collisions_recactByNode as rds, recact, reccho, seedchoice, graphgen as gg, graphmod as gm #RDS-dependent modules
import time, sys, random, math, string, subprocess as sub, networkx as nx, os, numpy as np #general modules



recruitment_activity_method = recact.rec_degree_for_both_and_value_bias  
recruitment_parameters = [('degree', [(50,),(5, 1)] ), (0, [('0','1'), (1, 1)])]
rec_method = reccho.meth_random
rec_method_params = None


level = .1
rewire_method = gm.double_edge_swap #gm.between_group_edge_swap #

assortativity_params = [0.5]

attLevels = [ ('race', ('0','1',))] 

ass = 0
cross = 0
for ii in range(10):
    G0 = nx.barabasi_albert_graph( 1000, 40) 
    gm.assortativity_edge_swap(G0, assortativity_params)
    #print nx.degree_assortativity_coefficient(G0)
        
    G1 = nx.barabasi_albert_graph( 1000, 40) 
    gm.assortativity_edge_swap(G1, assortativity_params)
    #print nx.degree_assortativity_coefficient(G1)
        
    net0 = rds.RDNet(network=G0, attLevels= attLevels)
    net1 = rds.RDNet(network=G1, attLevels= attLevels)

    # Assign the site variable
    attdist.all( net0, attIndex=0, param='0' )
    attdist.all( net1, attIndex=0, param='1' )

    net0.absorb(net1)  
    mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(), rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=rec_method, rec_meth_params=rec_method_params)
    
    numSwaps = int( math.floor(level*mySim.network.size()/2) )

    actualRewired = mySim.alterNet(rewire_method, params = [numSwaps])

    ass += nx.degree_assortativity_coefficient(mySim.network)
    cross += mySim.tie_report_complex()['0','1']
    
print ass/10.0, cross/10.0