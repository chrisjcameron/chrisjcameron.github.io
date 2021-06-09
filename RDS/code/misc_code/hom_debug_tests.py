import attdist, simrdclass_collisions_recactByNode as rds, recact, reccho, seedchoice, graphgen as gg, graphmod as gm #RDS-dependent modules
import itertools, time, sys, random, math, string, subprocess as sub, networkx as nx, os, numpy as np #general modules

#------------------------------------
# A network composed of two random networks
G0 = nx.barabasi_albert_graph( 2000 , 100)
G1 = nx.barabasi_albert_graph( 2000, 100)
net0 = rds.RDNet(network=G0, attLevels= attLevels)
net1 = rds.RDNet(network=G1, attLevels= attLevels)

# Assign the site variable
attdist.all( net0, attIndex=0, param='0' )
attdist.all( net1, attIndex=0, param='1' )

# Assign the variable correlated with site
attdist.all( net0, attIndex=1, param='0' )
attdist.all( net1, attIndex=1, param='0' )

attdist.randProp( net0, attIndex=1, param=[.75, '1' ])
attdist.randProp( net1, attIndex=1, param=[.25, '1' ])

# Assign the variable uncorrelated with site
attdist.randProp( net0, attIndex=2, param=[.5, '1' ])
attdist.randProp( net1, attIndex=2, param=[.5, '1' ])

net0.absorb(net1)
mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(), \
                  seedcho=seed_choice, seed_choice_params=seed_choice_params, \
                  rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=rec_method)
net0.clear()
net1.clear()
mySim.reID()


#Calculate the percent in-group ties
within = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]=='1' ]
without1 = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]!='1' ]
without2 = [x for x in mySim.network.edges() if x[0].attlist[2]!='1' and x[1].attlist[2]=='1' ]

float(len(within)) / (len(without1)+len(without2)+len(within))


#------------------------------------
# A single BA network
G0 = nx.barabasi_albert_graph( 2000 , 100)
net0 = rds.RDNet(network=G0, attLevels= attLevels)

# Assign the site variable
attdist.all( net0, attIndex=0, param='0' )

# Assign the variable correlated with site
attdist.all( net0, attIndex=1, param='0' )

attdist.randProp( net0, attIndex=1, param=[.75, '1' ])

# Assign the variable uncorrelated with site
attdist.randProp( net0, attIndex=2, param=[.5, '1' ])

mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(), \
                  seedcho=seed_choice, seed_choice_params=seed_choice_params, \
                  rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=rec_method)
net0.clear()
mySim.reID()

#Calculate the percent in-group ties
within = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]=='1' ]
without1 = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]!='1' ]
without2 = [x for x in mySim.network.edges() if x[0].attlist[2]!='1' and x[1].attlist[2]=='1' ]

float(len(within)) / (len(without1)+len(without2)+len(within))

#------------------------------------
# A single WS network
G0 = nx.connected_watts_strogatz_graph( 2000 , 190, p=.15)
net0 = rds.RDNet(network=G0, attLevels= attLevels)

# Assign the site variable
attdist.all( net0, attIndex=0, param='0' )

# Assign the variable correlated with site
attdist.all( net0, attIndex=1, param='0' )

attdist.randProp( net0, attIndex=1, param=[.75, '1' ])

# Assign the variable uncorrelated with site
attdist.randProp( net0, attIndex=2, param=[.5, '1' ])

mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(), \
                  seedcho=seed_choice, seed_choice_params=seed_choice_params, \
                  rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=rec_method)
net0.clear()
mySim.reID()

#Calculate the percent in-group ties
within = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]=='1' ]
without1 = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]!='1' ]
without2 = [x for x in mySim.network.edges() if x[0].attlist[2]!='1' and x[1].attlist[2]=='1' ]

float(len(within)) / (len(without1)+len(without2)+len(within))

#------------------------------------
# A single ER (random) network
G0 = nx.erdos_renyi_graph( 2000, .098)
net0 = rds.RDNet(network=G0, attLevels= attLevels)

# Assign the site variable
attdist.all( net0, attIndex=0, param='0' )

# Assign the variable correlated with site
attdist.all( net0, attIndex=1, param='0' )

attdist.randProp( net0, attIndex=1, param=[.75, '1' ])

# Assign the variable uncorrelated with site
attdist.randProp( net0, attIndex=2, param=[.5, '1' ])

mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(), \
                  seedcho=seed_choice, seed_choice_params=seed_choice_params, \
                  rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=rec_method)
net0.clear()
mySim.reID()

#Calculate the percent in-group ties
within = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]=='1' ]
without1 = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]!='1' ]
without2 = [x for x in mySim.network.edges() if x[0].attlist[2]!='1' and x[1].attlist[2]=='1' ]

float(len(within)) / (len(without1)+len(without2)+len(within))




#---------
# Other interesting checks:

#Does this capture all the edges
within2 =  [x for x in mySim.network.edges() if x[0].attlist[2]=='0' and x[1].attlist[2]=='0' ]
within =   [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]=='1' ]
without1 = [x for x in mySim.network.edges() if x[0].attlist[2]=='1' and x[1].attlist[2]!='1' ]
without2 = [x for x in mySim.network.edges() if x[0].attlist[2]!='1' and x[1].attlist[2]=='1' ]

within2_len    =    len(within2) 
within1_len    =    len(within) 
without1_2_len = len(without1)
without2_1_len = len(without2)

sum([within2_len, within1_len, without1_2_len, without2_1_len])
mySim.network.size()
# These numbers are equal, so these 4 lists encompass all the ties
