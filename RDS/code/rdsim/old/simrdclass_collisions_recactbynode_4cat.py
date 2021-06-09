#RDS simulator class
import random, networkx as nx, sys, itertools #, pylab as p
from types import GeneratorType
#these modules act as libraries of recruitment activity
#attribute assignment
#recruitment choice functions
#seed selection
import attdist, recact, reccho, seedchoice

#helper function for generating all possible recruitment combinations
#breadth first search; output format is depth-first however
#e.g. if we have:
#[('gender', ('male','female')), ('race', ('white','black','other')), ('aids', ('yes','no'))]
#then the leaves should be:
#'malewhiteyes','malewhiteno','maleblackyes','maleblackno',etc.
"""
# Retire this function, itertools can now do this inline
def genreccombs(attlisttree,biglist=[],outlist=[]):
    #print 'starting'
    templist = []
    #down the tree
    #print 'current level: ', attlisttree[0][1]
    for varval in attlisttree[0][1]:
        if not biglist:
            templist.append(varval)
            
    for big in biglist:
        for varval in attlisttree[0][1]:
            templist.append(big+varval)
        #print 'templist: ', templist

    biglist = templist
    #print 'biglist: ', biglist
    #print 'outlist: ', outlist
    outlist = biglist
    
    if len(attlisttree) > 1:
        return genreccombs(attlisttree[1:],biglist,outlist)
    else:
        #print 'final outlist: ', outlist
        return outlist
"""
#generates a new RDS simulator
#defaults: 
#recruitment network = empty
#number of seeds = 1
#seed choice = uniform at random from network
#reclimit = maximum number of recruits desired = -1 (no limit)
#recact = recruitment activity function = fixed (always recruit k)
#reccho = recruitment choice function = uniform at random from alters

#Recruit class:
# Need to add node parameters for recruit filtering and recruitability
#   rec_meth and rec look at these
#   also need fancy assignment fn for the filtering and recruitability when networks... 
#   modify att_dist att_assign
class Recruit:
    def __init__(self, id, attlist, degree=None):
        self.id = id
        self.attlist = list(attlist)  #Each Recruit needs their own attribute list!
        self.degree = degree

#RDS Network class
#this class should be used to create hierarchical / tiered networks
#the networks can then be combined using nx.union() or some such function
#and passed (after combination / manipulation) to RDSim object for simulation
#networks still have redundant id's which should be taken care of by reID for now
class RDNet:
    def __init__(self, network=nx.DiGraph(), attLevels = [('att1',('0','1')),('att2',('0','1'))] ) :

        #attribute list template tree
        #this structure governs the attlisttemplate
        #it is a representation of a complete n-ary tree as follows
        #[(first-level, (first-level-val-1,first-level-val-2...)), (second-level, ... ())]
        #for example: if we have race, gender, and aids status, we would show it as:
        #[('gender', ('male','female')), ('race', ('white','black','other')), ('aids', ('yes','no'))]
        #self.attlisttree = [('gender', ('male','female')), ('race', ('white','black','other')), ('aids', ('yes','no'))]
        self.attlisttree = attLevels
               
        #attribute list template
        #generated automatically from attribute dictionary template tree
        #by default assigns first value for each variable to attlisttemplate
        #e.g. in the example above, the corresponding attlisttemplate would be:
        #['male','white','yes']
        self.attlisttemplate = []
        for var in self.attlisttree :
            self.attlisttemplate.append(var[1][0])
               
        #network
        self.network = self.objNet(network)
        #erase self network so we don't keep 2 large objects in memory        
        network.clear()
        
        #debugging function - output an attlist from the network
        #self.printPop()
        
        #RDNet no longer assigns params, that is handled by the attdist module
        
    def nodes(self):
        return self.network.nodes()

    #transform network into network of objects with attributes (necessary for homophily, estimates)
    # cjc73 2009-5-20: Added a dictionary tracking system for graphs where IDs are not (0, N-1)
    def objNet(self,net) :
        if isinstance(net.nodes()[0], Recruit):
            return net.copy()
        else:
            objNet = nx.Graph()
            objDict = {}
            for i in net.nodes() :
                rec = Recruit(i,self.attlisttemplate, net.degree(i))
                objNet.add_node(rec)
                objDict[i] = rec
            for (i,j) in net.edges() :
                objNet.add_edge(objDict[i],objDict[j])
            return objNet 
             
    def printPop(self):
        print 'sample attlist: ', self.network.nodes()[0].attlist
        #for i in self.network.nodes():
        #    print '

    def absorb( self, netList ):
        """ Join the networks into one large network."""
        #BAD THINGS WILL HAPPEN if the nodes don't have the same attribute set.... \
        # - Should add code to check for mismatches """
        newNet = self.network.copy()

        if not isinstance(netList,list):
            netList=[netList]
        
        for net in netList:
            newNet = nx.union(newNet,net.network)
        
        self.network = newNet

    #Renumber node ID to eliminate duplicate ID in network
    # cjc73 2009-5-20: 
    def reID(self) :
        """ Most networks should be reIDed after absorbing other nets"""
        nodes = self.nodes()
        for i in xrange(len(nodes)):
            nodes[i].id = i
     
    def clear(self):
        self.network.clear()
        self.network = None
        self.attlisttree = None
        self.attlisttemplate = None
            
#RDS Simulation class        
class RDSim :
    def __init__(self, RDNet=None, reclimit=-1, seeds=1, seedcho=seedchoice.seed_random, seed_choice_params=None, \
    rec=recact.rec_fixed, recparams = 1, rec_meth=reccho.meth_random, rec_meth_params=None,  limit_sample_size=False, add_seeds=False ) :
        
        #master network
        self.master = RDNet.network.copy()   #Deep copy of all objects
        
        #Make a shallow copy of the master network of the same nx.Graph type as master
        self.network = self.master.__class__(self.master)
        
        #erase self network so we don't keep 2 large objects in memory        
        RDNet.network.clear()
        
        #Copy is not deep, objects in graph are same, only structure is copied
        
        #recruitment template (generated from attlisttree)
        self.node_value_list = list(itertools.product(*[x[1] for x in RDNet.attlisttree]))
        self.recstrings = [''.join(x) for x in self.node_value_list]
        # itertools replaces genreccombs(RDNet.attlisttree,[],[])  
         
        #correct rectemplate:
        self.rec_value_list = list(itertools.product(self.node_value_list, repeat=2))
        self.rectemplate = [(''.join(x[0]), ''.join(x[1]) ) for x in self.rec_value_list] 
        
        #example: nbr = (a,b); aids = (0,1)
        #output of rectemplate should be: [(a0,a0),(a0,a1),,(a1,a1),(b0,b0),(b0,b1),(b1,b1)]
        
        #error dummy var (because I don't want the initializer function to return stuff)
        self.error = 0
        
        #recruitment limit
        self.reclimit = reclimit
        if self.reclimit > self.network.number_of_nodes() :
            self.error = 1
        
        #seed choice function and params
        self.seedcho = seedcho
        self.seed_choice_params = seed_choice_params
        self.is_seed_gen = False  #Will autodetect and set correctly
        
        #recruitment activity function and params
        self.rec = rec
        self.recparams = recparams
        
        #recruitment choice function and params
        self.rec_meth = rec_meth
        self.rec_meth_params = rec_meth_params
        
        #number of seeds to start sample from
        self.seeds = seeds
        
        #How to handle when the sample is greater or less than size desired
        self.limit_sample_size = limit_sample_size
        self.add_seeds = add_seeds
        

        # These parameters are reinitialized by the reset() function
        #
        #
        
        #nodes in the sample already
        self.inSample = dict(zip(self.network.nodes(),[0]*len(self.network)))
        
        #list of nodes in sampled order (may contain duplicates)
        self.nodeList = [] 
        
        #seeds
        self.seedList = self.seedcho(self.network.nodes() , self.seeds, self.seed_choice_params)
        if isinstance(self.seedList, GeneratorType):
           self.seed_gen_next = self.seedList.next
           self.seedList = self.seed_gen_next()
           self.is_seed_gen = True

        #current recruits
        self.currentWave = self.seedList
        
        #upcoming recruits
        self.nextWave = []
        
        #ordered list of participant IDs
        self.ID = []
        
        ###output###
        #ordered list of participant recruiter IDs
        self.RID = []
        
        #ordered list of participant attribute values
        #list elements are attlists
        self.atts = []

        #ordered list of participant out-degrees
        self.degree = []
        
        #ordered list of participant recruited-in-waves
        self.wave = []
        ###end output###
        
        #number of recruits so far
        self.sampleSize = 0
        
        #current recruitment wave number
        self.waveNum = 0

        #initialize seeds
        self.popSeeds()
        

    #Renumber node ID to eliminate duplicate ID in network
    # cjc73 2009-5-20: 
    def reID(self) :
        nodes = self.master.nodes()
        for i in xrange(len(nodes)):
            nodes[i].id = i
            
    #initialize simulation with seed parameters
    def popSeeds(self) :
        #print 'seed list:', self.seedList
        for x in self.seedList: 
            self.ID.append(x.id)
            self.RID.append("NA")
            self.atts.append(x.attlist)
            self.wave.append(self.waveNum)
            self.degree.append(self.network.degree(x))
            self.inSample[x]=1   
            self.nodeList.append(x)
    
    #create a rewired network
    def alterNet(self, method, params):
        self.resetNet()
        ret = method(self.network, params)
        return ret
    
    #restore original network
    def resetNet(self):
        #Make a shallow copy of the master network of the same nx.Graph type as master
        self.network = self.master.__class__(self.master)
    
    #reset simulation instance
    def reset(self):
        #nodes in the sample already (only the seeds)
        self.inSample = dict(zip(self.network.nodes(),[0]*len(self.network)))
        
        #list of nodes in sampled order (may contain duplicates)
        self.nodeList = [] 
         
        #seeds
        # Get the first x seeds if the seed count changes, but keep them constant
        if self.is_seed_gen:
            self.seedList = list(self.seed_gen_next())[0:self.seeds]
        else:
            self.seedList = self.seedcho(self.network.nodes() , self.seeds, self.seed_choice_params)

        #current recruits
        self.currentWave = self.seedList
        
        #upcoming recruits
        self.nextWave = []
        
        #ordered list of participant IDs
        self.ID = []
        
        ###output###
        #ordered list of participant recruiter IDs
        self.RID = []
        
        #ordered list of participant attribute values
        #list elements are hashes (attlist)
        self.atts = []
        
        #ordered list of participant out-degrees
        self.degree = []
        
        #ordered list of participant recruited-in-waves
        self.wave = []
        ###end output###
        
        #number of recruits so far
        self.sampleSize = 0
        
        #current recruitment wave number
        self.waveNum = 0
        
        #initialize seeds
        self.popSeeds()
        self.sampleSize += len(self.seedList)
        

    #run simulation while a) reclimit is not reached, and b) recruits are being added    
    def Simulate(self, reclimit=None, replace="no", limit_sample_size=False, add_seeds=False ) :
        """ replace can be "yes", "no", <eventually: and "stepwise">
        """
        
        #How to handle when the sample is greater or less than size desired
        self.limit_sample_size = limit_sample_size
        self.add_seeds = add_seeds

        if reclimit==None:
            reclimit=self.reclimit
        self.reset()
        

        while self.sampleSize < reclimit and len(self.currentWave)>0:
            #increment wave number
            self.waveNum += 1

            #iterate over current recruits 
            
            for node in self.currentWave:
                if self.sampleSize >= reclimit: break 
                #Calculate the max number of recruits for this node. The recruitment activity 
                # function, self.rec(self.recparams) , returns an integer value.
                # NOTE: This value may be zero!
                rec_num = self.rec(params=self.recparams, node=node, degree=self.network.degree(node)) 
                
                #Skip to next node if the number of recruits is less than 1
                if rec_num < 1: continue
                
                #Grab the nodes neighbors. Nodes in the sample always have at least one neighbor.
                potentials = [x for x in self.network.neighbors(node)]  #contains some nodes already in sample

                #Generate list of potential recruits ordered by recruitment priority
                recruit_priority = self.rec_meth( node, potentials, self.rec_meth_params)   
                ############################################################
                #
                #
                #   EDIT HERE - if necessary
                #
                #
                #
                ############################################################
                
                #Sample Without Replacement or Without Replacement
                if (replace=="yes"):
                    recruits = recruit_priority[0:rec_num]
                    
                else:    #(replace=="no")
                    #get a list of non-recruited potential recruits and truncate to right size
                    recruits = [ x for x in recruit_priority if self.inSample[x] == 0 ]
                    if (len(recruits) > rec_num): recruits = recruits[0:rec_num]
                    
                    if recruits:
                        # update the recruitment attempt counts for the people already in the sample
                        for x in recruit_priority[0:recruit_priority.index(recruits[-1])]:
                            if self.inSample[x] > 0: self.inSample[x] += 1
                    else: # if recruits is empty
                        # update the recruitment attempt counts for the people already in the sample
                        # ie: we tried all the potential recruits but none were eligible
                        for x in recruit_priority:
                            if self.inSample[x] > 0: self.inSample[x] += 1
            
                
                #if still need to recruit more people
                if ((self.sampleSize <= reclimit) and recruits):  #bool(recruits) == True iff list is not empty     
                    #update upcoming recruits, sample size
                    self.nextWave.extend(recruits)
                    self.sampleSize += len(recruits)

                    #recruit all recruits
                    for x in recruits: 
                        #lf.write("adding recruit: " + str(x.id) + " of parent: " + str(node.id) + "\r\n")
                        
                        if x.attlist[0]=='0' and node.attlist[0]=='1':
                            newxattlist = ['2']
                        elif x.attlist[0]=='1' and node.attlist[0]=='0':
                            newxattlist = ['3']
                        else:
                            newxattlist = [x.attlist[0]]
			#print x.attlist, node.attlist, newxattlist
                        
                        # Update state list
                        self.inSample[x]=self.inSample[x]+1
                        self.nodeList.append(x)

                        # add ID, RID, wave info to results list
                        # ID, RID
                        self.RID.append(node.id)
                        self.ID.append(x.id)
                        self.atts.append(newxattlist)
                        self.degree.append(self.network.degree(x)) 
                        self.wave.append(self.waveNum)
                            
            self.currentWave = self.nextWave
            self.nextWave = []
            
            if self.add_seeds and len(self.currentWave) <= 0 and self.sampleSize < reclimit:
                # Recruitment chains are stuck and more nodes are needed!
                # Add a new seed from the unrecruited nodes
                
                unrecruited_list = [x for x in self.inSample.keys() if self.inSample[x]==0]
                if unrecruited_list:  #the list is not empty
    
                    new_seed = self.seedcho( unrecruited_list , 1, self.seed_choice_params) 
                    new_seed=new_seed[0]
                    
                    self.ID.append(new_seed.id)
                    self.RID.append("NA")
                    self.atts.append(new_seed.attlist)
                    self.wave.append(self.waveNum)
                    self.degree.append(self.network.degree(new_seed))
                    self.inSample[new_seed]=1   
                    self.nodeList.append(new_seed)
                    self.currentWave.append(new_seed)
            
        rec_count = [ self.inSample[x] for x in self.nodeList ]    
        
        # Python does not care if the index range extends beyond the range of the list
        #  It will just return the valid data values that lie withing the range
        if self.limit_sample_size:
            # If sample is too big, truncate. It is possible that 
            # rec_count may count attempted recruitment from the truncated tails 
            return [self.ID[0:reclimit], self.RID[0:reclimit], self.atts[0:reclimit], self.degree[0:reclimit], self.wave[0:reclimit], rec_count[0:reclimit]]
        else:
            return [self.ID, self.RID, self.atts, self.degree, self.wave, rec_count]

    #These functions should be wrapped into separate module
    # Raw Tie info to Calculate Homophily based on Heckathorn 2002
    def tie_report(self):
         # Need to make this more flexible for > 2 groups
         crossTies = {(0,0):0, (0,1):0, (1,0):0, (1,1):0}
         # Get each nodes value and a list of neighbors values
         for node in self.network.nodes():
              nValues = [x.attval for x in self.network.neighbors(node)]
              edgeValues = zip([node.attval]*len(nValues), nValues)
              for edge in edgeValues:
                   if crossTies.has_key(edge): crossTies[edge] += 1
                   else: crossTies[edge] = 1
         
         return crossTies
         
    # Raw Tie info to Calculate Homophily based on Heckathorn 2002
    def tie_report3(self):
         # for 3 groups
         crossTies = {(0,0):0, (0,1):0, (0,2):0, (1,0):0, (1,1):0, (1,2):0, (2,0):0, (2,1):0, (2,2):0}
         # Get each nodes value and a list of neighbors values
         for node in self.network.nodes():
              nValues = [x.attval for x in self.network.neighbors(node)]
              edgeValues = zip([node.attval]*len(nValues), nValues)
              for edge in edgeValues:
                   if crossTies.has_key(edge): crossTies[edge] += 1
                   else: crossTies[edge] = 1
         
         return crossTies
         
    # Raw tie info to Calculate Homophily for complex variable schemes
    """
    # 10-17-2012 Changed To Cycle via edges and made rectemplate a list rather than string
    # Not yet used, requires updating the runSim....py
    
    def tie_report_complex_new(self):
        crossTies = {rt:0 for rt in self.rec_value_list ]}
        for edge in self.network.edge():
            # Edge is a tuple of two node objects
            (fromNode, toNode) = edge
                crossTies[(fromNode, toNode)]+=1
                crossTies[(toNode, fromNode)]+=1
        #print 'crossTies: ', crossTies
        return crossTies
    """

    # Raw tie info to Calculate Homophily for complex variable schemes
    def tie_report_complex(self):
        crossTies = dict([ [rt,0] for rt in self.rectemplate ])
        for node in self.network.nodes():
            nodeattstr = str(reduce(lambda x,y: x+y,node.attlist))
            for nbr in self.network.neighbors(node):
                nbrattstr = str(reduce(lambda x,y: x+y, nbr.attlist))
                crossTies[(nodeattstr,nbrattstr)]+=1
        #print 'crossTies: ', crossTies
        return crossTies
         
    #Population composition for Homophily based on Heckathorn 2002
    def pop_report(self, atts2="no"):
        popValues = {}
        # Get each nodes value and a list of neighbors values
        if atts2=='yes':
            for node in self.network.nodes():
                if popValues.has_key((node.attval,node.attval2)): 
                    popValues[(node.attval,node.attval2)] +=1
                else: 
                    popValues[(node.attval,node.attval2)] =1   
        else:
            for node in self.network.nodes():
                if popValues.has_key(node.attval): 
                    popValues[node.attval] +=1
                else: 
                    popValues[node.attval] =1      
        return popValues
    
    #Population composition for Homophily based on Heckathorn 2002, complex values
    def pop_report_complex(self):
        popValues = dict([ [rs,0] for rs in self.recstrings ])
        #print(popValues)
        for node in self.network.nodes():
            #print 'attlist: ', node.attlist
            nodeattstr = reduce(lambda x,y: x+y,node.attlist)
            #print nodeattstr
            popValues[str(nodeattstr)]+=1
        #print 'popTies: ', popTies
        return popValues
         
        
if __name__ == '__main__' :
    ract = recact.rec_fixed
    mysim = RDSim(nx.fast_gnp_random_graph(1000, .01), attdist.nothing, 0, 50, 5, random.sample, ract, 1, reccho.meth_random)
