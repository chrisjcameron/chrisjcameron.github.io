import random, math

#functions for assigning distribution of attribute values across network
#takes in network of recruit objects and desired fraciton of "0" nodes
#the second param is only useful for ghetto function at this point

#does nothing (for speed)
def nothing(RDnet,param) :
    return 0

#random assignment
def randomAtts(RDnet,attIndex, param=['0','1']) :
    for i in RDnet.nodes() :
        i.attlist[attIndex] = random.choice(param)
    return 0

#random assignment with fixed equal proportions
def randomAtts_fixed(RDnet,attIndex, param=['0','1','3']) :
    net_size = len(RDnet.nodes())
    split = int(net_size/len(param))
    nodes = RDnet.nodes()
    random.shuffle(nodes)
    for i in range(len(nodes)):
        if i/split < len(param):
            nodes[i].attlist[attIndex] = param[i/split]
        else:
            # Last group gets any remainder
            nodes[i].attlist[attIndex] = param[i/split - 1]
            
    return 0


# works on a lattice, creates clustered ghetto of 1s
# assigns all else to 0s
def ghetto(RDnet, attIndex, param) :
    """assign proportion param[0] of nodes in net a value of param[1] at attIndex in a ghetto"""
    #these should become parameters later on
    per1 = param[0]
    #per0 = 1.0-per1
    curr1 = 0
    
    #start with a seed
    seedind = random.randint(1,len(RDnet.nodes()))
    seednode = RDnet.nodes()[seedind-1]
    seednode.attlist[attIndex] = 1
    currWave = [seednode]
    
    #until get necessary fraction
    while curr1*1.0/len(RDnet.nodes()) < per1 :
        #the next-current wave growth allows a smooth blossoming out of the 1 nodes
        nextWave = []
        for node in currWave :
            potentials = [x for x in RDnet.neighbors(node) if x.attlist[attIndex] != param[1]]
            for pot in potentials :
                pot.attlist[attIndex] = param[1]
                curr1+=1
            nextWave.extend(potentials)
        currWave = nextWave
            
            
def all(RDnet,attIndex,param=0):
    for node in RDnet.nodes():
        node.attlist[attIndex] = param

def randProp(RDnet,attIndex,param):
    """randomly assign proportion param[0] of nodes in RDnet a value of param[1] at attIndex"""
    for node in random.sample(RDnet.nodes(),int(math.floor(RDnet.network.order())*param[0])):
        node.attlist[attIndex] = param[1]
        
def assignFrom( RDnet, attIndex, att_List, node_id_start_index=1):
    for node in RDnet.nodes():
        #print( node.id )
        node.attlist[attIndex] = att_List[int(node.id)-node_id_start_index]

def calc_deg_grp(RDnet, attIndex, param):
    for node in RDnet.nodes():
        node.attlist[attIndex] = str(sum([ node.degree > x for x in param ]))





# Could do a set of random stars or blocks