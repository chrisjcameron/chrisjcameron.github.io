import random
import itertools as itr
#import bisect
#import networkx.utils as utils
## Seed Selection Functions
""" 
  These functions accept:
  -- nodeList:  a list of nodes in the network
  -- seed_count:  how many seeds to select
  
  These functions return a list of seeds
  
"""


def seed_random( nodeList, seed_count, seed_choice_params=None ):  
    return random.sample(nodeList, seed_count)


##### Check these


def seeds_from_group( nodeList, seed_count, seed_choice_params={ 0:('0'), 1:('0') } ):
    potentials  = nodeList
    for key in seed_choice_params.keys():
        potentials = [ node for node in potentials if (node.attlist[key] in seed_choice_params[key]) ]
        
    return random.sample(potentials, seed_count)

def seeds_from_each_group( nodeList, seed_count, seed_choice_params=[['0','1'],['0','1']] ):
    unique_combs = list(itr.product(*seed_choice_params))
    seeds = []
    #print(unique_combs)
    #print(nodeList[0].attlist)
    for i in range(len(unique_combs)):
        potentials  = [ node for node in nodeList if tuple(node.attlist)==unique_combs[i] ]
        if len(potentials) >= seed_count :
            seeds.extend( random.sample( potentials, seed_count ))
    
    return seeds
    
def constant_seeds_from_group( nodeList, seed_count, seed_choice_params={ 0:('0'), 1:('0') } ):
    potentials  = nodeList
    for key in seed_choice_params.keys():
        potentials = [ node for node in potentials if (node.attlist[key] in seed_choice_params[key]) ]
        
    while True:
        seeds = random.sample(potentials, seed_count)
        yield seeds