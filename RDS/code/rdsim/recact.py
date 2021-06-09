import random

#Methods to select how many recruitees a person recruits
def rec_fixed(params=1, node=None, degree=None):
    return params
    
def rec_urand(params=(1,1), node=None, degree=None):
    return random.randint(params[0],params[1])
    
def rec_rand_choice(params=[1], node=None, degree=None):
    return random.choice(params)

def rec_biased_choice(params=[(0,1,2), (.25, .5, .25)], node=None, degree=None):        
    # sum params[1] had better equal 1 or this does not work
    # validate higher up for speed, no validation here
    values = params[0]
    probs = params[1]
    draw = random.random()
    #s = [sum(probs[0:x+1]) for x in range(len(probs))]  ## cumulative sum
    #sum([x < 1 for x in s]) ## gives the index
    return values[ sum([draw > sum(probs[0:x+1]) for x in range(len(probs))]) ]

def rec_degree_and_value_bias(params=[('degree', .10 ), (1, [(0,1,2), (4 , 2 , 1)])], node=None, degree=None):
    # take the attribute value of node and use the params scale their recruitment based on their degree
    # take the degree of the node and scale recruitment activity by degree param
    node_attVal = node.attlist[params[1][0]]
    node_attVal_rec_count = params[1][2][ params[1][1].index(node_attVal) ]
    
    return degree * params[0][1] * node_attVal_rec_count

def rec_var_matrix(params=[[0,1], {}], node=None, degree=None):
    # take the attribute value of node and use the degree params to scale their recruitment based on their degree
    # take the degree of the node and scale recruitment activity by their degree category
    if node==None:
        #print('recact returning 0')
        return 0
    else:
        ego_vals = tuple([ node.attlist[x] for x in params[0] ])
        activity = params[1][ego_vals]
        return activity


def rec_value_bias(params=[1, [(0, 1), (4, 2)]], node=None, degree=None):
    # take the attribute value of node and use it to assign their recruitment activity
    if node==None:
        #print('recact returning 0')
        return 0
    else:
        node_attVal = node.attlist[params[0]]
        activity = params[1][1][ params[1][0].index(node_attVal) ]
        
        return int(activity)
        
def rec_value_bias_random(params=[0, [('0','1'), ([1,2], 1)]], node=None, degree=None):
    node_attVal = node.attlist[params[0]]
    node_attVal_rec_count = params[1][1][ params[1][0].index(node_attVal) ]    
    if isinstance(node_attVal_rec_count, list): 
        return random.choice(node_attVal_rec_count)
    else: 
        return node_attVal_rec_count
    
    
def rec_degree_for_value0_and_value_bias(params=[('degree', [(10,),(2,1)] ), (0, [('0','1'), (4 , 2)])], node=None, degree=None):
    # take the attribute value of node and use it to assign their recruitment activity
    # if the node is of attribute value 0, take the degree of the node and scale recruitment activity by degree param
    node_attVal = node.attlist[params[1][0]]
    node_attVal_rec_count = params[1][1][1][ params[1][1][0].index(node_attVal) ]    
    if node_attVal=='0': 
        mult = params[0][1][1][sum([ degree > x for x in params[0][1][0] ]) ]
    else: 
        mult = 1    
    return mult * node_attVal_rec_count
    
    
def rec_degree_for_value1_and_value_bias(params=[('degree', [(10,),(2,1)] ), (0, [('0','1'), (2 , 4)])], node=None, degree=None):
    # take the attribute value of node and use it to assign their recruitment activity
    # if the node is of attribute value 0, take the degree of the node and scale recruitment activity by degree param
    node_attVal = node.attlist[params[1][0]]
    node_attVal_rec_count = params[1][1][1][ params[1][1][0].index(node_attVal) ]    
    if node_attVal=='1': 
        mult = params[0][1][1][sum([ degree > x for x in params[0][1][0] ]) ]
    else: 
        mult = 1    
    return mult * node_attVal_rec_count
        
    
def rec_degree_for_both_and_value_bias(params=[('degree', [(10,),(2,1)] ), (0, [('0','1'), (4 , 2)])], node=None, degree=None):
    # take the attribute value of node and use it to assign their recruitment activity
    # take the degree of the node and scale recruitment activity by degree param
    node_attVal = node.attlist[params[1][0]]
    node_attVal_rec_count = params[1][1][1][ params[1][1][0].index(node_attVal) ]
    
    mult = params[0][1][1][sum([ degree > x for x in params[0][1][0] ]) ]
        
    return mult * node_attVal_rec_count
    
    
def rec_degree_and_value_bias_capped(params=[('cap', 3),('degree', [(9, 14),(1,2,4)] ), (1, [(0,1,2), (4 , 2 , 1)])], node=None, degree=None):
    # take the attribute value of node and use the degree params to scale their recruitment based on their degree
    # take the degree of the node and scale recruitment activity by their degree category
    if node==None:
        #print('recact returning 0')
        return 0
    else:
        node_attVal = node.attlist[params[2][0]]
        node_attVal_rec_count = params[2][1][1][ params[2][1][0].index(node_attVal) ]
        
        degree_rec_val = params[1][1][1][sum([ degree > x for x in params[1][1][0] ]) ]
        
        ret_val = min(round(degree_rec_val * node_attVal_rec_count), params[0][1])
        #print('recact returning %i' % ret_val)
        return int(ret_val)


def old_rec_degree_and_value_bias_capped(params=[('cap', 3),('degree', .10 ), (1, [(0,1,2), (4 , 2 , 1)])], node=None, degree=None):
    # take the attribute value of node and use the params scale their recruitment based on their degree
    # take the degree of the node and scale recruitment activity by degree param
    if node==None:
        #print('recact returning 0')
        return 0
    else:
        node_attVal = node.attlist[params[2][0]]
        node_attVal_rec_count = params[2][1][1][ params[2][1][0].index(node_attVal) ]
        
        ret_val = min(round(degree * params[1][1] * node_attVal_rec_count), params[0][1])
        #print('recact returning %i' % ret_val)
        return int(ret_val)

'''
#Not sure this works
def rec_foo(foo,x, node=None):
    return [foo(x) for x in range(1,10)]
'''

