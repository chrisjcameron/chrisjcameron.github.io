import random
import bisect
import networkx.utils as utils
## Recrutiment Priority Functions
""" 
  These functions accept:
  -- ego:  the recruiter (ego)
  -- potential:  list of neighbors - potential rectuits (potentials) 
  
  These functions return a list recruits ordered by recruitment priority
  
"""


def meth_random( ego, potentials, rec_meth_params=None ):
    random.shuffle(potentials)  # shuffle the list
    return potentials


def meth_var_matrix( ego, potentials, params=[ [0,1], {} ]):
    # Specify a different affinity for each combination of variable values
    # Params[0] is the attlist positions of the variables
    # Params[1] is the dictionary of weights keyed by tuples of ego_vals, pot_vals
    # {  (0,0):{(0,0):1, (0,1):1, (1,0):1, (1,1):1},
    #    (0,1):{(0,0):1, (0,1):1, (1,0):1, (1,1):1},
    #    (1,0):{(0,0):1, (0,1):1, (1,0):1, (1,1):1},
    #    (1,1):{(0,0):1, (0,1):1, (1,0):1, (1,1):1}   }
        
    # 2-Variable Case
    ego_vals = tuple( [ ego.attlist[x] for x in params[0] ] )
    weights = [ params[1][ego_vals][( tuple([pot.attlist[x] for x in params[0]]) )] for pot in potentials ]
    
    #Must be a more efficient way to do this....
    potentials_left = potentials
    results = []
    while len(potentials_left) > 0:
        cum_weights = list(utils.cumulative_sum(weights))
        rand_draw = random.random()*cum_weights[-1]
        sel_index = bisect.bisect(cum_weights,rand_draw)
        results.append(potentials_left[sel_index])
        del potentials_left[sel_index]
        del weights[sel_index]
        
        if len(potentials_left) == 1:
            results.append(potentials_left[0])
            potentials_left=[]
    
    return results
    


def meth_deg_bias( ego, potentials, rec_meth_params=[(9, 14),(2,1 )] ):
    ego_degree_group = sum([ int(ego.degree) > x for x in rec_meth_params[0] ])
    potentials_degree_group = [sum([ int(alter.degree) > x for x in rec_meth_params[0] ]) for alter in potentials]
    status = [ 0 if ego_degree_group==x else 1 for x in potentials_degree_group ]
    
    # Build a quick dictionary
    pot_dict = {}
    pot_dict[0] = [ potentials[i] for i in range(len(status)) if status[i]==0 ]
    pot_dict[1] = [ potentials[i] for i in range(len(status)) if status[i]!=0 ]
    
    # shuffle the lists
    for key in pot_dict.keys():
        random.shuffle(pot_dict[key]) 
    
    cutoff = float(rec_meth_params[1][0])/sum(rec_meth_params[1])
    counter_0 = 0
    counter_1 = 0
    results = []
    for i in range(len(status)):
        if random.random() <= cutoff:
            if counter_0 < len(pot_dict[0])-1 :
                results.append(pot_dict[0][counter_0])
                counter_0 +=1
            elif counter_1 < len(pot_dict[1])-1:
                results.append(pot_dict[1][counter_1])
                counter_1 +=1
        else:
            if counter_1 < len(pot_dict[1])-1:
                results.append(pot_dict[1][counter_1])
                counter_1 +=1
            elif counter_0 < len(pot_dict[0])-1 :
                results.append(pot_dict[0][counter_0])
                counter_0 +=1

    return results




def meth_deg_and_var_bias( ego, potentials, rec_meth_params=[(9, 14),(2,1 ),0,(2,1) ] ):
    # This is simple in/out group bias. 
    ego_degree_group = sum([ int(ego.degree) > x for x in rec_meth_params[0] ])
    potentials_degree_group = [sum([ int(alter.degree) > x for x in rec_meth_params[0] ]) for alter in potentials]
    
    deg_status = [ ego_degree_group==x for x in potentials_degree_group ]
    var_status = [ ego.attlist[rec_meth_params[2]]==x.attlist[rec_meth_params[2]] for x in potentials]
    
    # Build a quick dictionary
    pot_dict = {}
    pot_dict[(True,True)]   = [ potentials[i] for i in range(len(deg_status)) if deg_status[i] and var_status ]
    pot_dict[(True,False)]  = [ potentials[i] for i in range(len(deg_status)) if deg_status[i] and (not var_status)]
    pot_dict[(False,True)]  = [ potentials[i] for i in range(len(deg_status)) if (not deg_status[i]) and var_status ]
    pot_dict[(False,False)] = [ potentials[i] for i in range(len(deg_status)) if (not deg_status[i]) and (not var_status)]
    
    # shuffle the lists
    for key in pot_dict.keys():
        random.shuffle(pot_dict[key]) 
    
    deg_cutoff = float(rec_meth_params[1][0])/sum(rec_meth_params[1])
    var_cutoff = float(rec_meth_params[3][0])/sum(rec_meth_params[3])

    counter_dict = dict(zip(pot_dict.keys(), [0]*4))
    results = []
    for i in range(len(deg_status)):
        o_deg_roll = random.random() <= deg_cutoff
        o_var_roll = random.random() <= var_cutoff
        
        if counter_dict[(o_deg_roll, o_var_roll)] < len(pot_dict[(o_deg_roll, o_var_roll)]):
            results.append( pot_dict[(o_deg_roll, o_var_roll)][counter_dict[(o_deg_roll, o_var_roll)]] )
            counter_dict[(o_deg_roll, o_var_roll)] +=1
        
        else:
            order = random.randint(0,2)
            deg_roll = o_deg_roll
            var_roll = o_deg_roll
            if order == 0: deg_roll = not deg_roll
            else: var_roll = not var_roll   
            
            if counter_dict[(deg_roll, var_roll)] < len(pot_dict[(deg_roll, var_roll)]):
                results.append( pot_dict[(deg_roll, var_roll)][counter_dict[(deg_roll, var_roll)]] )
                counter_dict[(deg_roll, var_roll)] +=1
            
            deg_roll = o_deg_roll
            var_roll = o_deg_roll
            if order == 1: deg_roll = not deg_roll
            else: var_roll = not var_roll
            
            if counter_dict[(deg_roll, var_roll)] < len(pot_dict[(deg_roll, var_roll)]):
                results.append( pot_dict[(deg_roll, var_roll)][counter_dict[(deg_roll, var_roll)]] )
                counter_dict[(deg_roll, var_roll)] +=1
            
            deg_roll = not o_deg_roll
            var_roll = not o_deg_roll
            if counter_dict[(deg_roll, var_roll)] < len(pot_dict[(deg_roll, var_roll)]):
                results.append( pot_dict[(deg_roll, var_roll)][counter_dict[(deg_roll, var_roll)]] )
                counter_dict[(deg_roll, var_roll)] +=1

    return results


"""
THESE FUNCTIONS MUST BE UPDATED

def meth_time_biased( ego, potentials, rec ):

    global GH
    global GR
    global GP

    recruits = []
    weights = [[] for i in range(4)]
    # potentials has all legal recruits
    # check each network by priority
    #  until quota is satisfied
    rr = list((set(GR.neighbors(ego)).intersection(set(potentials))))
    rh = list((set(GH.neighbors(ego)).intersection(set(potentials))))
    rp = list((set(GP.neighbors(ego)).intersection(set(potentials))))
    
    for x in potentials:
        weights[sum([ x in rr, x in rh, x in rp])].extend([x]) 

    for i in range(rec):
        rrp = set(weights[3]).difference(recruits)
        if len(rrp)>1:
            recruits.extend(random.sample(rrp,1))
        else:
            rhp = set(weights[2]).difference(recruits)
            if len(rhp)>1:
                recruits.extend(random.sample(rhp,1))
            else:
                rpp = set(weights[1]).difference(recruits)
                if len(rpp)>1:
                    recruits.extend(random.sample(rpp,1))
                else:
                    gcp = set(weights[0]).difference(recruits)
                    if len(gcp)>1:
                        recruits.extend(random.sample(gcp,1))

    return recruits
    return potentials
"""

def meth_time_biased2( ego, potentials):
    """global GH
    global GR
    global GP

    recruits = []
    weights = [[] for i in range(2)]
    # potentials has all legal recruits
    # check each network by priority
    #  until quota is satisfied
    rh = list((set(GH.neighbors(ego)).intersection(set(potentials))))
    rp = list((set(GP.neighbors(ego)).intersection(set(potentials))))
    
    for x in potentials:
        weights[sum([ x in rp])].extend([x]) 

    for i in range(rec):
        rpp = set(weights[1]).difference(recruits)
        if len(rpp)>1:
            recruits.extend(random.sample(rpp,1))
        else:
            gcp = set(weights[0]).difference(recruits)
            if len(gcp)>1:
                recruits.extend(random.sample(gcp,1))

    return recruits"""
    return potentials
