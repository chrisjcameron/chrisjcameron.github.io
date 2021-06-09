import random
import math
import networkx as nx

def double_edge_swap( G, params ):
    nx.double_edge_swap( G, params[0], max_tries=params[0]*1000)
    return params[0]    
    
def between_group_edge_swap( G, params ): 
    # swaps in-group ties to make between-group ties
    G0_edges = [edge for edge in G.edges_iter() if edge[0].attlist[0]=='0']
    G1_edges = [edge for edge in G.edges_iter() if edge[0].attlist[0]=='1']
    
    # in case swap parameter is more than number of edges in either group:
    nswaps = min( [params[0], len(G0_edges), len(G1_edges)] )
    
    # need to oversample in case the swap creates an edge that already exists
    G0_sample = random.sample(G0_edges, nswaps + min( [int(nswaps*0.2), len(G0_edges)] ))
    G1_sample = random.sample(G1_edges, nswaps + min( [int(nswaps*0.2), len(G1_edges)] ))
    
    for i in range(nswaps):
        #gsize=G.size()
        a, b = G0_sample[i]
        c, d = G1_sample[i]
        if (d not in G[a]) and (b not in G[c]):
            G.add_edge(a, d)
            G.add_edge(c, b)
            G.remove_edge(a, b)
            G.remove_edge(c, d)
        else:
            # get edges from the end of the sample until the swapping produces new edges
            torepeat = True
            while torepeat:
                c, d = G1_sample.pop()
                if (d not in G[a]) and (b not in G[c]):
                    G.add_edge(a, d)
                    G.add_edge(c, b)
                    G.remove_edge(a, b)
                    G.remove_edge(c, d)
                    torepeat = False
    return nswaps
  
  
def assortativity_edge_swap( G, params ):   
    # increases assortativity in network by swapping param*edges double edges
    # to improve degree match
    # code closely follows code of double_edge_swap() in NetworkX

    # Instead of choosing uniformly at random from a generated edge list,
    # this algorithm chooses nonuniformly from the set of nodes with
    # probability weighted by degree.
    nswap = params[0]*G.size()
    swapcount = 0
    keys,degrees=zip(*G.degree().items()) # keys, degree
    cdf=nx.utils.cumulative_distribution(degrees)  # cdf of degree
    while swapcount < nswap:
        # pick two random edges without creating edge list
        # choose source node indices from discrete distribution
        (ai,ci)=nx.utils.discrete_sequence(2,cdistribution=cdf)
        if ai==ci:
            continue # same source, skip
        a=keys[ai] # convert index to label
        c=keys[ci]
        # choose target uniformly from neighbors
        b=random.choice(list(G[a]))
        d=random.choice(list(G[c]))
        if b==d or a==d or c==b:
            continue # same target, skip
        # make change if the distance between degrees decreases and neither of the new links already exists
        da = G.degree(a)
        db = G.degree(b)
        dc = G.degree(c)
        dd = G.degree(d)
        if (abs(da-dd) + abs(dc-db) < abs(da-db) + abs(dc-dd)) and (d not in G[a]) and (b not in G[c]):
            G.add_edge(a,d)
            G.add_edge(c,b)
            G.remove_edge(a,b)
            G.remove_edge(c,d)
            swapcount+=1  
            
            
def dissortativity_edge_swap( G, params ):   
    # increases assortativity in network by swapping param*edges double edges
    # to improve degree match
    # code closely follows code of double_edge_swap() in NetworkX

    # Instead of choosing uniformly at random from a generated edge list,
    # this algorithm chooses nonuniformly from the set of nodes with
    # probability weighted by degree.
    nswap = params[0]*G.size()
    swapcount = 0
    keys,degrees=zip(*G.degree().items()) # keys, degree
    cdf=nx.utils.cumulative_distribution(degrees)  # cdf of degree
    while swapcount < nswap:
        # pick two random edges without creating edge list
        # choose source node indices from discrete distribution
        (ai,ci)=nx.utils.discrete_sequence(2,cdistribution=cdf)
        if ai==ci:
            continue # same source, skip
        a=keys[ai] # convert index to label
        c=keys[ci]
        # choose target uniformly from neighbors
        b=random.choice(list(G[a]))
        d=random.choice(list(G[c]))
        if b==d or a==d or c==b:
            continue # same target, skip
        # make change if the distance between degrees decreases and neither of the new links already exists
        da = G.degree(a)
        db = G.degree(b)
        dc = G.degree(c)
        dd = G.degree(d)
        if (abs(da-dd) + abs(dc-db) > abs(da-db) + abs(dc-dd)) and (d not in G[a]) and (b not in G[c]):
            G.add_edge(a,d)
            G.add_edge(c,b)
            G.remove_edge(a,b)
            G.remove_edge(c,d)
            swapcount+=1 



def rewire_in_to_cross_ties(G, numRewire):
	#returns the number of ties rewired
	# Assume valid attvals are 0 or 1
	G0_nodes = [node for node in G.nodes() if node.attval == 0] 
	G1_nodes = [node for node in G.nodes() if node.attval == 1] 
	
	G0_edges = G.subgraph(G0_nodes).edges()
	G1_edges = G.subgraph(G1_nodes).edges()
	rewired = 0
	
	for i in range(numRewire):
		if (len(G1_edges) < 1 or len(G0_edges) < 1): break 
		(a,b) = random.choice(G0_edges)
		(c,d) = random.choice(G1_edges)
		
		if not ( G.has_edge(a,d) or G.has_edge(c,b) ):
			G.remove_edge(a,b)
			G.remove_edge(c,d)
			G0_edges.remove((a,b))
			G1_edges.remove((c,d))
			G.add_edge(a,d)
			G.add_edge(c,b)
			rewired += 1
	
	return rewired

def binary_rewire_within_to_cross_ties(G, params):
	#returns the number of ties rewired
	# params[0] = numSwaps
	# params[1] = attLevel index
	# params[2] = attValues
	# Assume valid attvals are 0 or 1
	G0_nodes = [node for node in G.nodes() if node.attval[params[1]] == params[2][0]] 
	G1_nodes = [node for node in G.nodes() if node.attval[params[1]] == params[2][1]] 
	
	G0_edges = G.subgraph(G0_nodes).edges()
	G1_edges = G.subgraph(G1_nodes).edges()
	rewired = 0
	
	for i in range(params[0]):
		if (len(G1_edges) < 1 or len(G0_edges) < 1): break 
		(a,b) = random.choice(G0_edges)
		(c,d) = random.choice(G1_edges)
		
		if not ( G.has_edge(a,d) or G.has_edge(c,b) ):
			G.remove_edge(a,b)
			G.remove_edge(c,d)
			G0_edges.remove((a,b))
			G1_edges.remove((c,d))
			G.add_edge(a,d)
			G.add_edge(c,b)
			rewired += 1
	
	return rewired


#I think this is just a clunkier version of Maslov-Sneppen
#but it allows me to control for rewiring on a per-node basis
def rewire_homophily(G, hlevel):
	G0_nodes = [node for node in G.nodes() if node.attlist[0] == '0']
	G1_nodes = [node for node in G.nodes() if node.attlist[0] == '1'] 
	rewirectr = 0
	for n in G.nodes():
		natt = n.attlist[0]
		if natt == '0': tarnodes = G0_nodes
		else: tarnodes = G1_nodes
		numedgestorewire = int( math.floor(hlevel*G.degree(n)) )
		htars = random.sample(tarnodes,numedgestorewire)
		nbrs = random.sample(G.neighbors(n),numedgestorewire)
		nbrctr = 0
		for htar in htars:
			secondedgeremoved = False

			#ignore edges that are already targeting similar-att node
			nbr = nbrs[nbrctr]
			if nbr.attlist[0] == natt: continue
			
			#ignore targets with insufficient degree
			if G.degree(htar) < 1: continue
			
			#pick a random diff-att edge of target
			tarnbrs = G.neighbors(htar)
			tarnbrs = [x for x in tarnbrs if x.attlist[0] != htar.attlist[0]]
			if len(tarnbrs) < 1: continue
			tarnbr = random.choice(tarnbrs)

			#remove source edge
			try:
				G.remove_edge(n,nbr)
			except:
				#edge has already been removed - don't add an extra edge, just continue
				continue
				
			#remove target edge
			try:
				G.remove_edge(htar,tarnbr)
				secondedgeremoved = True
			except:
				pass #sometimes the first remove_edge removes the second as well
				
			#add (source,target) edge
			G.add_edge(n,htar)

			#add in second edge to preserve balance
			if secondedgeremoved:
				G.add_edge(tarnbr,nbr)
			nbrctr+=1
			rewirectr+=1
	return rewirectr


# 
# 
# def rewire_to_among_ties(G, numRewire, attvals):
# 	#returns the number of ties rewired
# 	attvalNodes = dict.fromkeys(attvals)
# 	attvalEdges = dict.fromkeys(attvals)
# 	for key in attvalNodes.keys():
# 		attvalNodes[key] = [node for node in G.nodes() if node.attval == key] 
# 		attvalEdges[key] =  G.subgraph(attvalNodes[key]).edges()
# 	
# 	rewired = 0
# 	
# 	for i in xrange(numRewire):
# 		if (len(G1_edges) < 1 or len(G0_edges) < 1): break 
# 		(a,b) = random.choice(G0_edges)
# 		(c,d) = random.choice(G1_edges)
# 		
# 		if not ( G.has_edge(a,d) or G.has_edge(c,b) ):
# 			G.remove_edge(a,b)
# 			G.remove_edge(c,d)
# 			G0_edges.remove((a,b))
# 			G1_edges.remove((c,d))
# 			G.add_edge(a,d)
# 			G.add_edge(c,b)
# 			rewired += 1
# 	
# 	return rewired
