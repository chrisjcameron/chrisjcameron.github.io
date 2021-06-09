import random, math, numpy as np
import networkx
import networkx.utils
#from networkx.generators.classic import empty_graph

def random_stub_triangle_graph(s, t, seed=None):
    """Return a random graph G(s,t) with expected degrees given by s+2*t.
    
    :Parameters:
       - `s`: list - count of stubs emanating from node[i]
       - `t`: list - count of triangles including node[i]
       - `seed`: seed for random number generator (default=None)
    
    >>> z=[10 for i in range(100)]
    >>> G=nx.random_stub_triangle_graph
    
    Reference::
      @Article{arXiv:0903.4009v1,
        author =        {M. E. J. Newman},
        title =         {Random graphs with clustering},
        journal =       {},
        year =          {2009},
        volume =        {},
        pages =         {},
        }
    """
    
    if len(s) != len (t) :
        msg = "NetworkXError: stub and triangle vector must be same length"
        raise networkx.NetworkXError(msg)
    
    if sum(s)%2 != 0 :
        msg = "NetworkXError: sum(stubs) must be even"
        raise networkx.NetworkXError(msg)
    
    if sum(t)%3 != 0 :
        msg = "NetworkXError: sum(triangles) % 3 must be zero"    
        raise networkx.NetworkXError(msg)

    n = len(s)
    
    # allow self loops, exclude in later code
    G=networkx.empty_graph(n,create_using=networkx.Graph())
    G.name="random_stub_triangle_graph"
    
    
    if n==0 or (max(s)==0 and max(t)==0): # done if no edges
        return G
    
    
    #might not be needed
    #d = sum(s+2*t)
    #rho = 1.0 / float(d) # Vol(G)
    
    
    if seed is not None:
        random.seed(seed)
    
    # connect triangle corners
    #  Get a list of nodes that have triangle corners
    triNodes = [ x for x in range(n) if t[x]>0 ]
    tri = list(t)
    
    while len(triNodes) >= 3:
        [A,B,C] = random.sample(triNodes,3)
        #if not (G.has_edge(A,B) or G.has_edge(A,C) or G.has_edge(B,C)):
        G.add_cycle([A,B,C])
        for node in [A,B,C]:
            tri[node] -= 1
            if tri[node] == 0: triNodes.remove(node)
    
    # connect stubs
    #  Get a list of nodes that have stubs
    stubNodes = [ x for x in range(n) if s[x]>0 ]
    stubs = list(s)
    
    while len(stubNodes) >= 2:
        [A,B] = random.sample(stubNodes,2)
        #if not (G.has_edge(A,B)):
        G.add_edge(A,B)
        for node in [A,B]:
            stubs[node] -= 1
            if stubs[node] == 0: stubNodes.remove(node)
    
    """
    for node in xrange(n):
        for v in xrange(u,n):
            if random.random() < w[u]*w[v]*rho:
                G.add_edge(u,v)
    """
    return G


def max_clustering( degSeq ):    
    """ Return a valid degree sequence with high clustering.
    """
    # Floors given degree sequence, then pair as many edges as possible into 
    # triangle corners, assigns any left over edges an non-triangle edges 
    [t,s]=[list(a) for a in zip(*[divmod(math.floor(x),2) for x in degSeq])]
    # T must be a multile of 3
    removeT = int(sum(t)%3.0)
    removeS = int(sum(s)%2.0)
    
    for extra in range(removeT):
        edge = random.randint(0,sum(t))
        rmIndex = [ x>=edge for x in np.cumsum(t)].index(True)
        t[rmIndex] -= 1
    
    for extra in range(removeS):
        edge = random.randint(0,sum(s))
        rmIndex = [ x>=edge for x in np.cumsum(s)].index(True)
        s[rmIndex] -= 1
    
    return [t,s]














