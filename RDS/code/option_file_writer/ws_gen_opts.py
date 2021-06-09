
netSizes = {'N500':500, 'N5k':5000, 'N10k':10000}
rec_methods = {'rUnif':'recact.rec_fixed'}
rec_act_methods = {'c1':('recact.rec_fixed',1),
                  'c2':('recact.rec_fixed',2),
                  'c3':('recact.rec_fixed',3),
                  'cu04':('recact.rec_urand',[0,4]),
                  'cb04':('recact.rec_rand_choice',[0,4]),

netType = 'ws'
pop_split = [.8, .2]

def print_net_gen(outFile, net_size, pop_split):
    popA =  int(net_size*pop_split[0])
    popB =  int(net_size*pop_split[1])
    outFile.write( "def mySim_iter( numNets ):\n")
    outFile.write( "    for net in xrange(numNets):\n")
    outFile.write( "        G0 = nx.connected_watts_strogatz_graph( %s , 8, .2)\n" % popA)
    outFile.write( "        G1 = nx.connected_watts_strogatz_graph( %s, 8, .2)\n"  % popB)  
    outFile.write( "        net0 = rds.RDNet(network=G0, attLevels= attLevels)\n")
    outFile.write( "        net1 = rds.RDNet(network=G1, attLevels= attLevels)\n")
    outFile.write( "        attdist.all( net0, attIndex=0, param='0' )\n")
    outFile.write( "        attdist.all( net1, attIndex=0, param='1' )\n")
    outFile.write( "        net0.absorb(net1)\n")
    outFile.write( "        mySim = rds.RDSim(RDNet=net0, seeds=10, reclimit=net0.network.order(),  rec=recruitment_activity_method, recparams = recruitment_parameters, rec_meth=)\n")
    outFile.write( "        net0.clear()\n")
    outFile.write( "        net1.clear()\n")
    outFile.write( "        mySim.reID()\n")
    outFile.write( "        yield mySim\n\n")
    

def print_params(outFile, base_name, rec_act_meth, rec_act_param ):
    outFile.write( "print( 'Executing Options: %s')\n" % base_name )
    outFile.write( 'name = "%s.txt"\n' % base_name )
    outFile.write( "outFileName = string.join([\"/Volumes/TerraFirma/RDS/samples/\", name], '')\n")
    outFile.write( "if os.name == 'nt':\n")
    outFile.write( "    outFileName = os.path.normpath(''.join([\"C:/RDS/samples/\", name]))\n")
    outFile.write( "\n")
    outFile.write( "outFile = open(outFileName , 'w')\n")
    outFile.write( "\n")
    outFile.write( "rewireLevels = [.1] #[10**(x/10.0) for x in range(-20, 4, 1)]\n")
    outFile.write( "samplingFractions = [.5] # [.01,.05,.1]\n")
    outFile.write( "numNets = 10\n")
    outFile.write( "numSamples = 10 #500\n")
    outFile.write( "\n")
    outFile.write( "seeds = [5] #[1,2,5,7,10]\n")
    outFile.write( "recruitment_activity_method = %s\n" % rec_act_meth )
    outFile.write( "recruitment_parameters = %s\n" % rec_act_param )
    outFile.write( "replacementLevels =[0]  # choose with [1] or without [0] replacement or both [0, 1]\n")
    outFile.write( "\n")
    outFile.write( "rewire_method = gm.double_edge_swap\n")
    outFile.write( "rewire_params = [0] #this is changed to numSwaps in the runSim\n")
    outFile.write( "\n")
    outFile.write( "attLevels = [ ('aids', ('0','1'))  ]\n")