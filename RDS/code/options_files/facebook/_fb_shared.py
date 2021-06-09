
def out_file_namer(name):
    if os.name == 'nt':
        return os.path.normpath(os.path.join("R:/", "RDS", "samples", name))
    else: 
        return ''.join(["/Volumes/TerraFirma/RDS/samples/", name])

sampling_params = {
    'sample_size': [500],    # list
    'num_samples': 50, #200,     # int
    'seed_count': [5],         # list
    'replacement_levels': [0],  # choose with [1] or without [0] replacement or both [0, 1]
}

# 4 level version
sim_params = {
    # parameter function definitions 
    'rec_activity_method': recact.rec_value_bias_random,
    'rec_parameters': [ 0, [('0','1'), ([1], [1])]],
    'rec_method': reccho.meth_random,
    'rec_method_params': None,
    'att_levels': [ 
     ('fresh',    ('0','1')),
     #('sophomore', ('0','1')),
     #('junior',   ('0','1')),
     #('senior',   ('0','1')),
     ('sex',      ('0','1')),
     ('fr_m',     ('0','1')),
     ('fr_f',     ('0','1')),
     ('so_m',     ('0','1')),
    ]    
}

r_commands = [
"estVals = list()",
#'estVals$class = c("0", "1", "2", "3")',
#'estVals$sex  = c("0", "1")'
'estVals$sex_class = c("0", "1", "2", "3", "4", "5", "6", "7")'
]

networks = [
"American75", "Amherst41", "Auburn71", "BC17", "BU10",
"Baylor93", "Berkeley13", "Bingham82", "Bowdoin47",
"Brandeis99", "Brown11", "Bucknell39", "Cal65", "Caltech36",
"Carnegie49", "Colgate88", "Columbia2", "Cornell5",
"Dartmouth6", "Duke14", "Emory27", "FSU53", "GWU54",
"Georgetown15", "Hamilton46", "Harvard1", "Haverford76",
"Howard90", "Indiana69", "JMU79", "Johns Hopkins55",
"Lehigh96", "MIT8", "MSU24", "MU78", "Maine59",
"Maryland58", "Mich67", "Michigan23", "Middlebury45",
"Mississippi66", "NYU9", "Northeastern19", "Northwestern25",
"Notre Dame57", "Oberlin44", "Oklahoma97", "Penn94",
"Pepperdine86", "Princeton12", "Reed98", "Rice31",
"Rochester38", "Rutgers89", "Santa74", "Simmons81",
"Smith60", "Stanford3", "Swarthmore42", "Syracuse56",
"Temple83", "Tennessee95", "Texas80", "Texas84",
"Trinity100", "Tufts18", "Tulane29", "UC33", "UC61", "UC64",
"UCF52", "UCLA26", "UCSB37", "UCSC68", "UCSD34",
"UChicago30", "UConn91", "UF21", "UGA50", "UIllinois20",
"UMass92", "UNC28", "UPenn7", "USC35", "USF51", "USFCA72",
"UVA16", "Vanderbilt48", "Vassar85", "Vermont70",
"Villanova62", "Virginia63", "Wake73", "WashU32",
"Wellesley22", "Wesleyan43", "William77", "Williams40",
"Wisconsin87", "Yale4"
]

#networks = ["Reed98"]

#rds_iter should yield named networks
def rdsim_iter(network_names, sim_params):
    # parameter function definitions 
    for net_name in network_names:
        net_path = os.path.normpath("R:/RDS/facebook_data/graphml_gc/{}_gc.graphml".format(net_name))
        G =nx.read_graphml(net_path)
        
        if len(G.nodes()) < 2000: continue
        
        for nname in G.nodes():
            G.node[nname] = {k:int(v) for k,v in G.node[nname].items()}
        
        # Trim network as appropriate
        G.remove_nodes_from([i for i in G.nodes() if G.node[i]['student']!=1])
        G.remove_nodes_from([
                i for i in G.nodes() 
                if G.node[i]['year'] > 2009 or G.node[i]['year'] < 2006
            ])
        G = G.to_undirected()
        G = max(nx.connected_component_subgraphs(G), key=len)
        G.name = net_name
        
        
        # Assign nodes
        for node in G.nodes():
            n_atts = G.node[node]
            n_atts['fresh'] = str(int(n_atts['year']==2009))
            n_atts['sophomore'] = str(int(n_atts['year']==2008))
            n_atts['junior'] = str(int(n_atts['year']==2007))
            n_atts['senior'] = str(int(n_atts['year']==2006))
            # male & missing = 1, female = 0
            n_atts['sex'] = '0' if n_atts['gender']==1 else '1'
            
            #assign class
            if int(n_atts['fresh']):     n_atts['class'] = '0' 
            if int(n_atts['sophomore']): n_atts['class'] = '1' 
            if int(n_atts['junior']):    n_atts['class'] = '2' 
            if int(n_atts['senior']):    n_atts['class'] = '3'
               
            
            #assign gender/class pairs     
                        
            n_atts['sex_class'] = str( 
                2*int(n_atts['class']) + int(n_atts['sex'])
            )

            #assign special rec groups
            n_atts['fr_m'] = '1' if (n_atts['sex'] == '1') & (n_atts['class'] == '0') else "0"
            n_atts['fr_f'] = '1' if (n_atts['sex'] == '0') & (n_atts['class'] == '0') else "0"
            n_atts['so_m'] = '1' if (n_atts['sex'] == '1') & (n_atts['class'] == '1') else "0"
            

        for att_tup in sim_params['att_levels']:
            # print(att_tup)
            # RDNet may scramble node id numbers
            # print("G net size: ", G.order())
            base_net = rds.RDNet(
                    network=G.copy(),
                    attLevels=[ 
                        ('var', att_tup[1]), 
                        ('sex_class', ('0', '1', '2', '3', '4', '5', '6', '7')),
                    ],
                    att_pairs=[ att_tup[0], 'sex_class' ]
            )
            # print("Base_net_size: ", base_net.network.order())

            cur_sim = rds.RDSim(
                RDNet=base_net, 
                seeds=10, 
                reclimit=base_net.network.order(), 
                rec=sim_params['rec_activity_method'], 
                rec_params=sim_params['rec_parameters'], 
                rec_meth=sim_params['rec_method'],
                rec_meth_params=sim_params['rec_method_params']
            )

            base_net.clear()
            cur_sim.reID()
            cur_sim.var_name = att_tup[0]
            # print("Net_size: ", cur_sim.network.order())
            yield cur_sim

        G.clear()
