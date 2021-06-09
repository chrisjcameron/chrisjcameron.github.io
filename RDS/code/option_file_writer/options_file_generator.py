# python option_file_generator.py 
import sys

if __name__ == '__main__':
    config = open( sys.argv[1] , 'r')
    exec config

    for net_size in netSizes.keys(): 
        for rec_meth in rec_methods.keys():
            for rec_act_method in rec_act_methods.keys():
                        
                base_name = '_'.join([netType, net_size, rec_act_method, rec_meth])
                outFileName = ''.join(['/Users/Shared/Vesta/code/options_files/opt_', base_name, '.py'])
                outFile = open(outFileName , 'w')
                
                print_params( outFile, base_name, rec_act_methods[rec_act_method][0], rec_act_methods[rec_act_method][1], rec_method = rec_methods[rec_meth] )
                outFile.write("\n\n\n")
                print_net_gen( outFile, netSizes[net_size], pop_split)
                
                outFile.write("\n\n#No Char or newlines past here")

                outFile.close()



#mlab.prctile(G0.degree().values(), p=(0, 33, 66, 100))