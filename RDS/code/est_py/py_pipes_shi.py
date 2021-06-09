import subprocess, sys

def commandline_interface(ids, rids, degrees, variables, estimator, confidence_alpha, bootstraps, levels):
    netsize = len(ids)
    #print "here"
    if netsize!=len(rids) or netsize!=len(degrees) or netsize!=len(variables):
        print "data is wrong.."
        pass
    else:
        sample=str(netsize)+"\t"+estimator+"\t"+str(confidence_alpha)+"\t"+str(bootstraps)+"\t"
        for item in ids:
            sample+=str(item)+" "
        sample+="\t"

        for item in rids:
            sample+=str(item)+" "
        sample+="\t"

        for item in degrees:
            sample+=str(item)+" "
        sample+="\t"

        for item in variables:
            sample+=str(item)+" "
        sample+="\t"

        for item in levels:
            sample+=str(item)+" "

        #print str(sample)
        p1 = subprocess.Popen(["java", "rdsat", str(sample)], stdout=subprocess.PIPE)
        p1_out = p1.communicate()[0]   #Reading output directly can block. Com() makes sure command finishes
        
        #print(p1_out)

        #to parse the printout
        dat = p1_out[-200:].split("\n")   #only string.split() the last 200 characters to avoid making trash strings
        nlines = len(dat)
        
        #print(dat)

        results=[]
        for i in range(nlines-(len(levels)+1), nlines-1):
            if dat[i].__contains__("-"):
            	g = [float(0), float('nan'), float('nan')]
                results.append(g)
            else:
                g = [float(item) for item in dat[i].split(' ')[2:5]]
                results.append(g)
                
        return  results



def commandline_interface_aggregate(data_samples, popsizes, estimator, confidence_alpha, bootstraps, levels):

    if len(data_samples)<2:
        print "needs more than 2 data sets"
        pass
    else:
        sample_string = str(0)+"&&"+estimator+"&&"+str(confidence_alpha)+"&&"+str(bootstraps)+"&&"
        for popsize in popsizes:
            sample_string += str(popsize)+" "
        sample_string += "&&"
        
        for sample in data_samples:
            ids = sample[0]
            rids = sample[1]
            degrees = sample[2]
            variables = sample[3]
            netsize = len(ids)
            #print "here"
            if netsize!=len(rids) or netsize!=len(degrees) or netsize!=len(variables):
                print "data is wrong.."
                pass
            else:
                for item in ids:
                    sample_string+=str(item)+" "
                sample_string+="\t"
                for item in rids:
                    sample_string+=str(item)+" "
                sample_string+="\t"
                for item in degrees:
                    sample_string+=str(item)+" "
                sample_string+="\t"
                for item in variables:
                    sample_string+=str(item)+" "
                sample_string+="\t"
                for item in levels:
                    sample_string+=str(item)+" "
            sample_string+="&&"


        
        p1 = subprocess.Popen(["java", "aggregate", str(sample_string)], stdout=subprocess.PIPE)
        p1_out = p1.communicate()[0]   #Reading output directly can block. Com() makes sure command finishes
        #print p1_out
        

        #to parse the printout
        dat = p1_out[-200:].split("\n")   #only string.split() the last 200 characters to avoid making trash strings
        nlines = len(dat)
        
        results=[]
        for i in range(nlines-(len(levels)+1), nlines-1):
            #print dat[i]
            if dat[i].__contains__("-"):
            	g = [float(0), float('nan'), float('nan')]
                results.append(g)
            else:
                g = [float(item) for item in dat[i].split(' ')[2:5]]
                results.append(g)
        
        return  results
        

'''
if __name__ == '__main__':
    data_samples = [] 
    
    #prepare the data
    ids=[]
    rids=[]
    degrees=[]
    variables=[]
    popsizes=[]
    f = open('transformed_rds.txt','r')
    for count, line in enumerate(f):
        items=line.split()
        ids.append(items[1])
        rids.append(items[17])
        degrees.append(items[2])
        variables.append(items[11])
    f.close()
    
    data_samples.append([ids, rids, degrees, variables])
    popsizes.append(10000)
    data_samples.append([ids, rids, degrees, variables])
    popsizes.append(15000)
    #print data_samples

    estimator="DualComponent"
    confidence_alpha=0.025
    bootstraps=2500
    levels=[0,1,2]
    
    count = 0
    while count<10000:
        count+=1
        print count
        commandline_interface_aggregate(data_samples, popsizes, estimator, confidence_alpha, bootstraps, levels)

'''
    
    
    