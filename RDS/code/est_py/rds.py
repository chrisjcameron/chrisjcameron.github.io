#RDS 1.1
#This version adds a switch to turn off RDS estimation (i.e. estimateRDS = True) in the "main_for_sim" function.
#By turning off RDS estimation, the program returns only sample characteristics such as cnum, ctie, stub...

#RDS 1.0
#This version fixes the adjusted phat function (Heckathorn 2007) to properly deal with multicategory variables.
#Also, this version adds Yongren's Voltz-Heckathorn 2008 estimation function.

#RDS 0.9
#This version adds code for calculating non-dichotomous (multicategory) variables.

#RDS 0.8
#This version fixes the degree categorization code so that the differences in consecutivedegree cutpoints are at least 1. 

#RDS 0.7
#This version fixes the bugs in the script so that the results match Chris's.

#RDS 0.6
#This version adds a def main_for_sim() function which is tailored to be embedded in Chris's
#RDS simulation data. 

"""
data = open('c://cornell//RDS//ws_N500_cu04_rUnif.txt', 'r')
t=0
for line in data:
    line=line[: -1]
    
    if t > 100000 and t<101000:
        print line
    if t == 101001:
        break
    t+=1
data.close()
"""

import time
import string
import numpy as np
from scipy import stats
import networkx as nx


#The following function returns a list with n elements where n is the number of columns. element is 0 if not digit, 1 if digit. nrows is the number of lines in the data (excluding the first line) to check.
def col_isdigit(file_name, nrows):   
    data = open(file_name, 'r')
    k=0
    dtype_test= []
    varname_list = []
    for line in data:
        line = line.strip()
        
        if k <= 10:
            varnames = line.split('\t')
            numcol = len(varnames)
            
            if k == 0:
                for i in range(numcol):
                    dtype_test.append( [ varnames[i] ] )
                    varname_list.append( varnames[i] )
            else:
                for i in range(numcol):
                    dtype_test[i].append( varnames[i] )
                    
            if k == nrows:
                var_isdigit = []
                for i in range(numcol):
                    digit_count = 0
                    for j in range(k):
                        if dtype_test[i][j+1].isdigit():
                            digit_count += 1
                    if digit_count != 0:
                        var_isdigit.append(1) #var_digit = 1 if there is at least one numeric value in the ith variable.
                    else:
                        var_isdigit.append(0) #var_digit = 0 if there is no numeric value in the ith variable.
                break    
        k+=1
    data.close()
    return var_isdigit


def col_isalpha(file_name, nrows):   
    data = open(file_name, 'r')
    k=0
    dtype_test= {}
    for line in data:
        line = line.strip()
        
        if k <= nrows:
            if k == 0:
                varnames = line.split('\t')
                numcol = len(varnames)
                for i in range(numcol):
                    dtype_test[ varnames[i] ] = []

            else:
                for i in range(numcol):
                    values = line.split('\t')
                    dtype_test[ varnames[i] ].append( values[i] )
                    
            if k == nrows:
                var_isalpha = []
                for i in range(numcol):
                    alpha_count = 0
                    for j in range(k):
                        if dtype_test[ varnames[i] ][j].isalpha():
                            alpha_count += 1
                    alpha_proportion = float(alpha_count) / k
                    if alpha_proportion > 0.6:
                        var_isalpha.append(1) #var_isalpha = 1 if more than 60% of the first k cases are alphabet
                    else:
                        var_isalpha.append(0) #var_isalpha = 0 if under 40% of the first k cases are alphabet
                break    
        k+=1
    data.close()
    return var_isalpha



#Read in the data to numpy arrays and return the arrays.
def read_data( rdsat_format, file_name, total_recruitment=0 ):
    
    if rdsat_format == True:  #If the data file is in RDSAT format (All values are numeric)
        
        data = open(file_name, 'r')
        k = -2
        for line in data:
            line = line.strip()
            if k == -1:
                header = line.split(' ')
                #print "header", header
                #print
                sample_size = int( header[0] )
                num_coupons = int( header[1] )
                missing_val = int( header[2] )
                varname = []

                vars()['ID'] = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                vars()['RID'] = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                vars()['Degree'] = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                coupons_ID = np.ma.MaskedArray( np.empty( (sample_size, num_coupons), dtype = float) ) #The coupons given to respondent
                received_coupon = np.ma.MaskedArray( np.empty( sample_size, dtype = float) ) #The coupon received from a recruiter
                    
                for i in range( len(header) - 3 ):
                    if '(' in header[i+3]:
                        cut = string.index( header[i+3], '(' )
                        if header[i+3][:cut] == 'Degree':  #Degree appears in the second column and in one of later columns. Therefore, create a separate degree array (Degree1) to store these.
                            Degree1 = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                            varname.append( 'Degree1' )
                        else:
                            vars()[ header[i+3][:cut] ] = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                            varname.append( header[i+3][:cut] )
                        #print header[i+3][:cut]
                        
                    else:
                        if header[i+3] == 'Degree':  #Degree appears in the second column and in one of later columns. Therefore, create a separate degree array (Degree1) to store these.
                            Degree1 = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                            varname.append( 'Degree1' )
                        else:
                            vars()[ header[i+3] ] = np.ma.MaskedArray( np.empty( sample_size, dtype = float ) )
                            varname.append( header[i+3] )
                
            elif k >= 0:
                row = line.split(' ')
                vars()['ID'][k] = float(row[0])
                vars()['Degree'][k] = float(row[1])
                received_coupon[k] = float(row[2])
                coupons_ID[k] = [ float( row[j+3] ) for j in range(num_coupons) ]
                
                for x in range( len( varname ) ):
                    vars()[ varname[x] ][k] = float( row[ 3 + num_coupons + x ] )
            k += 1
        data.close()
        
        #Find the ID of the recruiter (RID)
        for i in range(sample_size):
            index = np.where( coupons_ID == received_coupon[i] )[0]
            if len(index) == 1:  #If individual i is not a seed, "index" will give only one value.
                vars()['RID'][i] = vars()['ID'][ index[0] ]
            else:
                vars()['RID'][i] = 0

        #Export the raw data to file including RID
        chris = open('C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/ConvertingRDS/Jazz_with_RID.txt', 'w')
        chris.write( 'ID' + '\t' + 'RID' + '\t' + 'Degree' + '\t' + 'Gender' + '\t' + 'Race' + '\t' + 'Airplay' + '\t' + 'Union' + '\n' )
        for i in range( len( vars()['ID'] ) ):
            chris.write( str(vars()['ID'][i]) + '\t' + str(vars()['RID'][i]) + '\t' + str(vars()['Degree'][i]) + '\t' + str(vars()['Gender'][i]) + '\t' + str(vars()['Race'][i]) + '\t' + \
                         str(vars()['Airplay'][i]) + '\t' + str(vars()['Union'][i]) + '\n' )
        
                
        #Mask the missing values
        all_fields = ['ID', 'RID', 'Degree'] + varname
        #print "all fields", all_fields

        for var in all_fields:
            vars()[ var ] = np.ma.masked_where( vars()[ var ] == missing_val, vars()[ var ], copy = True )
        
        #Mask the degree value of cases that are seeds.
        vars()['Degree_seed_masked'] = vars()['Degree'].copy()
        for i in range( sample_size ):
            if vars()['RID'].mask[i] == True:  #If case i is a seed
                vars()['Degree_seed_masked'][i] = np.ma.masked  #Mask the degree value for that case.
        #for i in range(len(vars()['ID'])):
            #print vars()['ID'][i], vars()['Degree_seed_masked'][i], vars()['Airplay'][i]
        return vars()


       
    else:  #For simulated data or other formats
        var_isalpha = col_isalpha( file_name, 20 )
        
        data = open(file_name, 'r')
        k = -1
        for line in data:
            line = line.strip()
            row = line.split('\t')
            if k == -1:   #Don't process the first line in the raw data file because it contains variable names.
                varnames = list(row)
                print "Names of the columns:", varnames
                numcol = len(varnames)
                for i in range(numcol):
                    if var_isalpha[i] == 1:
                        vars()[ varnames[i] ] = np.ma.MaskedArray( np.empty( total_recruitment, dtype = 'S15' ) ) #Create an empty array for each column where the col name is identical to the first line in the input file
                    else:
                        vars()[ varnames[i] ] = np.ma.MaskedArray( np.empty( total_recruitment, dtype = 'float' ) )  #Create an empty array for each column where the col name is identical to the first line in the input file
            else:
                for i in range(numcol):
                    if var_isalpha[i] == 0:   #If the element is numerical
                        if row[i].isalpha() == False and row[i] != '':
                            vars()[ varnames[i] ][k] = row[i]

                        else:   #If a numeric variable contains alphabet, it is 'NA'. Treat this as missing.
                            vars()[ varnames[i] ][k] = -9999.  #Assign a numeric value instead of 'NA'
                            vars()[ varnames[i] ][k] = np.ma.masked  #Mask this element.
                            
                    else:  #If it is a categorical variable array
                        vars()[ varnames[i] ][k] = row[i]
                        if row[i] == 'NA':
                            vars()[ varnames[i] ][k] = np.ma.masked  #Mask this element.     
            k += 1

        data.close()    

        #Mask the degree value of cases that are seeds.
        vars()['degree_seed_masked'] = vars()['degree'].copy()
        for i in range( total_recruitment ):
            if vars()['RID'].mask[i] == True:  #If case i is a seed
                vars()['degree_seed_masked'][i] = np.ma.masked  #Mask the degree value for that case.
        #print "Aids", vars()['aids']
        return vars()

#Returns the longest wave number
def longest_wave(ID, RID):
    G = nx.DiGraph()
    ebunch = [ (RID[i], ID[i]) for i in range( len(ID) ) if RID.mask[i] == False ]
    G.add_edges_from( ebunch )
    #Check the longest path length
 
    

#Create Recruitment Matrix (categorical variable) and the ordered array of the variable values, and the transition matrix
def recruitment_matrix(ID, RID, Variable, var_values = None): 

    #If var_values are not provided as an argument, create it.
    if var_values == None:          
        #Get an array of unique Variable values (sorted)
        values = np.unique( Variable ) #This function returns a masked_array which extracts the unique values from the arg array (including mask value).
        
        #Count the number of non-masked elements of the array along the given axis. In this case, there's only one axis.
        num_val = values.count(axis=None)
        if len(values) != num_val:  #If there is a masked value, it will be at the end of the "values" array. Eliminate it.
            values = values[: -1]
    
    else:
        values = np.ma.masked_array( [ float(var_values[i]) for i in range( len(var_values) ) ] )
        num_val = len(values)
    
    #Assign indices for each non-masked variable value
    #valid_values = list(values[: num_val]) #excluding masked value
    lookup = {}
    for i in range(num_val):
        lookup[ values[i] ] = i  #key: variable value, value = assigned index (for rec)

    #Create empty recruitment matrix
    rec = np.zeros( (num_val, num_val), dtype = 'float' )

    #Populate the cells of the recruitment matrix with counts.
    for i in range( len(ID) ):
        try:  #Only do it where recruiter is not seed and where there are no missing values.
            recruited_val = Variable[ i ]
            recruiter_ID_index = np.ma.where( ID == RID[i] )[0][0]  #Find recruiter's ID index.
            #print recruiter_ID_index
            recruiter_val = Variable[ recruiter_ID_index ]

            #print ID[i], RID[i], recruited_val, recruiter_val, lookup[recruiter_val], lookup[recruited_val]
            
            rec[ lookup[recruiter_val] ][ lookup[recruited_val] ] += 1.
        except:
            pass
        
    """
    #If any cell in rec is 0, substitute 0.0001. (This is how Chris implements it in his R code)
    for i in range( num_val ):
        for j in range( num_val ):
            if rec[i][j] < 1 :
                rec[i][j] = 0.0001
    """
    
    return values, rec


# Demographically Adjust Recruitment Matrix (for multivalue variables)
def demographic_adjustment(rec_matrix):
    total_rec = np.sum( rec_matrix )
    selection_proportions = transition_matrix( rec_matrix )
    E = stationary1( selection_proportions )
    E_stack = np.repeat(E, len( E )).reshape(len(E), len(E))
    dem_adj = selection_proportions * E_stack * total_rec

    return dem_adj


# Data-Smoothing
def data_smoothing(dem_adj_matrix):
    return ( dem_adj_matrix + dem_adj_matrix.transpose() ) / 2.


#Construct Transition Matrix (Shat) from Recruitment Matrix
def transition_matrix(recruitment_matrix):
    #print "recruitment matrix", recruitment_matrix
    row_sum = recruitment_matrix.sum(axis = 1)
    sum_stack = np.repeat(row_sum, len(row_sum)).reshape(len(row_sum), len(row_sum))
    #print "recruitment matrix", recruitment_matrix
    #print "sum_stack", sum_stack
    #shat = np.divide( recruitment_matrix, sum_stack )
    shat = np.zeros( ( len(recruitment_matrix[0]), len(recruitment_matrix[0]) ), dtype = 'float' )

    for i in range(len(recruitment_matrix[0])):
        for j in range(len(recruitment_matrix[0])):
            if sum_stack[i][j] == 0. or recruitment_matrix[i][j] == 0.:
                shat[i][j] = 0.0001
            else:
                shat[i][j] = recruitment_matrix[i][j] / sum_stack[i][j]
      
    #If transition probability is 0 in a cell, change it to 0.0001. (consistent with Chris's code).
    #zero = (shat == 0.)  #zero is a boolean array of same dimension as shat. 0 elements in shat are True in zero.
    #shat[zero] = 0.0001  #Change the 0 elements to 0.0001
    #print "shat", shat
    
    return shat


#A function that solves the stationary (equilibrium) distribution of a finite markov chain
# from John Stachurski "Lectures on Computational Economics", mc_stationary1.py http://johnstachurski.net/lectures/markovdynam.html
def stationary1(shat):   #Use stationary2 or stationary3 for a large state space. Consult above website.
    """
    Parameters: 
    * p is a 2D NumPy array, assumed to be stationary (p is the transition matrix)
    Returns: A flat array giving the stationary distribution
    """
    N = len(shat)                               # p is N x N (transition matrix)
    I = np.identity(N)                       # Identity matrix
    B, b = np.ones((N, N)), np.ones((N, 1))  # Matrix and vector of ones
    A = np.transpose(I - shat + B) 
    solution = np.linalg.solve(A, b)
    return solution.flatten()                # Return a flat array

#Return the number of waves w required to achieve equilibrium within a given threshold level t.
def required_waves(shat, t=0.001):
    init_dist = np.zeros( len(shat) )
    init_dist[0] = 1
    # init_dist_matrix allows me to assess all starting distributions (100,010,001)...
    init_dist_matrix = np.identity( len(shat) )
    
    #current = np.dot(init_dist, shat) #Matrix multiplication for arrays
    current = np.dot(init_dist_matrix, shat) #Matrix multiplication for arrays
    w = 1
    reached_threshold = False
    while reached_threshold == False:
        next = np.dot(current, shat)
        if np.allclose(next, current, atol = t):  #Returns True if the two arrays are equal within the given tolerance. Substantively, this procedure returns the maximum number of required waves. Depending on starting distribution, some can converge quickly and some can converge after long waves. This procedure stops when the initial distribution which takes the longest converges. 
            reached_threshold = True
        else:
            current = next
            w += 1
    #print w
    return w


#Calculate sample proportions
def sample_proportion( variable, var_values ):
    sample_prop = np.zeros( len(var_values), dtype = float)
    sample_population_size = np.zeros( len(var_values), dtype = float )
    n = np.ma.count( variable )   #Count non-missing value elements (denominator)
    for i in range(len(var_values)):
        group_var_loc = np.ma.where(variable == var_values[i] ) #Returns a tuple where the 0th element contains indices of corresponding variable values.
        #print "group_variable_location for ", i, ":", group_var_loc
        
        count_each = np.ma.count( group_var_loc )
        #print i, count_each
        
        prop_each = float(count_each) / n
        sample_prop[i] = prop_each
        sample_population_size[i] = count_each
        #print "Number of individuals in the sample with variable value ", i, ":", count_each

    return sample_prop, sample_population_size

#Return array with the number of seeds possessing each var_value.
def seed_count(variable, var_values, RID):
    result = np.zeros( len(var_values), dtype = float )  #result container
    loc = np.ma.where( RID.mask == True)[0]
    seed_varval = variable[ loc ]  #Array for the variable values of the seeds.
    
    for i in range( len( var_values ) ):
        group_var_loc = np.ma.where(seed_varval == var_values[i] ) #Returns a tuple where the 0th element contains indices of corresponding variable values.
        count_each = np.ma.count( group_var_loc )
        result[i] = count_each
    return result
        
        
#Estimate the mean degree of groups using harmonic mean. (Heckathorn 2007)
def mean_degree(degree, degree_seed_masked, RID, variable, var_values, random_seed ):  #If seed is randomly selected(random_seed=True), seed's degree is used but with 0 weight.
    
    #Create an array (1 by n) to store the average degree for each of the n groups where n=number of groups.
    avg_degree_array = np.zeros( shape = ( var_values.count() ), dtype = 'float' )                  
    for i in range(len(var_values)):
        loc = np.ma.where(variable == var_values[i] )[0] #Returns a tuple where the 0th element contains indices of corresponding variable values.

        """
        #unmasked_loc = np.ma.where( RID.mask == False )
        # Take the intersection of group_var_loc (position index of variable value "i") and unmasked_loc (position index of the RID mask which is not NA).
        #loc = np.intersect1d( group_var_loc[0], unmasked_loc[0] ) #Take the 0th element from group_var_loc because it is a tuple (created from np.ma.where)
        """
        if random_seed == False:  #use degree_seed_masked
            #extract the elements that have the indices in loc
            group_degrees = degree_seed_masked[loc]
            #print i
            #print "group degrees", group_degrees
            #group_avg_degree = np.ma.average(group_degrees)

            #Calculate harmonic mean of group degree and place it in an array.
            #est_avg = float( group_degrees.count() ) / np.ma.sum( np.reciprocal( group_degrees ) )
            est_avg = stats.mstats.hmean(group_degrees, axis = 0) #Use scipy.stats.mstats module for calculating harmonic means in masked arrays.
            avg_degree_array[i] = est_avg
            
        else:  #If seed is random, use seeds' degree information
            group_degrees = degree[loc]
            pass

    return avg_degree_array


#Calculate the (traditional) estimated population proportions and transition matrix
def pop_estimates(degree_estimate,  shat):
    num_values = len( degree_estimate )

    #Create result array
    pop_est = np.zeros( num_values, dtype = float )
    
    if num_values > 2:  #For non-dichotomous variables
        coe = np.zeros( ( num_values, num_values), dtype = float )
        c1 = degree_estimate[0] * shat[0]   #This will be the elements that go into the first column of coe.
        coe[ : , 0] = c1
        
        diag = -1 * degree_estimate * shat[ : ,0]   #This will be the diagonal elements for coe.
        diagonal = np.diag_indices( num_values )  #This function gives the indices that point to diagonal elements
        coe[diagonal] = diag
        
        coe[0] = np.ones( num_values )  #Change the first row of coe to ones.

        b = np.zeros( num_values )
        b[0] = 1
        pop_est = np.linalg.solve( coe, b )
 
    else:   #For dichotomous variables
        """
        row_sum = rec_array.sum(axis = 1)
        sum_stack = np.vstack( (row_sum, row_sum)).transpose()

        #Element-wise division
        Shat = np.divide( rec_array, sum_stack )
        """

        #Calculate population estimate
        Phat_0 = (shat[1][0] * degree_estimate[1]) / ( shat[1][0]*degree_estimate[1] + shat[0][1]*degree_estimate[0] )
        Phat_1 = 1 - Phat_0
        pop_est[0] = Phat_0
        pop_est[1] = Phat_1
        
    return pop_est


#Voltz-Heckathorn RDS estimator function by Yongren
def rds_VH_estimate(variable, ID, recruiterID, degree):
    #refernce to the equation 9,  Volts and Heckathorn 2008
    #inputs are four np lists with same size. All the missing data point should be marked as masked.

    # count the group level
    groupCount=0
    groupNames=[]
    for item in set(variable):
        if item!='masked':
            groupCount+=1
            groupNames.append(item)

    sample_recruits_dist = [0. for i in range(groupCount)]
    sample_population_proportion = [0. for i in range(groupCount)]
    meanDegreeA = [0. for i in range(groupCount)]
    RDS2_estimate = [0. for i in range(groupCount)]
    
    Nu=0. # the n in the equation 9
    for i in range(len(variable)):
        if variable[i] is not np.ma.masked:
            Nu+=1

    NDu, sum_inverse_degree=0., 0.
    for i in range(len(variable)):
        if (variable[i] is not np.ma.masked) and \
           (ID[i] is not np.ma.masked) and \
           (recruiterID[i] is not np.ma.masked) and \
           (degree[i] is not np.ma.masked):
            sum_inverse_degree += 1/(degree[i])
            #print degree[i]
            NDu += 1
    meanDegreeU = NDu/sum_inverse_degree # delta_u in the eq 9
    
    for g in range(groupCount):
        NA=0. #n_A in the eq 9
        for i in range(len(variable)):
            if variable[i]==groupNames[g]:
                NA+=1
        sample_recruits_dist[g] = NA

        NDA, sum_inverse_degree=0., 0.
        for i in range(len(variable)):
            if (variable[i] == groupNames[g]) and \
               (ID[i] is not np.ma.masked) and \
               (recruiterID[i] is not np.ma.masked) and \
               (degree[i] is not np.ma.masked):
                sum_inverse_degree += 1/(degree[i])
                NDA += 1
        meanDegreeA[g] = NDA/sum_inverse_degree  # delta_A in eq9
        
        sample_population_proportion[g] = NA/Nu
        
        RDS2_estimate[g] = sample_population_proportion[g] * (meanDegreeU/meanDegreeA[g])

    RDS2_estimate1 = np.ma.masked_array(RDS2_estimate)  #Change the estimate list into an array for compatibility.
    #to print
    #print
    #print 'RDS 2 estimate: ', RDS2_estimate1
    #print 'mean network size(multiplicity):', meanDegreeA
    #print 'sample population proportion:', sample_population_proportion
    #print 'meanDegree ratio: ', meanDegreeU/meanDegreeA
    #print 'overall mean network size(multiplicity):', meanDegreeU            
    return RDS2_estimate1


#Calculate Sampling Weights and Dual Component Weights (RC and group-based & individual-based DC)
def weights(total_recruitment, RID, variable, var_values, degree_seed_masked, mean_group_degree, samp_prop, pop_est, Ehat):
    #Sample weights
    sample_weights = np.divide(pop_est, samp_prop)

    #Recruitment Component
    RC= np.divide( Ehat, samp_prop )
    #print "Group Recruitment Component", RC

    #Individual RC (based on group RC)
    Indiv_RC = np.ma.MaskedArray( np.empty( total_recruitment, dtype = 'float' ) )
    for i in range( total_recruitment ):
        #RC that corresponds to case i
        try:
            index = np.where( var_values == variable[i] )
            Indiv_RC[i] = RC[index]
        except:
            Indiv_RC[i] = np.ma.masked
    
    #Degree Component
    DC = np.divide( pop_est, Ehat )
    #print "Group Degree Component", DC

    #Constant K that is used for calculating individual-based degree components. Note that missing degree is imputed with average group degree.
    imputed_D = degree_seed_masked.copy()
    
    #Fill in imputed degree array where the masked (missing) degree values are imputed with the group mean degree.
    masked_degree_loc = np.where( degree_seed_masked.mask == True )[0]  # Identify the location of missing degree values. [0] is here because np.where returns a tuple.
    
    for each in masked_degree_loc:
        x = variable[ each ]  #Value of the respondent (ex. Male vs. female)
        var_loc = np.ma.where( var_values == x )[0]

        try:
            imputed_D[ each ] = mean_group_degree[ var_loc[0] ]
        except:
            imputed_D[ each ] = np.ma.masked
        
    K = float( total_recruitment ) / np.ma.sum( np.reciprocal( imputed_D ) * Indiv_RC  )  #Elementwise multiplication operator *
    #print "K constant", K
    #Dual Component Weight for Individuals
    Indiv_DW = Indiv_RC * np.reciprocal( imputed_D ) * K

    return Indiv_DW  #Return individual dual component weights


######### CREATE FUNCTION THAT CALCULATES AGGREGATION LEVEL (P. 179) #########
# Create cutpoints based on a given aggregation level (AL). Default AL = 12 as mentioned in Heckathorn(2007) and implemented by Chris.
def deg_cutpoint(degree, avg_cell):
    n = np.ma.count( degree ) #Number of non-masked elements
    AL =  (float(n) / avg_cell)**0.5
    if AL < 2:
        AL = 2
    else:
        AL = int(AL)
    
    prob = [ (1./AL)*i for i in range( 1, AL ) ]  #A list of q-quantiles to compute where q= AL
    quantiles = stats.mstats.mquantiles(degree, prob)
    
    #Get unique values as cutpoints.
    result = sorted( list( set(quantiles) ) )
    
    #If, for example, the cutpoint list is [7.0, 8.0, 8.8666666666666742, 9.0] or [7.0, 8.0, 8.866, 9.0, 9.4], this will create 5 categorical degree values instead of the total possible (i.e. 4). No agent can have a non-integer degree.
    #Therefore, we should eliminate the non-integer cutpoint.
    perfect = False

    if len(result) > 1: #If there is only one cutpoint in "result", we do not need to go into this loop.
        while perfect == False:
            for i in range(len(result) - 1):  
                if result[i+1] - result[i] < 1:
                    result.pop(i+1)
                    break
                else:
                    if i >= len(result) - 2:
                        perfect = True

    return result
    

#Categorize a continuous variable based on a given list of cutpoints
def categorize_degree(variable, avg_cell_size=12):
    #print "entering categorize degree"
    cutpoint_list = deg_cutpoint(variable, avg_cell = avg_cell_size)
    #print "degree cutpoint_list", cutpoint_list
    #print "original degree", variable
    
    #Create a categorical variable from continuous variable
    cat_var = np.ma.MaskedArray( np.zeros( len(variable) ) )
    
    if np.any( variable.mask ) == True:  #If there is at least one masked element in the variable
        for i in range( len(variable) ):
            
            if variable.mask[i] == True:
                cat_var[i] = np.ma.masked
                
            else:
                if len(cutpoint_list) == 1: #if there is only one cutpoint
                    if variable[i] <= cutpoint_list[0]:
                        cat_var[i] = 0
                    else:
                        cat_var[i] = 1
                else:
                    for j in range(len(cutpoint_list)):
                      
                        if j==0:
                            if variable[i] <= cutpoint_list[j] :
                                cat_var[i] = j
                        elif j == len(cutpoint_list)-1:
                            if variable[i] > cutpoint_list[j-1] and variable[i] <= cutpoint_list[j]:  
                                cat_var[i] = j
                            elif variable[i] > cutpoint_list[j] :
                                cat_var[i] = j+1
                        else:
                            if variable[i] > cutpoint_list[j-1] and variable[i] <= cutpoint_list[j]:                
                                cat_var[i] = j

    else:  #If there are no masked elements
        for i in range( len(variable) ):
            if len(cutpoint_list) == 1:  #If there is only one cutpoint in the list, hard-code the cat_var values
                if variable[i] <= cutpoint_list[0]:
                    cat_var[i] = 0
                else:
                    cat_var[i] = 1
            else:
                for j in range(len(cutpoint_list)):
                        if j==0:
                            if variable[i] <= cutpoint_list[j] :
                                cat_var[i] = j
                        elif j == len(cutpoint_list)-1:
                            if variable[i] > cutpoint_list[j-1] and variable[i] <= cutpoint_list[j]:  
                                cat_var[i] = j
                            elif variable[i] > cutpoint_list[j] :
                                cat_var[i] = j+1
                        else:
                            if variable[i] > cutpoint_list[j-1] and variable[i] <= cutpoint_list[j]:                
                                cat_var[i] = j
            

                            
    #print "raw degree", variable
    #print "categorical variable list", cat_var
    return cat_var


# Calculate Recruitment Component for Degree
def RC_degree(ID, RID, degree, variable, avg_cell_size=12):  #Use the raw degree array where seed's degree is not masked!!
    
    #Create a recruitment matrix by degree
    categorical_degree = categorize_degree(degree, avg_cell_size)
    #print "categorical_degree", categorical_degree
    
    var_values, rec = recruitment_matrix(ID, RID, categorical_degree)  #Note that seed's degree is also used for constructing recruitment matrix (rec).
    #print "RC_degree's Recruitment matrix", rec
    shat = transition_matrix(rec)
    
    #print "In RC_degree: rec, var_values, shat", rec, var_values, shat
    
    C, samp_pop_size = sample_proportion( categorical_degree, var_values )
    #print "C", C

    E = stationary1(shat)
    #print "E", E

    RCD = np.ma.divide(E, C)
    #print "RCD", RCD
    
    Indiv_RCD = np.zeros( len( categorical_degree), dtype = float )
    for i in range( len( categorical_degree ) ):
 
        if np.any(RID.mask) == False:
            if np.any(degree.mask) == False:
                Indiv_RCD[i] = RCD[ int(categorical_degree[i]) ]
            else:
                if degree.mask[i] == False:
                    Indiv_RCD[i] = RCD[ int(categorical_degree[i]) ]
                else:
                    Indiv_RCD[i] = 0

        else:
            if RID.mask[i] == False:
                if np.any(degree.mask) == False:
                    Indiv_RCD[i] = RCD[ int(categorical_degree[i]) ]
                else:
                    if degree.mask[i] == False:
                        Indiv_RCD[i] = RCD[ int(categorical_degree[i]) ]
                    else:
                        Indiv_RCD[i] = 0
            else:
                Indiv_RCD[i] = 0
                
    return Indiv_RCD


# Calculate Adjusted Degree for each group
def adjusted_mean_degree(degree_seed_masked, indiv_RCD, variable, var_values):
    adj_mean_degrees = np.zeros( len(var_values), dtype = float)
    for i in range(len(var_values)):
        loc = np.ma.where(variable == var_values[i] ) #Returns a tuple where the 0th element contains indices of corresponding variable values.
        
        #extract the degree elements that have the indices in loc
        group_RCD = indiv_RCD[loc]
        group_degrees = degree_seed_masked[loc]

        adj_mean_degree = np.ma.divide( np.sum( group_RCD ), np.sum( np.ma.divide( group_RCD, group_degrees ) ) )
        adj_mean_degrees[i] = adj_mean_degree
    
    return adj_mean_degrees


#Calculate Adjusted Population Estimate
def adjusted_pop_estimates(adj_mean_degree, shat):   
    adj_pop_est = pop_estimates(adj_mean_degree, shat)  # In case of a multicategory variable, the shat here derives from an already demographically adjusted and smoothed recruitment matrix (see main_for_sim).
    return adj_pop_est


# Calculate Adjusted Dual-Component Weight: ADW_i = DW_i * ( Adj_Phat / Phat ) 
def adjusted_DC_weight(indiv_DW, adj_pop_est, original_pop_est, variable, var_values ): 
    adj_DC_weight = np.zeros( len(variable) )

    for i in range(len(variable)):
        index = np.where( var_values == variable[i] )
        try:
            adj_DC_weight[i] = indiv_DW[i] * ( adj_pop_est[ index ] / original_pop_est[ index ] )
        except:  #If index is missing (variable[i] is masked)
            adj_DC_weight[i] = np.ma.masked
            
        
    return adj_DC_weight


# Dual-Homophily Measures (Coleman Homophily, Affiliation Homophily, Degree Homophily)
def homophily(shat, phat, ehat, samp_pop_size):
    #Blau homophily is simply the diagonals on shat.
    BH = np.diag(shat)

    #Coleman Homophily
    CH = np.zeros( len(BH) )
    for i in range( len(BH) ):
        if BH[i] >= phat[i]:
            CH[i] = ( BH[i] - phat[i] ) / (1 - phat[i] )
        else:
            CH[i] = ( BH[i] / phat[i] ) - 1
            
    #Coleman Homophily which uses sample population size instead of phat       
    sample_phat = np.zeros( len(BH) )
    denom = np.sum(samp_pop_size)
    for i in range(len(BH)):
        sample_phat[i] = samp_pop_size[i] / denom
    CH0 = np.zeros( len(BH) )
    for i in range(len(BH)):
        if BH[i] >= sample_phat[i]:
            CH0[i] = ( BH[i] - sample_phat[i] ) / (1 - sample_phat[i] )
        else:
            CH0[i] = ( BH[i] / sample_phat[i] ) - 1
        
    #Affiliation Homophily
    AH = np.zeros( len(BH) )
    for i in range( len(BH) ):
        if BH[i] >= ehat[i]:
            AH[i] = ( BH[i] - ehat[i] ) / (1 - ehat[i] )
        else:
            AH[i] = ( BH[i] / ehat[i] ) - 1
            
    #Degree Homophily
    DH = np.zeros( len(ehat) )
    for i in range( len(ehat) ):
        if ehat[i] >= phat[i]:
            DH[i] = ( ehat[i] - phat[i] ) / (1 - phat[i] )
        else:
            DH[i] = ( ehat[i] / phat[i] ) - 1

    return CH, AH, DH, CH0


def main(file_name, degree_cutpoint, rdsat_format = True, varnames= ['whatever'], total_recruitment = 500):
    if rdsat_format:
        #file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/ConvertingRDS/nyjazz.txt'
        data = read_data( rdsat_format = True, file_name = file_name, total_recruitment=0 )
        sample_size = data['sample_size']

        for var in varnames:
            var_values, rec = recruitment_matrix(data['ID'], data['RID'], data[var])
            
            if len(var_values) > 2:
                dem_adj_rec = demographic_adjustment(rec)
                smoothed_rec = data_smoothing(dem_adj_rec)
                Shat = transition_matrix(smoothed_rec)
            else:
                Shat = transition_matrix(rec)
                
            Shat = transition_matrix(rec)
            Ehat = stationary1(Shat)
            group_degree = mean_degree( data['Degree'], data['Degree_seed_masked'], data['RID'], data[var], var_values, random_seed = False )
            Phat = pop_estimates(group_degree,  Shat)
            samp_proportions, samp_pop_size = sample_proportion( data[var], var_values )
        
            Indiv_RC_Degree = RC_degree(data['ID'], data['RID'], data['Degree'], data[var], degree_cutpoint)
            Adj_group_degree = adjusted_mean_degree(data['Degree_seed_masked'], Indiv_RC_Degree, data[var], var_values)
            Indiv_DW = weights(sample_size, data['RID'], data[var], var_values, data['Degree_seed_masked'], group_degree, samp_proportions, Phat, Ehat) #Unadjusted DW
            Adj_Phat = pop_estimates( Adj_group_degree, Shat)
            Adj_Indiv_DW = adjusted_DC_weight(Indiv_DW, Adj_Phat, Phat, data[var], var_values )
            CH, AH, DH, CH0 = homophily(Shat, Phat, Ehat, samp_pop_size)

            print "Variable to Analyze:", var
            print
            print "Recruitment:", rec
            print
            print "Shat:",Shat
            print
            print "Ehat:", Ehat
            print
            print "Average Group Degree:",group_degree
            print
            print "Phat", Phat
            print
            print "Sampling Proportions:", samp_proportions
            print
            print "Unadjusted Individual dual-component weights,"
            print Indiv_DW
            print
            print "Individual Recruitment Component for Degree"
            print Indiv_RC_Degree
            print
            print "Adjusted Average Group Degree:", Adj_group_degree
            print
            print "Adjusted Phat:", Adj_Phat
            print
            print "Adjusted Individual dual-component weights,"
            print Adj_Indiv_DW
            print
            print "Coleman Homophily", CH
            print
            print '--------------------------------------'
            print
        
    else:
        #file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/ConvertingRDS/test_input2.txt'  #This is one of Chris's simulated data
        #total_recruitment = 500  # This is the number of recruitments
                                # which should be supplied in advance
                                # in order to increase efficiency of creating array.
        time_start = time.time()
        data = read_data( False, file_name, total_recruitment )

        print file_name
        print "Time spent on reading in data file:", time.time()-time_start
        print
        
        for each in varnames:
            print "Variable to analyze:", each
            
            time0 = time.time()
            var_values, rec = recruitment_matrix(data['id'], data['RID'], data[each])
            print "Time spent on creating recruitment matrix:", time.time() - time0

            time0 = time.time()
            if len(var_values) > 2:
                dem_adj_rec = demographic_adjustment(rec)
                smoothed_rec = data_smoothing(dem_adj_rec)
                Shat = transition_matrix(smoothed_rec)
            else:
                Shat = transition_matrix(rec)
            print "Time spent on calculating Shat:", time.time()-time0
            print

            time0=time.time()
            group_degree = mean_degree( data['degree'], data['degree_seed_masked'], data['RID'], data[each], var_values, random_seed = False )
            print "Time spent on calculating average group degree:", time.time()-time0
            print
            
            Ehat = stationary1(Shat)           
            Phat = pop_estimates(group_degree,  Shat)
            
            samp_proportions, samp_pop_size = sample_proportion( data[each], var_values )
            
            time0 = time.time()
            Indiv_DW = weights(total_recruitment, data['RID'], data[each], var_values, data['degree'], group_degree, samp_proportions, Phat, Ehat)
            print "Time to calculate individual dual component weights:", time.time()-time0
            print
            CH, AH, DH, CH0= homophily(Shat, Phat, Ehat, samp_pop_size)
            print "Total time spent on analyses:", time.time()-time_start
            
            print "Variable to Analyze:", each
            print
            print "Recruitment:", rec
            print
            print "Shat:",Shat
            print
            print "Ehat:", Ehat
            print
            print "Average Group Degree:",group_degree
            print
            print "Phat", Phat
            print
            print "Sampling Proportions:", samp_proportions
            print
            print "Individual dual-component weights,"
            print Indiv_DW
            print
            print "Coleman Homophily", CH
            print "Affiliation Homophily", AH
            print "Degree Homophily", DH
            print
            print '--------------------------------------'
            print
            
            """
            #Dual Homophily Test
            shat = np.array( [[.67, .33], [.67, .33]])
            phat = np.array([.5, .5])
            ehat = stationary1(shat)

            homoph = homophily(shat, phat, ehat)
            """

def main_for_sim( data, attLevels, total_recruitment = 0, estimateRDS = True):
    results = {}
    varnames = [ attLevels[i][0] for i in range( len(attLevels) ) ]
    longest_wave( data['ID'], data['RID'] )
    max_obs_waves = data['wave'].max()
    

    #total_recruitment = 500  # This is the number of recruitments
                            # which should be supplied in advance
                            # in order to increase efficiency of creating array.
    time_start = time.time()
    for i in range( len(attLevels) ):
        each = attLevels[i][0]  #Name of the attribute to be analyzed (e.g. 'aids')
        each_levels = np.ma.masked_array( attLevels[i][1] )  #Levels of the attribute to be analyzed (e.g. ('0','1') )

        results[each] = {}
        ####print "### Variable to be analyzed", each
        ####print '\n'
        ####print "ENTERING recruitment_matrix()"
        
        var_values, rec = recruitment_matrix(data['ID'], data['RID'], data[each], var_values = each_levels)
        ####print "recruitment matrix", rec
       #### print
        ####print "ENTERING seed_count()"
        seedCount_varval = seed_count( data[each], var_values, data['RID'] )  # An array of seed counts of var value type.
        ####print
        ####print "ENTERING sample_proportion()"
        ####print
        samp_proportions, samp_pop_size = sample_proportion( data[each], var_values )
        ####print "samp_proportions", samp_proportions
        ####print
        
        if estimateRDS == True:
            if len(var_values) > 2:
                dem_adj_rec = demographic_adjustment(rec)
                smoothed_rec = data_smoothing(dem_adj_rec)
                Shat = transition_matrix(smoothed_rec)
            else:
                Shat = transition_matrix(rec)

            group_degree = mean_degree( data['degree'], data['degree_seed_masked'], data['RID'], data[each], var_values, random_seed = False )
            Ehat = stationary1(Shat)     
            req_waves = required_waves(Shat, t=0.001)   #Waves required to reach equilibrium
            Phat = pop_estimates(group_degree,  Shat)
            VH_Phat = rds_VH_estimate(data[each], data['ID'], data['RID'], data['degree_seed_masked']) #Voltz-Heckathorn estimator
      
            #degree_cutpoint = deg_cutpoint(data['degree'], avg_cell=12)

            Indiv_RC_Degree = RC_degree(data['ID'], data['RID'], data['degree'], data[each], avg_cell_size=12 )
            #print Indiv_RC_Degree
            
            Adj_group_degree = adjusted_mean_degree(data['degree_seed_masked'], Indiv_RC_Degree, data[each], var_values)
            Adj_Phat = pop_estimates( Adj_group_degree, Shat)
        
            #Indiv_DW = weights(total_recruitment, data['RID'], data[each], var_values, data['degree'], group_degree, samp_proportions, Phat, Ehat)
            #print "Time to calculate individual dual component weights:", time.time()-time0
            #print
            #CH, AH, DH, CH0= homophily(Shat, Phat, Ehat, samp_pop_size)
            #print "Total time spent on analyses:", time.time()-time_start

            """
            print "Variable to Analyze:", each
            print
            print "Recruitment:", rec
            print
            print "Shat:",Shat
            print
            print "Ehat:", Ehat
            print
            print "Average Group Degree:",group_degree
            print
            print "Phat", Phat
            print
            print "Sampling Proportions:", samp_proportions
            print
            print "Individual dual-component weights,"
            print Indiv_DW
            print
            print "Coleman Homophily", CH
            print "Affiliation Homophily", AH
            print "Degree Homophily", DH
            print
            #print '--------------------------------------'
            print
            """

            results[each]["ehat"] = Ehat
            results[each]["waves_req"] = req_waves
            results[each]["waves_max"] = max_obs_waves
            results[each]["phat"] = Phat
            results[each]["vh_phat"] = VH_Phat
            results[each]["adj_phat"] = Adj_Phat
            
        results[each]["cnum"] = samp_pop_size
        results[each]["ctie"] = rec
        results[each]["seedCount"] = seedCount_varval

        
    results["sample_seedCount"] = np.ma.count_masked( data['RID'] )
    results['rec_count_sum'] = np.sum( data['rec_count'] )

    return results
            

if __name__ == '__main__':
    #Simul_file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/ConvertingRDS/test_input2.txt'  #This is one of Chris's simulated data
    #main( file_name = Simul_file_name, degree_cutpoint= degree_cutpoint, rdsat_format = False, varnames= 'aids', total_recruitment = 500)

    
    rdsat_file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/ConvertingRDS/nyjazz.txt'
    degree_cutpoint = [ (1,200), (201,400), (401, 600), (601, 900) ]
    main( file_name = rdsat_file_name, degree_cutpoint= degree_cutpoint, rdsat_format = True, varnames= ['Gender'], total_recruitment = 264)
    
    
    """
    degree_cutpoint = [ (1,200), (201,400), (401, 600), (601, 900) ]
    
    Cyworld_file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/Dual Homophily/Cyfriend_fcat01.txt'
    main( file_name = Cyworld_file_name, degree_cutpoint= degree_cutpoint, rdsat_format = False, varnames= ['Sex', 'age4', 'Region'], total_recruitment = 2751)
    
    
    Cyworld_file_name = 'C:/Users/patrick/Dropbox/Cornell/2011_Fall/RDS_Research/Dual Homophily/Cyfriend_fcat012.txt'
    main( file_name = Cyworld_file_name, degree_cutpoint= degree_cutpoint, rdsat_format = False, varnames= ['Sex', 'age4', 'Region'], total_recruitment = 160480)
    """


















