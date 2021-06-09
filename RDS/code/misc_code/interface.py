from jpype import *
import numpy as np


#############################################################################################################
#startJVM("C:/Program Files/Java/jre6/bin/server/jvm.dll", "-Djava.class.path=C:/Program Files/RDSAT 7.1.38/lib/cloning-1.8.1.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-api.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-core.jar;C:/Program Files/RDSAT 7.1.38/lib/opencsv-2.1.jar;C:/Program Files/RDSAT 7.1.38/lib/commons-betwixt-0.8.jar;C:/Program Files/RDSAT 7.1.38/lib/objenesis-1.2.jar")

#subgroupType = JPackage("edu").cornell.rdsat.api.beans.Subgroup.Type




###########
def rdsat_analyze(var, ID, rID, degree, levels, method="EnhancedDataSmoothing", estimator='DualComponent', confidence_alpha=.025, bootstraps=2500 ):
    SubgroupType = JClass("edu/cornell/rdsat/api/beans/Subgroup$Type")
    #print JPackage.getclass(Type)
    AlgorithmType = JPackage("edu/cornell/rdsat/api").AlgorithmType
    AverageNetworkSizeEstimation = JPackage("edu/cornell/rdsat/api").AverageNetworkSizeEstimation
    DualComponentType = JPackage("edu/cornell/rdsat/api/DualComponent").Type

    Library = JPackage("edu").cornell.rdsat.api.Toolkit.getInstance().getLibrary()

    options = JPackage("edu").cornell.rdsat.api.beans.PartitionAnalysisOptions()
    if estimator== "DualComponent":
        #print AverageNetworkSizeEstimation.DUAL_COMPONENT
        options.setAveNetSizeEstimation(AverageNetworkSizeEstimation.DUAL_COMPONENT)
    elif esitmator == 'Multiplicity':
        options.setAveNetSizeEstimation(AverageNetworkSizeEstimation.MULTIPLICITY_ESTIMATE)
    options.setConfidenceInterval(float(confidence_alpha))

    # Prepare partition analysis options - method
    if method == 'DataSmoothing':
        options.setAlgoType(AlgorithmType.DATA_SMOOTHING)
    elif method == 'EnhancedDataSmoothing':
        options.setAlgoType(AlgorithmType.ENHANCED_DATA_SMOOTHING)
    elif method == 'LLS':
        options.setAlgoType(AlgorithmType.LLS)
        
    options.setNumberOfReSamplesForBootstrap(bootstraps)
    
    # prepare the variable name to vector map
    varsAsMap = JPackage("java").util.HashMap()
    #print var
    '''
    util = JPackage("java.util")
    al = util.ArrayList()
    for item in var:
        al.add(str(item))
        varsAsMap.put('var',str(item))
    '''
    var_=[]
    id_=[]
    rid_=[]
    degree_=[]
    for i in range(len(var)):
        var_.append(str(var[i]))
        id_.append(str(ID[i]))
        rid_.append(str(rID[i]))
        degree_.append(str(degree[i]))
    #print var1
    varsAsMap.put('var',JArray(JString, 1)(var_))


    adHocRDSDataset=Library.newRDSDataFile(JArray(JString,1)(id_), JArray(JString,1)(rid_), JArray(JString, 1)(degree_), varsAsMap)
    #print adHocRDSDataset.getAttributeNames()
    #print adHocRDSDataset.getCouponsSentOut()
    #print adHocRDSDataset.getMissingDataString() 
    #print adHocRDSDataset.getRdsFile().getFileName()

    # Prepare "var" subgroup
    varSubgroup = JPackage("edu/cornell/rdsat/api/beans").Subgroup()
    Jtype= SubgroupType.COMPLETE
    varSubgroup.setType(Jtype)
    varSubgroup.setVariable(JString("var"))

    # gather subgroups into a list
    variables = JPackage("java.util").ArrayList()
    variables.add(varSubgroup)
    
    partAnalysis = Library.partitionAnalysis(adHocRDSDataset, options)

    # Explicitly set variable levels if specified
    java.lang.System.out.println(JChar("H"))
    if len(levels)>0:
        varLevels = JPackage("java").util.HashSet()
        for level in levels:
            Jlevel = JPackage("java").lang.String(str(level))
            #print Jlevel
            varLevels.add(Jlevel)
        partAnalysis.setAttributeLevels("var", varLevels)

    #print "options.toString()", options.toString()
    #print "variables", variables
    #print partAnalysis.getAttributeLevels("var")
    # perform partition analysis on subgroups
    resultSet = partAnalysis.performAnalysis(variables)
    #print 'HERE'
    netSizes = resultSet.getNetworkSizes()
    recResultset = resultSet.getRecruitmentResultset()
    estResultset = resultSet.getEstimationResultset()
    #print "AAAAAAAAAAAAAa"
    est_table = resultSet.getEstimationResultset().getConfidenceInterval().getDataTable()
    
    est_results=list()
    for level in levels:
        #print level
        temp_list = list()
        for i in range(4):
            temp_list.append(est_table[level][i]) # Yongren originally had "temp_list.append(est_table[level][i-1])"
        est_results.append(temp_list)
    return resultSet, est_results



def rdsat_aggregate(analyses, popsizes, strategy,  confidence_alpha=0.025):
    tables = JPackage("java").util.ArrayList()

    for i in range(len(analyses)):
        #tables = JPackage("edu.cornell.rdsat.api.util.AggregateEstimatesUtil$WeightedEstimationTable")
        CI = analyses[i].getEstimationResultset().getConfidenceInterval()        
        #print "i is ",i
        EST_Table = JClass("edu.cornell.rdsat.api.util.AggregateEstimatesUtil$WeightedEstimationTable")
        tables.add(EST_Table(CI, JDouble(popsizes[i])))

    aggregator = JClass("edu.cornell.rdsat.api.util.AggregateEstimatesUtil")(None, None, confidence_alpha )
    resultSet = aggregator.aggregateEstimates("Arbitrary Title", tables, strategy)
    est_table = resultSet.getAggregated().getDataTable()

    est_results=list()
    levels=2
    for level in range(levels):
        #print level
        temp_list = list()
        for i in range(4):
            temp_list.append(est_table[level][i])
        est_results.append(temp_list)
    return  est_results



##########################

if __name__ == "__main__":
    startJVM("C:/Program Files/Java/jre6/bin/server/jvm.dll", "-Djava.class.path=C:/Program Files/RDSAT 7.1.38/lib/cloning-1.8.1.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-api.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-core.jar;C:/Program Files/RDSAT 7.1.38/lib/opencsv-2.1.jar;C:/Program Files/RDSAT 7.1.38/lib/commons-betwixt-0.8.jar;C:/Program Files/RDSAT 7.1.38/lib/objenesis-1.2.jar")

    #prepare the data
    ID_ = list()
    rID_=list()
    degree_=list()
    var_=list()

    f=open('C:/Users/pp286/Documents/jpype_test_data.txt','r')
    for count, line in enumerate(f):
        if count>0:
            #print line.split()[1]
            ID_.append(line.split()[0])
            rID_.append(line.split()[1])
            degree_.append(line.split()[2])
            var_.append(line.split()[3])

    ## separate estimations
    resultSet1, est1 = rdsat_analyze(var=var_, ID=ID_, rID=rID_, degree=degree_,\
                   levels=[1,2], method="EnhancedDataSmoothing", estimator='DualComponent', confidence_alpha=.025, bootstraps=2500 )
    resultSet2, est2 = rdsat_analyze(var=var_, ID=ID_, rID=rID_, degree=degree_,\
                   levels=[1,2], method="EnhancedDataSmoothing", estimator='DualComponent', confidence_alpha=.025, bootstraps=2500 )

    ## aggregating
    analyses=[resultSet1, resultSet2]
    popsizes=[10000, 15000]
    StrategyType = JClass("edu/cornell/rdsat/api/beans/AnalysisStrategy$Strategy").INCLUDE_MISSING
    aggr_est_results = rdsat_aggregate(analyses, popsizes, strategy=StrategyType)

    ##print out the separate estimates and aggregate estimates
    print est1
    print est2
    print aggr_est_results



    shutdownJVM()


