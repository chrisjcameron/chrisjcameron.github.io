from jpype import *
import numpy as np
#Interface1.py puts rdsat_analyze and rdsat_aggregate into a class so that the jpype portion is defined only once.
#This is an effort to reduce the potential of memory leakage. -pp286 1/25/2013


#############################################################################################################
#startJVM("C:/Program Files/Java/jre6/bin/server/jvm.dll", "-Djava.class.path=C:/Program Files/RDSAT 7.1.38/lib/cloning-1.8.1.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-api.jar;C:/Program Files/RDSAT 7.1.38/lib/RDSAT-core.jar;C:/Program Files/RDSAT 7.1.38/lib/opencsv-2.1.jar;C:/Program Files/RDSAT 7.1.38/lib/commons-betwixt-0.8.jar;C:/Program Files/RDSAT 7.1.38/lib/objenesis-1.2.jar")

#subgroupType = JPackage("edu").cornell.rdsat.api.beans.Subgroup.Type




###########

class rdsat_jpype:

    def __init__(self, confidence_alpha= 0.025):
        self.SubgroupType = JClass("edu/cornell/rdsat/api/beans/Subgroup$Type")
        self.AlgorithmType = JPackage("edu/cornell/rdsat/api").AlgorithmType
        self.AverageNetworkSizeEstimation = JPackage("edu/cornell/rdsat/api").AverageNetworkSizeEstimation
        self.DualComponentType = JPackage("edu/cornell/rdsat/api/DualComponent").Type

        self.Library = JPackage("edu").cornell.rdsat.api.Toolkit.getInstance().getLibrary()

        self.options = JPackage("edu").cornell.rdsat.api.beans.PartitionAnalysisOptions()
        self.StrategyType = JClass("edu/cornell/rdsat/api/beans/AnalysisStrategy$Strategy").INCLUDE_MISSING ##What happens if I use "EXCLUSE_MISSING"?
        self.EST_Table = JClass("edu.cornell.rdsat.api.util.AggregateEstimatesUtil$WeightedEstimationTable")
        self.confidence_alpha = confidence_alpha
        self.aggregator = JClass("edu.cornell.rdsat.api.util.AggregateEstimatesUtil")(None, None, self.confidence_alpha )
        
        
    def rdsat_analyze(var, ID, rID, degree, levels, method="EnhancedDataSmoothing", estimator='DualComponent', bootstraps=2500 ):

        if estimator== "DualComponent":
            #print AverageNetworkSizeEstimation.DUAL_COMPONENT
            self.options.setAveNetSizeEstimation(self.AverageNetworkSizeEstimation.DUAL_COMPONENT)
        elif esitmator == 'Multiplicity':
            self.options.setAveNetSizeEstimation(self.AverageNetworkSizeEstimation.MULTIPLICITY_ESTIMATE)
        self.options.setConfidenceInterval(float(self.confidence_alpha))

        # Prepare partition analysis options - method
        if method == 'DataSmoothing':
            self.options.setAlgoType(self.AlgorithmType.DATA_SMOOTHING)
        elif method == 'EnhancedDataSmoothing':
            self.options.setAlgoType(self.AlgorithmType.ENHANCED_DATA_SMOOTHING)
        elif method == 'LLS':
            self.options.setAlgoType(self.AlgorithmType.LLS)
            
        self.options.setNumberOfReSamplesForBootstrap(bootstraps)
        
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


        adHocRDSDataset=self.Library.newRDSDataFile(JArray(JString,1)(id_), JArray(JString,1)(rid_), JArray(JString, 1)(degree_), varsAsMap)
        #print adHocRDSDataset.getAttributeNames()
        #print adHocRDSDataset.getCouponsSentOut()
        #print adHocRDSDataset.getMissingDataString() 
        #print adHocRDSDataset.getRdsFile().getFileName()

        # Prepare "var" subgroup
        varSubgroup = JPackage("edu/cornell/rdsat/api/beans").Subgroup()
        Jtype= self.SubgroupType.COMPLETE
        varSubgroup.setType(Jtype)
        varSubgroup.setVariable(JString("var"))

        # gather subgroups into a list
        variables = JPackage("java.util").ArrayList()
        variables.add(varSubgroup)
        
        partAnalysis = self.Library.partitionAnalysis(adHocRDSDataset, self.options)

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
            
        #Erase all variables.
        varsAsMap = None
        adHocRDSDataset = None
        varSubgroup = None
        Jtype = None
        variables = None
        varLevels = None
        Jlevel = None
        partAnalysis = None
        netSizes = None
        recResultset = None
        estResultset = None
        est_table = None
        
        return resultSet, est_results



    def rdsat_aggregate(analyses, popsizes):
        tables = JPackage("java").util.ArrayList()

        for i in range(len(analyses)):
            #tables = JPackage("edu.cornell.rdsat.api.util.AggregateEstimatesUtil$WeightedEstimationTable")
            CI = analyses[i].getEstimationResultset().getConfidenceInterval()
            
            tables.add(self.EST_Table(CI, JDouble(popsizes[i])))

        
        resultSet = self.aggregator.aggregateEstimates("Arbitrary Title", tables, self.StrategyType)
        est_table = resultSet.getAggregated().getDataTable()

        est_results=list()
        levels=2
        for level in range(levels):
            #print level
            temp_list = list()
            for i in range(4):
                temp_list.append(est_table[level][i])
            est_results.append(temp_list)
            
        tables = None
        CI = None
        resultSet = None
        est_table = None
        temp_list = None
        
        return est_results



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


