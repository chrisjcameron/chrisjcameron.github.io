# Initialize java rdsLibrary and classpath 
require(rJava)
.jinit(silent=TRUE)
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/commons-betwixt-0.8.jar")
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/objenesis-1.2.jar")
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/cloning-1.8.1.jar")
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/opencsv-2.1.jar")
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/RDSAT-api.jar")
.jaddClassPath("C://Program Files/CDC-RDSAT 8.1.62/lib/RDSAT-core.jar")

print(.jclassPath())

#JFile <- .jnew("java/io/File", "sysout.log")
#JFileOutputStream <- .jnew("java/io/FileOutputStream",JFile)

#Jff<-.jcast(JFileOutputStream , "java/io/OutputStream")
#JPrintStream <- .jnew("java/io/PrintStream",Jff)

#J("java.lang.System")$setOut(JPrintStream)
#J("java.lang.System")$setErr(JPrintStream)

#options(warn=-1) 



# setup RDSAT Enum Data Types
SubgroupType <- J("edu/cornell/rdsat/api/beans/Subgroup$Type")
AlgorithmType <- J("edu/cornell/rdsat/api/AlgorithmType")
AverageNetworkSizeEstimation <- J("edu/cornell/rdsat/api/AverageNetworkSizeEstimation")
DualComponentType <- J("edu/cornell/rdsat/api/DualComponent$Type")


#print_out <- J("java/lang/Object")
#Jprint <- .Jnew("java/io/PrintStream")
#print_out$setOut(Jprint)
# main rdsLibrary
rdsLibrary <- J("edu/cornell/rdsat/api/Toolkit")$getInstance()$getLibrary()

print("Finished Java Init in rdsat_wrapper.r")

# define an 'analyze' wrapper function
rdsat.analyze <- function(id=c(), rid=c(), degree=c(), var=c(), levels=c(), method='EnhancedDataSmoothing', estimator='DualComponent', confidence_alpha=.025, bootstraps=2500)
{
    if (length(id) == 0) stop('Empty id vector')
    if (length(id) != length(rid)) stop('id vector length (', length(id), ') <> rid vector length (', length(rid), ')')
    if (length(id) != length(degree)) stop('id vector length (', length(id), ') <> degree vector length (', length(degree), ')')
    if (length(id) != length(var)) stop('id vector length (', length(id), ') <> var vector length (', length(var), ')')
 
    # Prepare partition analysis options - estimator
    options <- .jnew("edu/cornell/rdsat/api/beans/PartitionAnalysisOptions")
    if (estimator == 'DualComponent') { options$setAveNetSizeEstimation(AverageNetworkSizeEstimation$DUAL_COMPONENT) }
    else if (estimator == 'Multiplicity') { options$setAveNetSizeEstimation(AverageNetworkSizeEstimation$MULTIPLICITY_ESTIMATE) }
    else if (estimator == 'ArithmeticMean') { options$setAveNetSizeEstimation(AverageNetworkSizeEstimation$ARITHMETIC_MEAN) }
    else { stop ('Unknown estimator type: ', estimator, ' (should be one of "DualComponent", "ArithmeticMean", or "Multiplicity")') }
    options$setConfidenceInterval(as.numeric(confidence_alpha))
    
    # Prepare partition analysis options - method
    if (method == 'DataSmoothing') { options$setAlgoType(AlgorithmType$DATA_SMOOTHING) }
    else if (method == 'EnhancedDataSmoothing') { options$setAlgoType(AlgorithmType$ENHANCED_DATA_SMOOTHING) }
    else if (method == 'LLS') { options$setAlgoType(AlgorithmType$LLS) }
    else { stop ('Unknown method type: ', method, ' (should be one of "LLS", "DataSmoothing" or "EnhancedDataSmoothing"') }
    
    # Prepare partition analysis options - Number of Bootstraps
    #varAsInt <- .jnew('java.lang.Integer' 
    options$setNumberOfReSamplesForBootstrap(as.integer(bootstraps))

    # prepare the variable name to vector map
    varsAsMap <- .jnew('java.util.HashMap')
    varsAsMap$put('var', .jarray(as.character(var)))
    print("Trying to Create DataSet")
    #print(id)
    #print(rid)
    #print(degree)
    #print(var)
    adHocRDSDataset <- rdsLibrary$newRDSDataFile(.jarray(as.character(id)), .jarray(as.character(rid)), .jarray(as.character(degree)), varsAsMap)
    #print(paste('Ad hoc dataset created in:', adHocRDSDataset$getRdsFile()$getFileName())) 
    
    # Prepare "var" subgroup
    varSubgroup<- .jnew("edu/cornell/rdsat/api/beans/Subgroup")
    varSubgroup$setType(SubgroupType$COMPLETE)
    varSubgroup$setVariable("var")

    # gather subgroups into a list
    vars <- .jnew("java.util.ArrayList")
    vars$add(varSubgroup)
    
    # prepare partition analysis on subgroups
    partAnalysis <- rdsLibrary$partitionAnalysis(adHocRDSDataset, options)
    
    # Explicitly set variable levels if specified
    if (length(levels) > 0) {
        varLevels <- .jnew("java.util.HashSet")
        for (level in levels) { varLevels$add(as.character(level)) }
        partAnalysis$setAttributeLevels("var", varLevels)
    }
    
    #print(paste(
     #      'rdsat.analyze(rows=', length(id), 
      #     ', levels=', length(levels),
       #    ', method=', method,
        #   ', estimator=', estimator,
         #  ', confidence_alpha=', confidence_alpha
          # , ')', sep=""))

    # print out the options for reference
    #print(options$toString())

    # print out the variables for reference
    #print(vars)
#print(partAnalysis[])
    # perform partition analysis on subgroups
    resultSet = partAnalysis$performAnalysis(vars)
    
    netSizes <- resultSet$getNetworkSizes()
    recResultset <- resultSet$getRecruitmentResultset()
    estResultset <- resultSet$getEstimationResultset()
    #print(subset(.jevalArray(recResultset$getSamplePopulationSizes()$getDataTable(), simplify = TRUE), select=-1))
    sample_population_sizes = apply(subset(.jevalArray(recResultset$getSamplePopulationSizes()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric)
    #print(sample_population_sizes)
    #print(.jevalArray(estResultset$getPopulationEstimates()$getDataTable(), simplify =FALSE))
    population_estimates = .jevalArray(estResultset$getPopulationEstimates()$getDataTable(), simplify =TRUE)
    #print(population_estimates)
    #confidence_intervals = apply(subset(.jevalArray(estResultset$getConfidenceInterval()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric)
    #population_estimates = confidence_intervals[1:2,1]
    #print(population_estimates)
    #print(confidence_intervals)
    #print(population_estimates)
    
    
    
    
    # prepare object to contain results
    list(
        resultSet = resultSet,
        confidence_intervals = apply(subset(.jevalArray(estResultset$getConfidenceInterval()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        affiliation_matrix = apply(subset(.jevalArray(resultSet$getAffiliationMatrix()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        homophily = apply(subset(.jevalArray(resultSet$getHomophily()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        network_size = apply(subset(.jevalArray(resultSet$getNetworkSizeInformation()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        key_of_group = .jevalArray(resultSet$getKeyOfGroupAndTraitCorrespondence()$getDataTable(), simplify = TRUE),
        adjusted_ave_net_sizes = apply(subset(.jevalArray(netSizes$getAdjustedAverageNetSizes()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        unadjusted_ave_net_sizes = apply(subset(.jevalArray(netSizes$getUnadjustedAverageNetSizes()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        recruitment_count = apply(subset(.jevalArray(recResultset$getRecruitmentCount()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        transition_probabilities = apply(subset(.jevalArray(recResultset$getTransitionProbabilities()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        demographicaly_adjusted_recruitment = apply(subset(.jevalArray(recResultset$getDemographicallyAdjustedRecruitmentMatrix()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        data_smoothed_recruitment = apply(subset(.jevalArray(recResultset$getDataSmoothedRecruitments()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        data_smoothed_transition_probabilities = apply(subset(.jevalArray(recResultset$getDataSmoothedTransitionProbabilites()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        sample_population_sizes =sample_population_sizes,
        initial_recruits = apply(subset(.jevalArray(recResultset$getInitialRecruits()$getDataTable(), simplify = TRUE), select=-1), c(1,2), as.numeric),
        population_estimates = population_estimates
    )
}





## sample for running a simulation file:
## Have RDS data in an R dataframe
# source("/Program Files/RDSAT 7.1.38/rdsat_wrapper.r")

#      source('C:/Program Files/RDSAT 7.1.38/rdsat_wrapper.r')

# myRDSdata = read.table(file='rewritten_rds.txt', header=TRUE, sep=' ')
# myAnalysis = rdsat.analyze(id=myRDSdata$ID, rid=myRDSdata$RID, degree=myRDSdata$Degree, var=myRDSdata$Gender.MF., levels=c(1, 2, 3))
# print(myAnalysis$confidence_intervals)
# print(myAnalysis$key_of_group)
