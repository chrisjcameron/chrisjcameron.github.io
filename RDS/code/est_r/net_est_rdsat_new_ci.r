require(compiler)
enableJIT(3)
require(rJava)

# run with command:
# R CMD BATCH --no-save --no-restore '--args fileName="powerlaw_cluster_1seed_ghetto25.txt"' /Volumes/TerraFirma/RDS/code/net_est_cjc73_sgen.r
# Rscript "/Volumes/TerraFirma/RDS/code/net_est_cjc73_salg_DE_rep.r" "fileName='salg_2k_2grp_tree_sampfrac_n500_md8.txt'"
# "C:\\Program Files\\R\\R-2.15.0\\bin\\x64\\Rscript.exe" "C:\\RDS\\code\\net_est_rdsat_ci.r" fileName='C:/RDS/samples/ws_40k_N500_c1_rUnif2.txt'
comments = 
  "# first
# second
"
options('warn'=1)
print("R: Loading Helpers")

##############################################
##  Some helper functions
##############################################

keyword_match = function( kword ){
    switch( kword,
            ehat      = 'E.hat',
            phat      = 'P.hat',
            vhat      = 'VH.P.hat',
            adj_phat  = 'adj.P.hat',
            waves_req = 'waves.req',
            maves_max = 'waves.max',
            )   
}

compute_report_items = function( lvl_col_names=c(), var_col_names=c()) {
    report_items = unlist( lapply(c(lvl_col_names,var_col_names), keyword_match) )
    if (is.null(report_items)) { return(c()) }   
    return( report_items )   
}

level_name_expand = function( estVals, name_str=NULL ){
    # name_str = list(c("simple_ci_low", 'simple_ci_high'), c('var_ci_low', 'var_ci_high'))
    # applies low/high pairs to all levels of var1, then all levels of var2
    header = c()
    if (is.null( name_str )) { return( header ) }
    
    # if name_str = c('foo', 'bar'), then vname
    
    for (vname in names(estVals)) {
      for (n_str in name_str) {
        for (lname in estVals[[vname]]) {
         header = c(header,paste(vname, lname, n_str, sep="_"))
        }
      }
    }
    return(header)
}


var_name_expand = function( estVals, name_str=NULL ){
    # name_str = list(c("simple_ci_low", 'simple_ci_high'), c('var_ci_low', 'var_ci_high'))
    # applies low/high pairs to all levels of var1, then all levels of var2
    header = c()
    if (is.null( name_str )) { return( header ) }
    
    # if name_str = c('foo', 'bar'), then vname
    
    for (vname in names(estVals)) {
      for (n_str in name_str) {
         header = c(header,paste(vname, n_str, sep="_"))
      }
    }
    return(header)
}


rdsat.level_name_expand = function( estVals, name_list=NULL ){
    #name_str = list('rdsat.2007est',c('rdsat.2007lower', 'rdsat.2007higher'))
    header = c()
    if (is.null( name_list )) { return( header ) }
    
    for (vname in names(estVals)) {
      for (name_elem in name_list) {
        for (lname in estVals[[vname]]) {
            header = c(header,paste(name_elem, vname, lname, sep="_"))
        }
      }
    }
    return(header)
}


expand_sampleComp = function( attVals ) {
   header2 = c()
   eg = expand.grid( attVals, stringsAsFactors=FALSE )
   #cnum: 
   for (i in 1:nrow(eg)) {
     header2 = c(header2,paste("cnum_",paste(eg[i,], collapse=''),sep=""))
   }
   #ctie:
   for (i in 1:nrow(eg)) {
     for (j in 1:nrow(eg)) {
          header2 = c( header2,
                       paste("ctie",
                           paste(eg[i,], collapse=''),
                           paste(eg[j,], collapse=''),
                       sep="_") )
     }
   }

#    #seedCount
#    header2 = c(header2,"seedCount")
#    
#    for (i in 1:nrow(eg)) {
#      header2 = c(header2,paste("seed_",paste(names(eg), eg[i,],sep='', collapse='_'),sep=""))
#    }
   
   return(header2)
}  

expand_RDS_heads = function( estVals, lvl_col_names=c('ehat', 'adj_phat'), var_col_names=c(), rdsat_cols='2007') {
      # provide desired groups to estimate and value 
      # level column names: 
      #   'ehat', 'phat', 'vh_phat', 'adj_phat', 
      #   list(c("simple_ci_low", 'simple_ci_high'), c('var_ci_low', 'var_ci_high')),
      #   "deg.imp_min", "deg.imp_med",
      #   "deg.adj_min", "deg.adj_med",
      #
      # variable column names:
      #   "waves_req",  "waves_max", 
      #
      # 
      # rdsat_cols = c('2004', '2007')
      #
      # AttVals is created by a command like:
      #  attVals = list()
      #  attVals$race = c('0', '1')
      #
      #
      header2 = c() 
      
      for (name_str in lvl_col_names) {
         header2 = c(header2, level_name_expand(estVals, lvl_col_names))
      }
      
      if ('2007' %in% rdsat_cols) {
        rdsat_col_names = list('rdsat.2007est',c('rdsat.2007lower', 'rdsat.2007higher'))
        header2 = c(header2, rdsat.level_name_expand(estVals, rdsat_col_names) )
      }
      if ('2004' %in% rdsat_cols) {
        rdsat_col_names = list('rdsat.2004est',c('rdsat.2004lower', 'rdsat.2004higher'))
        header2 = c( header2, rdsat.level_name_expand(estVals, rdsat_col_names) )
      } 
      for (name_str in var_col_names) {
         header2 = c(header2, var_name_expand(estVals, var_col_names))
      }
      
   return(header2)
}





print("R: Processing")

##############################################
##  Processing code
##############################################

args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  ##supply default values
  fileName = "fbve_50_diffRec_1_s0.3.txt"
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

print(fileName)

if (Sys.info()["sysname"]=="Windows") {
  source('E:\\RDS\\boulder_code\\est_r\\RDS3.R')
  source('E:\\RDS\\boulder_code\\est_r\\rdsat_wrapper.r') ####
  source('E:\\RDS\\boulder_code\\est_r\\new_bootstrap.r')
  now = date()
  # open files
  inFile = paste("E:\\RDS\\samples\\", fileName, sep="")
  scanner = file(inFile, open="r")
  reader = file(inFile , open="r")
  results = file(paste("E:\\RDS\\estimates\\", 'new_ci_', fileName, sep=""), open="w")
} else {
  source('/Volumes/TerraFirma/RDS/code/RDS3.R')
  now = date()
  # open files
  #fileName = "powerlaw_cluster_10seed_ghetto25.txt"
  inFile = paste("/Volumes/TerraFirma/RDS/samples/", fileName, sep="")
  scanner = file(inFile, open="r")
  reader = file(inFile , open="r")
  results = file(paste("/Volumes/TerraFirma/RDS/estimates/", fileName, sep=""), open="w")
}

# -----------------------------------
# read settings header
#   get scanner and reader pointed to first non-comment line
#   they read it, so the next line is the first header...
currentLine = "#"
while( substring(currentLine,1,1) == "#" ) {
  write( currentLine, results, append=TRUE )
  currentLine = readLines(scanner,1)
  foo = readLines(reader,1)
}


# set vars - these values may be replaced by embedded commands 
nlines = 2
go = TRUE
skip = 0
finish = 0
numlines = 0
counter = 1
estVH = FALSE
bs_count = 2000  #Bootstrap count
ci_alpha=.025

# What goes in output?
var_col_names = c()
lvl_col_names = c()
rdsat_col_names = '2007'

reportItems = compute_report_items( lvl_col_names, var_col_names)


# -----------------------------------
# execute embedded commands
# embedded command includes the attribute names and levels in a pairlist:
# names(attVals) = attributes
# attVals$___ = c(levels of attribute)
# ie:
#  -attVals = list()
#  -attVals$race = c('0', '1')
while( substring(currentLine,1,1) == "-" ) {
  eval(parse(text=substring(currentLine, 2)))
  currentLine = readLines(scanner,1)
  foo = readLines(reader,1)
  #print( currentLine )
}

if(!exists("estVals")) { 
  estVals = attVals
}

#cats = expand.grid(attVals)

#get column header
#this will order columns in header2 similarly to
#how they're ordered in header 1 - a constraint
headerread = readLines(scanner,1)

#print(paste("headerread: ",headerread))

header1 = unlist(strsplit(headerread,'\t'))

header2 = expand_sampleComp( attVals ) 
 #estVals can be defined in the header commands passed in, otherwise estVals==attVals
header2 = c(header2, expand_RDS_heads(estVals, lvl_col_names, var_col_names, rdsat_col_names) )

#header2 = c(header2, "rec_count_sum")
#everything else

while (go) {
  lines = readLines(scanner,nlines)
  if (length(lines) <= 0) { go=FALSE }
  
  if ("----\t" %in% lines) {
    numlines = finish - skip + which(lines == "----\t")
    
    if (counter > 1) {
      mynrows=numlines-2
    }
    else {
      mynrows=numlines-1
    }
    rds = read.table(reader, header = TRUE, sep="\t", skip=0, nrows=mynrows)
    #print(paste("Trial Number: ", counter))
    
    variables = cbind()
    for (attribute in names(attVals) ) {
      i = match(attribute, names(rds))
      variables = cbind(variables, as.matrix(rds)[,i])
    }
    comp = multi.recruitment( variables, rds$id, rds$RID, attVals) 
    
    ## Looping code that generates estimates for each variable in estvals
    resultList = list()
    simple_ci_list = list()
    var_ci_list = list()

    if ( length(c(lvl_col_names, var_col_names)) > 0 ) {
        for (varNum in 1:length(estVals) ) {
          variable = rds[,which(names(rds)==names(estVals)[varNum])]
          resultList[[varNum]] = rds.set_groups_estimate(variable, rds$id, rds$RID, rds$degree, groups=estVals[[varNum]], estVH=estVH )
          #print(rds[,which(names(rds)==names(estVals)[varNum])])
          probs = resultList[[varNum]]$proportion.recruitment
          deg_wt_list = list()
          for (i in 1:length(estVals[[varNum]])) {
            # Get 1/degree separated by variable value
            deg_wt_list[[i]]= ( resultList[[varNum]]$imputed.degree[variable==estVals[[varNum]][i]] )^-1
          }
      
      
          simple_ci_list[[varNum]] = simple_bs(probs, deg_wt_list, trials = bs_count, one_tail_alpha=ci_alpha)
          var_ci_list[[varNum]] = trans_var_bs(probs, deg_wt_list, trials = bs_count, one_tail_alpha=ci_alpha)
      
        }
        
        reportItems = c( 'bar$E.hat', 'bar$P.hat', 'bar$adj.P.hat') 
        if (estVH) {
            reportItems = c( 'bar$E.hat', 'bar$P.hat', 'bar$VH.P.hat', 'bar$adj.P.hat') 
        } 
    
        resultsReport = c()
                
    }
    
    resultsReport = c()
    rdsatReport2007 = c()
    rdsatReport2004 = c()
    
    ####
#     for (item in reportItems) {
#       for (varNum in 1:length(estVals)) {
#         bar = resultList[[varNum]]
#       }
#     }
    
#    for (varNum in 1:length(estVals)) {
#        #resultsReport = c(resultsReport, eval(parse(text=item)) )
#        resultsReport = c(resultsReport, simple_ci_list[[varNum]], var_ci_list[[varNum]])
#    }
    
    for (varNum in 1:length(estVals)) {
      if ('2007' %in% rdsat_col_names) { 
        # 2007 estimator
        est.rdsat2007=c()
        confidence_intervals2007=c()
        Analysis2007 = rdsat.analyze(id=rds$id, rid=rds$RID, degree=rds$degree, var=rds[,which(names(rds)==names(estVals)[varNum])], levels=estVals[[varNum]] , estimator='DualComponent', confidence_alpha=ci_alpha, bootstraps=bs_count)####

        est.rdsat2007 = c( Analysis2007$confidence_intervals[1:length(estVals[[varNum]]),1])####
        for (lvl in 1:length(estVals[[varNum]])){

        confidence_intervals2007 =  c(confidence_intervals2007, Analysis2007$confidence_intervals[lvl,2:3])####
        }
        rdsatReport2007 = c(rdsatReport2007, est.rdsat2007, confidence_intervals2007) ####
      }
      if ('2004' %in% rdsat_col_names) { 
        #2004 estimator
        est.rdsat2004=c()
        confidence_intervals2004=c()
        Analysis2004 = rdsat.analyze(id=rds$id, rid=rds$RID, degree=rds$degree, var=rds[,which(names(rds)==names(estVals)[varNum])], levels=estVals[[varNum]] , estimator='Multiplicity', confidence_alpha=ci_alpha, bootstraps=bs_count)####
        est.rdsat2004 = c( Analysis2004$confidence_intervals[1:length(estVals[[varNum]]),1])####
        for (lvl in 1:length(estVals[[varNum]])){

        confidence_intervals2004 =  c(confidence_intervals2004, Analysis2004$confidence_intervals[lvl,2:3])####
        }
        rdsatReport2004 = c(rdsatReport2004, est.rdsat2004, confidence_intervals2004) ####
      }

    }
    
    
    if (counter == 1) { 
      
      #header1 = c( names(rds)[ 1:(length(names(rds))-(length(attVals)+5)) ] ) #trim off id, RID, degree, wave, rec_count and attVals
      header1 = c(names(rds)[1:(which(names(rds)=='id')-1)])
      header = c(header1,header2)

      write(header, file=results, ncolumns=length(header), sep="\t", append=TRUE)
      #print(comp$categories)
    }
    
    write( c(
             as.numeric(rds[1,1:length(header1)]),
             comp$composition,
             t(comp$multi.rec),
             resultsReport,
             rdsatReport2007,
             rdsatReport2004), 
           file=results, ncolumns=length(header), sep="\t", append=TRUE )
    
    skip = skip + numlines
    readLines(reader,1)
    #if (counter %% 100 == 0) { print(paste("Trial Number: ", counter)) }
    counter = counter + 1
  } 
  finish = finish + nlines
  
}
close(results)
warnings()
q()
