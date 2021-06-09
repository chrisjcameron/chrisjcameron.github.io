require(compiler)
enableJIT(3)
library(RDS)
# run with command:
# &"C:\\Program Files\\R\\R-3.3.3\\bin\\Rscript.exe" "R:\\RDS\\code\\est_r\\net_est_ss.r" "fileName = 'mt_temp.txt'"


comments = 
"
# first
# second
"
options('warn'=1)
print(R.Version()$version.string)
print("R: Loading Helpers")

##############################################
##  Some helper functions
##############################################
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
  
  #seedCount
  header2 = c(header2,"seedCount")
  
  for (i in 1:nrow(eg)) {
    header2 = c(header2,paste("seed_",paste(names(eg), eg[i,],sep='', collapse='_'),sep=""))
  }
  
  return(header2)
}   


expand_RDS_heads = function( estVals, estVH=FALSE) {
  header2 = c()
  
  
  #ehat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"ehat", sep="_"))
    }
  }
  
  #phat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"phat", sep="_"))
    }
  }
  
  #VH phat
  if (estVH) {
    for (i in 1:length(estVals)) {
      for (j in 1:length(estVals[[i]])) {
        header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"vh_phat", sep="_"))
      }
    }
  }
  
  #adj_phat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"adj_phat", sep="_"))
    }
  }
    
  #deg_comp
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"deg_comp", sep="_"))
    }
  }
  
  #rec_comp
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"rec_comp", sep="_"))
    }
  }
  
  #mean recruitment activity
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"rec_act", sep="_"))
    }
  }
  
  #adj.degree
  #for (i in 1:length(estVals)) {
  #  for (j in 1:length(estVals[[i]])) {
  #    header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"adj_deg", sep="_"))
  #  }
  #}
  
  #SS phat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"ss_phat", sep="_"))
    }
  }
  
  #RDSI phat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"rdsI_phat", sep="_"))
    }
  }
  
  #RDSISM phat
  for (i in 1:length(estVals)) {
    for (j in 1:length(estVals[[i]])) {
      header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"rdsIsm_phat", sep="_"))
    }
  }
  
  #waves data
  for (i in 1:length(estVals)) {
    wavesHeader = c( paste(names(estVals)[i],"waves_req", sep="_"), 
                     paste(names(estVals)[i],"waves_max", sep="_") )
    header2 = c(header2, wavesHeader)
  }
  
  #degree data
  #Replacing: header2 = c(header2,"deg.imp_min","deg.imp_med","deg.adj_min","deg.adj_med")
  
  for (i in 1:length(estVals)) {
    degreeHeader = c( paste(names(estVals)[i],"deg.imp_min", sep="_"), 
                      paste(names(estVals)[i],"deg.imp_med", sep="_"),
                      paste(names(estVals)[i],"deg.adj_min", sep="_"), 
                      paste(names(estVals)[i],"deg.adj_med", sep="_") )
    header2 = c(header2, degreeHeader)
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
  fileName = "foo"
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

print(fileName)

source('R:\\RDS\\code\\est_r\\rds4.r')
now = date()
# open files
inFile = paste("R:\\RDS\\samples\\", fileName, sep="")
scanner = file(inFile, open="r")
reader = file(inFile , open="r")
results = file(paste("R:\\RDS\\estimates\\", fileName, sep=""), open="w")


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

# skip blank lines
while( trimws(currentLine)=="" ) {
  currentLine = readLines(scanner,1)
  foo = readLines(reader,1)
}

# -----------------------------------
# execute embedded commands
# embedded command includes the attribute names and levels in a pairlist:
# names(attVals) = attributes
# attVals$___ = c(levels of attribute)
while( substring(currentLine,1,1) == "-" ) {
  print( currentLine )
  eval(parse(text=substring(currentLine, 2)))
  currentLine = readLines(scanner,1)
  foo = readLines(reader,1)
}

if(!exists("estVals")) { 
  estVals = attVals
}

#cats = expand.grid(attVals, stringsAsFactors=FALSE)


# set vars
nlines = 2
go = TRUE
skip = 0
finish = 0
numlines = 0
counter = 1
estVH = TRUE


#get column header
#this will order columns in header2 similarly to
#how they're ordered in header 1 - a constraint
headerread = readLines(scanner,1)

#print(paste("headerread: ",headerread))

header1 = unlist(strsplit(headerread,'\t'))

header2 = expand_sampleComp( attVals ) 
header2 = c(header2, expand_RDS_heads(estVals, estVH) ) #estVals can be defined in the header commands passed in, otherwise estVals==attVals
header2 = c(header2, "rec_count_sum")
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
      if (!(is.na(i))) {
      	variables = cbind(variables, as.matrix(rds)[,i])
      }
    }
    comp = multi.recruitment( variables, rds$id, rds$RID, attVals) 
    
    ## Looping code that generates estimates for each variable in estvals
    resultList = list()
    rec_act_list = list()
    for (varNum in 1:length(estVals) ) {
      resultList[[varNum]] = rds.set_groups_estimate(
      	rds[,which(names(rds)==names(estVals)[varNum])], 
      	rds$id, 
      	rds$RID, 
      	rds$degree, 
      	groups=estVals[[varNum]], 
      	estVH=estVH
      )
      rec_act_list[[varNum]] = rds.set_groups_diff_rec(
        rds[,which(names(rds)==names(estVals)[varNum])], 
        rds$id, rds$RID, groups=estVals[[varNum]]
      )
    }
    
    reportItems = c( 'bar$E.hat', 'bar$P.hat', 'bar$adj.P.hat',
                     'bar$group.degree.component', 'bar$group.recruitment.component') 
    if (estVH) {
      reportItems = c( 'bar$E.hat', 'bar$P.hat', 'bar$VH.P.hat', 'bar$adj.P.hat',
                       'bar$group.degree.component', 'bar$group.recruitment.component') 
    } 
    
    resultsReport = c()
    for (item in reportItems) {
      for (varNum in 1:length(estVals)) {
        bar = resultList[[varNum]]
        resultsReport = c(resultsReport, eval(parse(text=item) ))
      }
    }

    item='bar$mean_rec' 
    for (varNum in 1:length(estVals)) {
        bar = rec_act_list[[varNum]]
        resultsReport = c(resultsReport, eval(parse(text=item)))
    }
    
    ## Add Gile's estimate    
    net_size = sum(rds[1,grep("^num*", names(rds))])
    # recruiter for seeds cannot be NA, assign instead value 0
    rds$RID = rds$RID + 1
    rds$id = rds$id + 1
    NAs = is.na(rds$RID)
    rds$RID[NAs] = 0
    ssrds = as.rds.data.frame(rds, id="id", recruiter.id="RID", network.size="degree", population.size = net_size)

    for (varNum in 1:length(estVals)) { 
        ssrds$charoutcome = sapply(rds[names(estVals)[varNum]], as.character)
        #as.character(eval(parse(text=paste('rds$',names(estVals)[1], sep = ""))))
        var_cat_count = length(estVals[[varNum]]) # Count of levels of variable

        ssest_est = tryCatch({
                ssest = RDS.SS.estimates(rds.data=ssrds, outcome.variable="charoutcome", N=net_size,  empir.lik=FALSE)
                ssest@estimate
            }, 
            error=function(err) {
                #print("Bailing on SS");
                #rm(ssest);
                return(rep(NA_character_, var_cat_count))
            })
        if (length(ssest_est) != var_cat_count) {
            ssest_est = rep(NA_character_, var_cat_count)
        }
        
        rdsi_est = tryCatch({
                rdsi_est = RDS.I.estimates(rds.data=ssrds, outcome.variable="charoutcome", empir.lik=FALSE)
                rdsi_est@estimate
            }, 
            error=function(err) {
                #print("Bailing on RDSI");
                #rm(rdsi_est);
                return(rep(NA_character_, var_cat_count))
            })
        if (length(rdsi_est) != var_cat_count) {
            rdsi_est = rep(NA_character_, var_cat_count)
        }
        
        rdsis_est = tryCatch({
                rdsis_est = RDS.I.estimates(rds.data=ssrds, outcome.variable="charoutcome", smoothed=TRUE, empir.lik=FALSE)
                rdsis_est@estimate
            }, 
            error=function(err) {
                #print("Bailing on RDSI_sm");
                #rm(rdsis_est);
                return(rep(NA_character_, var_cat_count))
            })
        if (length(rdsis_est) != var_cat_count) {
            rdsis_est = rep(NA_character_, var_cat_count)
        }   
    	resultsReport = c(resultsReport, ssest_est, rdsi_est, rdsis_est)         #, 1 - ssest@estimate, ssest@estimate)
    }
    rm(ssrds);
    gc();
     
    
    for (varNum in 1:length(estVals)) {
      bar = resultList[[varNum]]
      resultsReport = c(resultsReport,
                        bar$waves.req, bar$waves.max,
                        min(bar$imputed.degree, na.rm=TRUE), 
                        median(bar$imputed.degree, na.rm=TRUE),
                        min(bar$adj.degree, na.rm=TRUE),
                        median(bar$adj.degree, na.rm=TRUE)   
      ) 
    }
    
    
    if (counter == 1) { 
      header1 = c( names(rds)[ 1:(length(names(rds))-(length(attVals)+5)) ] ) #trim off id, RID, degree, wave, rec_count and attVals
      header = c(header1,header2)
      write(header, file=results, ncolumns=length(header), sep="\t", append=TRUE)
      #print(comp$categories)
    }
    
    if ("var_name" %in% names(rds)) {
    	rds$var_name = as.character(rds$var_name)
    }
    write( c(
      unlist(rds[1,1:length(header1)]),
      comp$composition,
      t(comp$multi.rec),
      sum(comp$seed.composition),
      comp$seed.composition,
      resultsReport,
      sum(rds$rec_count)), 
           file=results, ncolumns=length(header), sep="\t", append=TRUE )
    
    skip = skip + numlines
    readLines(reader,1)
    if (counter %% 500 == 0) {
        print(paste(fileName, ", Trial Number: ", counter))
        #print(paste("Memory Use: ", memory.size()))
    }
    counter = counter + 1
    
    # cjc73 - try to work around 32-bit R
    #rm(ssrds)
    #rm(ssest)
    #rm(resultsReport)
    #rm(variables)
    #rm(comp)
    #rm(rds)
    #rm(bar)
    #gc()
    
  } 
  finish = finish + nlines
  
}
close(results)
#warnings()
q()
