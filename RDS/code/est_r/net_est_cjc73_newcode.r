require(compiler)
enableJIT(3)
# run with command:
# R CMD BATCH --no-save --no-restore '--args fileName="powerlaw_cluster_1seed_ghetto25.txt"' /Volumes/TerraFirma/RDS/code/net_est_cjc73_sgen.r
# Rscript "/Volumes/TerraFirma/RDS/code/net_est_cjc73_salg_DE_rep.r" "fileName='salg_2k_2grp_tree_sampfrac_n500_md8.txt'"
# "C:\\Program Files\\R\\R-3.3.2\\bin\\Rscript.exe" "R:\\RDS\\code\\net_est_cjc73_newCode.r" fileName="R:\RDS\samples\fb_50_diffRec_1.1-1.66_s0.3.txt"
comments = 
"# first
# second
"
options('warn'=1)
print("R: Loading Helpers")

##############################################
##  Some helper functions
##############################################
expand_sampleComp = function( attVals ) {
   header2 = c()
   eg = expand.grid( attVals )
   #cnum: 
   for (i in 1:nrow(eg)) {
           header2 = c(header2,paste("cnum_",i,sep=""))
   }
   #ctie:
   for (i in 1:nrow(eg)) {
      for (j in 1:nrow(eg)) {
           header2 = c(header2,paste("ctie",i,j,sep="_"))
       }
   }
   #stie - redundant - simply use prop.table(ctie, 1)
#   for (i in 1:nrow(eg)) {
#      for (j in 1:nrow(eg)) {
#           header2 = c(header2,paste("stie",i,j,sep="_"))
#       }
#   }

   #seedCount
   header2 = c(header2,"seedCount")
   
   #seedComp
   for (i in 1:length(estVals)) {
      for (j in 1:length(estVals[[i]])) {
           header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"seedCount", sep="_"))
       }
   }



   return(header2)
}   
   
   
expand_RDS_heads = function( estVals ) {
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
   
   #adj_phat
   for (i in 1:length(estVals)) {
      for (j in 1:length(estVals[[i]])) {
           header2 = c(header2,paste(names(estVals)[i],estVals[[i]][j],"adj_phat", sep="_"))
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

if (Sys.info()["sysname"]=="Windows") {
  source('R:/RDS/code/est_r/RDS4.R')
  now = date()
  # open files
  inFile = paste("R:/RDS/samples/", fileName, sep="")
  scanner = file(inFile, open="r")
  reader = file(inFile , open="r")
  results = file(paste("R:/RDS/estimates/", fileName, sep=""), open="w")
} else {
  source('/Volumes/TerraFirma/RDS/code/RDS4.R')
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

# -----------------------------------
# execute embedded commands
# embedded command includes the attribute names and levels in a pairlist:
# names(attVals) = attributes
# attVals$___ = c(levels of attribute)
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




# set vars
nlines = 2
go = TRUE
skip = 0
finish = 0
numlines = 0
counter = 1



#get column header
#this will order columns in header2 similarly to
#how they're ordered in header 1 - a constraint
headerread = readLines(scanner,1)

#print(paste("headerread: ",headerread))

header1 = unlist(strsplit(headerread,'\t'))

header2 = expand_sampleComp( attVals ) 
header2 = c(header2, expand_RDS_heads(estVals) ) #estVals can be defined in the header commands passed in, otherwise estVals==attVals
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
			variables = cbind(variables, as.matrix(rds)[,i])
		}
		comp = multi.recruitment( variables, rds$id, rds$RID, attVals) 

		## Looping code that generates estimates for each variable in estvals
		resultList = list()
		rec_act_list = list()
		for (varNum in 1:length(estVals) ) {
			resultList[[varNum]] = rds.set_groups_estimate(rds[,which(names(rds)==names(estVals)[varNum])], rds$id, rds$RID, rds$degree, groups=estVals[[varNum]] )
			rec_act_list[[varNum]] = rds.set_groups_diff_rec(rds[,which(names(rds)==names(estVals)[varNum])], rds$id, rds$RID, groups=estVals[[varNum]])
		}
		
		reportItems = c( 'bar$E.hat', 
            			'bar$P.hat', 'bar$adj.P.hat',
            			'bar$waves.req', 'bar$waves.max' ) 
            			
        
        resultsReport = c()
        for (item in reportItems) {
        	for (varNum in 1:length(estVals)) {
        		bar = resultList[[varNum]]
        	    resultsReport = c(resultsReport, eval(parse(text=item) ))
        	}
        }
        for (varNum in 1:length(estVals)) {
            bar = resultList[[varNum]]
        	resultsReport = c(resultsReport,
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
			
        write( c(
            as.numeric(rds[1,1:length(header1)]),
            comp$composition,
            t(comp$multi.rec),
            sum(comp$seed.composition),
            comp$seed.composition,
            resultsReport,
            sum(rds$rec_count)), 
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
