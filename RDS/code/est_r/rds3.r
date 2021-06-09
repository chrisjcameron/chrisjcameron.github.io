require(compiler)
enableJIT(3)
# RDS analysis is done on a variable by variable basis. 
# recommended that results are stored in an easy to reference format

# Args( data, estVar, IDvar, rIDvar, options )
# Note that args are data columns
# options should include how to deal with levels > 2

# This estimation method reproduces the "multiplicity" estimate with "data smoothing"
# Calculated by RDS Analysis Tool v6.0.1
# 
# New in RDS3 is the ability to generate a multi-factor population composition and tie table
#

multi.composition = function( variables, attVals, RID) {
	# Convert variables table to single categories corresponding to expand.grid(attVals, stringsAsFactors=FALSE)
	# intersect( which(sdf$race[i]==cat_attVals$race), intersect(which(sdf$neighborhood[i]==cat_attVals$neighborhood), which(sdf$aids[i]==cat_attVals$aids)))
	
	#for each respondent
		#figure out which expand.grid category they belong to:
		#  intersect the results of "which" in a recursive way, but do not use recursion
		#  intersection of all intersections should yield one group
		#  !!! Missing data is not allowed !!! --> non-unique group assignment--return NA?
	cats = expand.grid(attVals, stringsAsFactors=FALSE)
	comp_group = rep(NA, nrow(variables))
	
	for (i in 1:nrow(variables)) {
		matchTF = rep(TRUE, nrow(cats))
		for (j in 1:ncol(variables) ) {
			matchTF = matchTF & (unlist(cats[j]) == variables[i,j])
		}
		if ( NA %in% matchTF ) { 
			comp_group[i] = NA
		}else{
			comp_group[i] = match(TRUE, matchTF)
		}
	}
	
	multi.comp = rep(0,nrow(cats))
	multi.seeds = rep(0,nrow(cats))
	for (i in 1:nrow(cats) ) {
		multi.comp[i] = length(which(comp_group==i))
		multi.seeds[i] = length( which(comp_group==i & is.na(RID) ))
	}
	
	results = list(composition=multi.comp, seed.composition=multi.seeds)
	
}

multi.recruitment = function( variables, ID, RID, attVals) {
	# Return recruitment composition and sample composition. Note that attVals and vars must be in same order.
	# Convert variables table to single categories corresponding to expand.grid(attVals, stringsAsFactors=FALSE)
	# intersect( which(sdf$race[i]==cat_attVals$race), intersect(which(sdf$neighborhood[i]==cat_attVals$neighborhood), which(sdf$aids[i]==cat_attVals$aids)))
	
	#for each respondent
		#figure out which expand.grid category they belong to:
		#  intersect the results of "which" in a recursive way, but do not use recursion
		#  intersection of all intersections should yield one group
		#  !!! Missing data is not allowed !!! --> non-unique group assignment--return NA?
	cats = expand.grid(attVals, stringsAsFactors=FALSE)
	comp_group = rep(NA, nrow(variables))
	
	# How many levels of grouping variable?
	groupCount = nrow(cats)
	
	for (i in 1:nrow(variables)) {
		matchTF = rep(TRUE, groupCount)
		for (j in 1:ncol(variables) ) {
			matchTF = matchTF & (unlist(cats[j]) == variables[i,j])
		}
		if ( NA %in% matchTF ) { 
			comp_group[i] = NA
		}else{
			comp_group[i] = match(TRUE, matchTF)
		}
	}
	
	multi.comp = rep(0,groupCount)
	multi.seeds = rep(0,groupCount)
	for (i in 1:groupCount ) {
		multi.comp[i] = length(which(comp_group==i))
		multi.seeds[i] = length( which(comp_group==i & is.na(RID) ))
	}
		
	groupNames = 1:groupCount

	#print(comp_group)

	rVar = rep(NA,length(comp_group))
	for (i in 1:length(ID) ) {
		#print(paste(i,': ', RID[which(ID==i)]))
		if (!(is.na(RID[i]))) {
			#rVar[i] = as.character(comp_group[match(RID[i], ID)])
			rVar[i] = comp_group[match(RID[i], ID)]

		}
	}
	rVar = factor(rVar,levels=groupNames)
	comp_group = factor(comp_group, levels=groupNames)	
	# Gives raw recruitment

	raw_rec=table(rVar, comp_group)
	
	results = list(categories = cats, composition=multi.comp, seed.composition=multi.seeds, multi.rec=raw_rec)
}


rds.VH.estimate = function(variable, ID, recruiterID, degree, groups=NULL){
	# How many levels of grouping variable?
	if ( is.null(groups)) {
		u=as.factor(variable)
	} else {
		u=factor(variable, levels=sort(groups))
	}

	groupCount = nlevels(u)
	groupNames = levels(u)
	grpRng = 1:groupCount
	
	
	sample.recruits.dist = rep(0, groupCount)
	sample.population.proportion = rep(0, groupCount)

	RDS2.estimate = rep(0, groupCount) 	
	
	indexSRU = which(!( is.na(variable)))
	Nu = length(indexSRU) # the n in the equation 9


	indexDU = which(!(is.na(variable)|is.na(ID)|is.na(recruiterID) |is.na(degree) ))
	meanDegreeU = length(indexDU)/sum(1/degree[indexDU], na.rm=TRUE ) # delta_u in the eq 9

	meanDegreeA = rep(0 , groupCount) # used to store the delta_A in eq 9

	for (i in 1:length(groupNames) ) {
		indexSR = which((variable==groupNames[i]))
		sample.recruits.dist[i]=length(indexSR) #n_A in the eq 9

		indexD = which((variable==groupNames[i]) & !( is.na(degree)|is.na(ID)|is.na(recruiterID)|is.na(variable)  ))
		meanDegreeA[i] = (length(indexD))/sum(1/degree[indexD], na.rm=TRUE ) # delta_A in eq9

		sample.population.proportion[i] = sample.recruits.dist[i]/Nu # n_A/n in eq 9

		RDS2.estimate[i] = sample.population.proportion[i] *(meanDegreeU/meanDegreeA[i])

	}
	results = list(RDS.VH.estimate = RDS2.estimate,
			mean.network.size.multiplicity = meanDegreeA,
			sample.population.proportion = sample.population.proportion ,
			meanDegree.ratio = meanDegreeU/meanDegreeA,
			overall.mean.network.size.multiplicity = meanDegreeU
			)
}



rds.estimate = function( variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, ci_alpha=0, ...) 
{
	#Should add some error checking
	# - args should be the same length
	# method should be recognized
	
	ok.methods = c("data.smoothing", "least.squares")
	if (!(method %in% ok.methods)) {
		stop(paste("*** Unimplemented Method ***"))
	}

	# How many levels of grouping variable?
	u=as.factor(variable)
	groupCount = nlevels(u)
	groupNames = levels(u)

	rVar = rep(NA,length(variable))
	for (i in ID) {
		if (!(is.na(recruiterID[which(ID==i)]))) {
			rVar[which(ID==i)] = as.character(variable[which(ID==recruiterID[which(ID==i)])])
		}
	}
	rVar = factor(rVar,levels=groupNames)
	
	# Gives raw recruitment
	t=table(rVar, variable)
	
	# Proportional Recrutiment
	S = prop.table(t,1)
	
	# Total Recruitment 
	#  of Group- RO
	#  by Group- RB
	RO = apply(t,2,sum)
	RB = apply(t,1,sum)

	#Recruitment Proportion - R
	R = RO/sum(t)
	
	# How many levels of grouping variable?
	#u=unique(as.factor(variable))
	#groupCount = nlevels(u)
	#groupNames = levels(u)
	
	# Sample Composition
	Cc = rep(0 , groupCount)
	names(Cc) = groupNames
	seeds = rep(0 , groupCount)
	names(seeds) = groupNames

	for (i in 1:length(groupNames) ) {
		Cc[i] = length(which(variable==groupNames[i]))
		seeds[i] = length( which(variable==groupNames[i] & is.na(recruiterID) ))
	}
	
	C = Cc/sum(Cc)
	
	S.org = S
	
	E.hat = rds.mcmc.props( S )
	t.dem.adj = sum( RB) * E.hat * S
	t.data.smoothed = (t.dem.adj + t(t.dem.adj))/2
	if ( method == "data.smoothing" ) {
		# Need to recalculate
		# row sum, column sums, selection prop, equilibria
		
		# Proportional Recrutiment
		S = prop.table(t.data.smoothed,1)
		
		# Total Recruitment 
		#  of Group- RO
		#  by Group- RB
		#RO = apply(t.data.smoothed,2,sum)
		#RB = apply(t.data.smoothed,1,sum)
	
		#Recruitment Proportion - R
		R = RO/sum(t.data.smoothed)
	}
	
	
	rec = list(sample.recruitment=t, recruitment.dem.adj=t.dem.adj, recruitment.smoothed=t.data.smoothed, proportion.recruitment=S, recruitment.of=RO, recruitment.by=RB,
				recruitment.proportion = R, comp.props=C, composition=Cc, seeds=seeds, groupNames = groupNames, method=method, rVar = rVar)
	
	
	#Sample Degree Estimate
	wt=1/degree
	
	meanDegree = rep(0 , groupCount)
	for (i in 1:length(groupNames) ) {
		index = which((variable==groupNames[i]) & !(is.na(recruiterID)))
		meanDegree[i] = weighted.mean(degree[index],wt[index], na.rm=TRUE )
	}
	
	# Population Estimates & Equilibrium
	
	#   Population Estimates			
	P.hat = rds.mcmc.props( S, meanDegree, method)

	#P.hat.vector = rep(0,length(variable))
	#for (i in 1:length(groupNames) ) {
	#	P.hat.vector[which(variable==groupNames[i])] = P.hat[i]
	#}
	
	#Equilibrium		
	E.hat = rds.mcmc.props( S )
	
	#Homophily
	homophily.2002 = rep(0,groupCount)
	for ( i in 1:groupCount ) {
		if ( P.hat[i] <= S[i,i] ) {
			homophily.2002[i] = (P.hat[i] - S[i,i])/(P.hat[i]-1)
			#ah = ()/() 
		} else {
			homophily.2002[i] = (S[i,i])/(P.hat[i])-1	
		}
	}
	
	
	# Store Estimate Variables
	est = list( E.hat.equilibrium=E.hat, P.hat.population.proportion=P.hat,
				multiplicity.mean.degree = meanDegree, Homophily = homophily.2002)
	 
	# Group Sampling Weights
	# 	ratio of pop estimate P and sample proportion composition
	sampling.weights = rep(0,length(variable))
	
	for (i in 1:length(groupNames) ) {
		sampling.weights[which(variable==groupNames[i])] = P.hat[i]/C[i]
	}
	
	#Group Dual Weights
	# Recruitment Component
	# Degree Component
	recruitment.component = rep(0 , groupCount)
	names(recruitment.component) = groupNames
	
	degree.component = rep(0 , groupCount)
	names(degree.component) = groupNames

	for (i in 1:length(groupNames) ) {
		recruitment.component[i] = E.hat[i]/C[i]	
		degree.component[i] = P.hat[i]/E.hat[i]
	}
	
						 
	#Individual Weight Components
	
	# Imputation Setup
	#   Set Seeds to their group mean
	#   Set missing values to their group means

	degree.imp = degree
	
	for (i in 1:length(groupNames) ) {
		degree.imp[which(is.na(recruiterID) & variable==groupNames[i] )] = meanDegree[i]
		degree.imp[which(is.na(degree) & variable==groupNames[i] )] = meanDegree[i]
		
		#People with zero degree are treated as missing
		degree.imp[which( degree <= 0 & variable==groupNames[i] )] = meanDegree[i]

	}
	
	
	# Calculate the Constants
	#  Recruitment Components
	RC = rep(0,length(variable))
	for (i in 1:length(groupNames) ) {
		RC[which(variable==groupNames[i])] = recruitment.component[i]
	}
	
	#  K is an estimate for the overall mean degree for respondents
	K = length(variable)/sum(RC/degree.imp)
	#  DW is the individualized Dual Weight
	dual.weights = RC/degree.imp*K
	 	
	 	
	est = c(est, list(   imputed.degree = degree.imp,
						 group.sampling.weights = sampling.weights,
						 group.recruitment.component = recruitment.component, 
						 group.degree.component = degree.component,
						 weights.dual.component = dual.weights,
						 recruitment.components = RC,
						 K.constant = K,
						 degree.components = K/degree.imp ) )
	
	
	
	# Adjusted Population Estimates
	parts = (length(na.omit(degree)) / group.size)^(.5)
	
	if (parts < 2) {
		parts = 2
	} else {
		parts = floor(parts)
	}
	
	breaks = unique(quantile(degree, probs=seq(1/parts,1,1/parts) ,na.rm=TRUE))
	#print(breaks)
	
	deg.groupNames = 1:length(breaks)
	group = rep(NA,length(variable))
	group[which(degree >= 1)] = 1

	for (i in 1:(parts-1)) {
		group[which(degree > breaks[i])] = i+1
	
	}
	
	
	rGrp = rep(NA,length(degree))
	for (i in ID) {
		if (!is.na(recruiterID[which(ID==i)]) ) {
			rGrp[which(ID==i)] = as.character(group[which(ID==recruiterID[which(ID==i)])])
		}
	}
	
	rGrp = factor(rGrp,levels=deg.groupNames)

	
	# Gives raw recruitment
	deg.t=table(rGrp, group)
	
	# Proportional Recrutiment
	deg.S = prop.table(deg.t,1)
		
	# Total Recruitment - RO
	deg.RO = apply(deg.t,2,sum)
	
	#Recruitment Proportion - R
	deg.R = RO/sum(deg.t)
	
	#Sample Composition - C
	
	Cg = rep(0 , parts)
	names(Cg) = deg.groupNames
	for (i in 1:parts ) {
		Cg[i] = length(which(group[which(group!=0)]==i))/length(group[which(group!=0)])
	}
	
	deg.C = rep(0 , length(degree))
	for (i in 1:parts ) {
		deg.C[which(group == i)] = Cg[i]
	}
		 
 	# Degree Adjusted Equilibrium
	deg.E.hat = rds.mcmc.props( deg.S )

 	# Produce Vector of equilibiria values
 	deg.eq = rep(NA,length(degree))
 	for ( i in 1:length(deg.groupNames) ) {
 		deg.eq[which(group == deg.groupNames[i])] = deg.E.hat[i]
 	}
	
	# Recruitment Component of Degree RCD
	deg.RCD = deg.eq/deg.C
	deg.RCD[which(is.na(recruiterID))] = NA
		
	# Adjusted Degree
	RCDtoD = deg.RCD/degree
	aggRCDtoD = sapply(groupNames,function(x) sum((RCDtoD)[which(variable==x)],na.rm=TRUE) )
	aggRCD = sapply(groupNames,function(x) sum((deg.RCD)[which(variable==x)],na.rm=TRUE) )
	
	adj.degree = c(aggRCD/aggRCDtoD)

 	# Degree Adjusted Population Estimates 
	adj.P.hat = rds.mcmc.props( S, adj.degree) 
	
	
	#Homophily
	ahomophily.2002 = rep(0,groupCount)
	for ( i in 1:groupCount ) {
		if ( P.hat[i] <= S[i,i] ) {
			ahomophily.2002[i] = (adj.P.hat[i] - S[i,i])/(adj.P.hat[i]-1)
			#ah = ()/() 
		} else {
			ahomophily.2002[i] = (S[i,i]/adj.P.hat[i])-1	
		}
	}
	
	#Bootstrap CI
  if (ci.alpha > 0) {
    simple_bs_values(S, degree_weights_lists, mean_degs, trials=100 )
    
  }
  
  
	adj = list( degree.partition.count=parts, adj.degree=adj.degree, 
				adj.E.hat=deg.E.hat, adj.P.hat = adj.P.hat, adj.Homophily = ahomophily.2002 )
	results = c( rec, est, adj )
}


read.rds = function(file, calc_wave=TRUE, ... )
{
# Reads a file formatted for RDS Analysis Tool v5.6
# and returns an R data.frame
#
# Does some checking and issues errors when errors might be present in data file
#   ** Does not check missing value is correct, but does substitute NA.
#   ** Does not check that number of coupons is correct.
#   -- Issues warnings if the recruiter for a respondent with a recruitment 
#      coupon cannot be found, but treats these cases as seeds.
#
# Adds a data column and computes Recruiter's ID
#
	header = scan(file, skip = 1, n = 3, quiet=TRUE)
	if (length(header) != 3) {
		stop("Error in Header: Wrong number of items.")
	} else {
		DF = read.delim( file, skip = 1, header=TRUE, na.strings=as.character(header[3]), ...)
	}
	
	if( nrow(DF) != header[1] ) { stop("Error in Header: Number of cases mis-match.") }
	
	names(DF)[1:3] = c("ID", "Degree", "recCouponID")
	names(DF)[4:(3+header[2])] = paste("C", 1:header[2], sep="")
	
	DF$RID = rep(NA, nrow(DF))
	for (i in 1:nrow(DF) ) 
	{
		index = which(DF[,4:(3+header[2])]==DF$recCouponID[i]) %% nrow(DF)
		if( !is.na(DF$recCouponID[i]) && length(index) == 0 ) {
			warning(paste("Respondent ID ", DF$ID[i],": Recruiter for non-seed not found, will be treated as seed"))
		}
		if ( length(index) > 0 && index > 0) { 
			DF$RID[i] = DF$ID[index]
		}
	}
	#Set degree 0 to missing to avoid calculation errors
	DF$Degree[which(DF$Degree==0)] = NA
	if (calc_wave) {
		DF$wave = rds.waves(DF$ID, DF$RID)
	}
	
	DF
}

rds.waves = function(ID, recruiterID) 
{
	wave=rep(0,length(ID))
	
	nextWaveIndex = which(is.na(recruiterID))
	
	currentWaveCount = 1
	currentWaveIndex = unlist(sapply(nextWaveIndex, function(x) which(recruiterID==ID[x])))

	while (length(currentWaveIndex) > 0) {
		wave[currentWaveIndex] = currentWaveCount
		nextWaveIndex = unlist(sapply(currentWaveIndex, function(x) which(recruiterID==ID[x])))
		currentWaveCount = currentWaveCount+1
		currentWaveIndex = nextWaveIndex
	}
	wave
}

rds.seed_chain = function(ID, recruiterID) 
{
	chain=rep(0,length(ID))
	chain_ID = seq(1,sum(is.na(recruiterID)) )
	
	nextWaveIndex = which(is.na(recruiterID))
	chain[nextWaveIndex] = chain_ID
	
	currentWaveCount = 1
	
	currentWaveIndex = lapply(nextWaveIndex, function(x) which(recruiterID %in% ID[x]))
	while (sum(unlist(lapply(currentWaveIndex,length))) > 0) {
		for (i in chain_ID ) {
			chain[currentWaveIndex[[i]]] = i
		}	
		currentWaveIndex = lapply(currentWaveIndex, function(x) which(recruiterID %in% ID[x]))
	}
	chain
}

rds.wave.est = function( pTab, tol=.001, printWaves=FALSE)
{
	# Variables to hold calculations
	if (is.null(dimnames(pTab)[[1]])) {
		 dimnames(pTab)[1] = list(1:nrow(pTab))
		 dimnames(pTab)[2] = list(1:nrow(pTab))
	}
	
	groupNames = dimnames(pTab)[[1]]
	len = length(groupNames)
	
	#Set up starting distributions
	starts = matrix(0,len,len)
	diag(starts) = 1
	
	notDone = TRUE
	
	waveCount = 0
	currentDists = starts
	while(notDone) {
		if (printWaves) {
			print(waveCount)
			print(currentDists)
		}
		currentDists = currentDists %*% pTab
		deviations = apply(currentDists,2,function(x) max(x) - min(x))
		notDone =  any( deviations > tol)
		waveCount = waveCount+1
		if (waveCount > 1000) {
			waveCount = 10000
			notDone = FALSE
		}
	}
	
	waveCount
}

rds.mcmc.props = function( S, weights=NULL, method="data.smoothing")
{
	# Variables to hold calculations
	if (is.null(dimnames(S)[[1]])) {
		 dimnames(S)[1] = list(1:nrow(S))
		 dimnames(S)[2] = list(1:nrow(S))
	}
	
	groupNames = dimnames(S)[[1]]
	len = length(groupNames)
	
	#print(len)
	
 	#Generic Weighted MCMC Estimate
	if (len==2) {	
		# Weighted Estimates of Equilibrium Proportions
		# P[A] = S[BA]*D[B]/ ( Sba*Db + Sab*Da )
		if (is.null(weights) ) { weights=rep(1,nrow(S)) }
		P.hat = rep(0 , len)
		for (i in 1:len ) {
			a = i
			b = 3 - i
			
			P.hat[i] = S[groupNames[b],groupNames[a]]*weights[b] /
			(S[groupNames[b],groupNames[a]]*weights[b] + S[groupNames[a],groupNames[b]]*weights[a])

		}		
			
	} else { # (groupCount > 2)
		P.hat = rep(NA , len)
		if (method=="data.smoothing") {
			if( is.null(weights) ) {
				#print(t(S))
				#print(diag(1,len,len))
				coe = t(S) - diag(1,len,len)
				coe[len,] = rep(1, len )
				b = c(rep(0,len-1),1)
			} else {
				coe = matrix(rep(0,len^2),nrow=len)
				coe[1:len] = c( weights[1] * S[1,] )
				diag(coe) = c(-1*weights*S[,1])
				coe[1,] = rep(1,len)
				b = c(1,rep(0,len-1))
			}
			try((P.hat = solve( coe , b )))
		}
		if (method=="least.squares") {
			stop("*** Unimplemented Method ***")
		}
	
	}
	names(P.hat) = groupNames
	P.hat[which(P.hat<=0)]=0
	#P.hat[which(is.na(P.hat))]=0	
	P.hat
}

rec.table = function( row.var , column.var, dim.Names, group.Names ) 
{
	len = length(group.Names)
	res = diag(0, len, len)
	for (i in 1:len ) {
		res[i,] = tabulate( na.omit(column.var[which(row.var==i)]), len)


	}

	res = as.table(res, dnn=c(as.charater(row.var)))
	dimnames(res) = list(group.Names,group.Names)
	names(dimnames(res)) = dim.Names
	res
	
}

# Generate a bootstrapped CI for estimate (no variance in transition probs)
simple_bs_values = function(S, degree_weights_lists, mean_degs, trials=100 ) {
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(unlist(degree_weights_lists))
	overall_mean = mean(mean_degs)

	state_count = nrow(S) 
	#print(bs_results)
	#print(sample_size)
	#print(bs_mean_deg)
	
	for (j in 1:trials) {
		#Bootstrap a sample composition and transition probs
		bs_sample = generate_state_seq(seq_len=sample_size, probs=S)		
		
		rec = matrix(0, state_count, state_count)

		for (i in 2:length(bs_sample)) {
			rec[bs_sample[i-1], bs_sample[i]] = rec[bs_sample[i-1], bs_sample[i]]+1
		}
		
		bs.props = prop.table(rec, 1)
		bs.cnum  = as.vector(table( factor(bs_sample, levels=c(1:state_count) ) ))
		#print(bs.cnum)
		
		#Bootstrap a degree for each group, based on the bootstrapped sample composition
		
		bs_mean_deg = rep(0,)
		for (i in 1:length(bs_mean_deg)){
			if (bs.cnum[i]>=1) {
				bs_mean_deg[i] = bs.cnum[i] / sum( sample(degree_weights_lists[[i]], size=bs.cnum[i], replace=TRUE) )
			} else {
				bs_mean_deg[i] = overall_mean
			}
		}
		
		bs_results[j,] = unlist(rds.mcmc.props(bs.props, bs_mean_deg))
	}
	#print(bs_results)
	#bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	return(bs_results)
}

agg_bs_values = function(bs_values_cbind, site_weights, one_tail_alpha=.025 ){
	bs_results = apply(bs_values, 1, weighted.mean, site_weights)
	bs_ci = quantile(bs_results, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE)
	return(bs_ci)
}

agg_bs_values_approx = function(bs_values_cbind, site_weights, one_tail_alpha=.025 ){
	bs_results = apply(bs_values, 1, weighted.mean, site_weights)
	bs_ci = quantile(bs_results, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE)
	return(bs_ci)
}


generate_state_seq = function( seq_len = 10, states = c(1, 2), probs = matrix(NA, 2, 2), initial_state_pos=NA ) {
	ret_seq = rep(0, seq_len)
	rand_list = runif(seq_len)
	if (is.na(initial_state_pos)) { ret_seq[1]=sample.int(nrow(probs), 1) }
	else { ret_seq[1] = initial_state_pos }
	cum_prob = t(apply(probs, 1, cumsum))
	cum_prob[,nrow(cum_prob)] = 1  #Set the last column to 1 to avoid rounding errors
	for (i in 2:seq_len){
		 ret_seq[i] = 1+sum(cum_prob[ret_seq[i-1],] <= rand_list[i] )
	}
	# Could add code to replace the state_index with the state code (as.factor)
	return(ret_seq)
}

rds.bootstrap_fast = function(o_variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, groups=NULL, minXrec=.00001, est.only=FALSE, trials=1000, one_tail_alpha=.025, ...) 
{
	print("Warning: This function produces narrow CI compared to RDSAT")
	# This function may exhibit unexpected behavior when ID is not of mode "numeric"
	# Levels of var should be coded 1,2,3,..
	# Fix it with code below:
	if (is.null(groups)) {
		variable = factor(o_variable)
		levels(variable) = 1:nlevels(variable)
	} else {
		variable = factor(o_variable)
		groups = 1:length(groups)
		levels(variable) = groups
	}
	# Generate Point Estimate for Variable
	actual = rds.set_groups_estimate(variable, ID, recruiterID, degree, method=method, group.size=group.size, 
									 groups=groups, minXrec=minXrec)
	ID = actual$ID
	recruiterID = actual$RID
	
	# Generate degree_weights list of lists
	# Rec of i by j
	# j recruted i
	degree_weights = vector(mode="list", length=length(actual$groupNames))
	for (i in 1:length(actual$groupNames)) {
		group_by_group = vector(mode="list", length=length(actual$groupNames))
		for ( j in 1:length( actual$groupNames ) ) {
			temp_list_value <- actual$imputed.degree[which(   (actual$rVar==actual$groupNames[j])
					   				        	  			& (variable==actual$groupNames[i])  )]			
			group_by_group[j] = list(temp_list_value^-1)
		}
		degree_weights[i] <- list(group_by_group)
	}
	S_bs = prop.table(actual$sample.recruitment+minXrec, 1)
	print(S_bs)
	
	sample_size = length(actual$ID)
	overall_mean = mean(actual$mean.degree)
	overall_mean_inv = overall_mean^-1
	
	bs_results = matrix(nrow=trials, ncol=ncol(S_bs))
	state_count = nrow(S_bs) 
	#bs_sample = generate_state_seq(seq_len=sample_size*trials, probs=S_bs)
	#start_index = seq(from=1, to=sample_size*trials, by=sample_size )
	
	for (t_num in 1:trials) {
		#Bootstrap a sample composition and transition probs
		bs_sample = generate_state_seq(seq_len=sample_size, probs=S_bs)		
		#bs_sample_degree = array(NA, sample_size)
		#bs_sample_degree[1] = sample(unlist(degree_weights[[ bs_sample_states[1] ]]), 1)
		rec = matrix(0, state_count, state_count)
		for (i in 1:(sample_size-1) ) {
			rec[ bs_sample[i], bs_sample[i+1]] = rec[ bs_sample[i], bs_sample[i+1]]+1
			#bs_sample_degree[i] = sample(unlist(degree_weights[[ bs_sample_states[i] ]][[ bs_sample_states[i-1] ]]), 1)
		}
		# rec might look like:
		#         [,1] [,2] [,3]
		#	[1,]   87   37   18
		#	[2,]   39   44    7
		#	[3,]   17    8    6

		
		bs.props = prop.table(rec+.00001, 1)
		bs.cnum  = as.vector(table( factor(bs_sample, levels=c(1:state_count) ) ))
		
		#Bootstrap a degree for each group, based on the bootstrapped sample composition	
		
		bs_mean_deg_mat = matrix(0, state_count, state_count)
		for (i in 1:ncol(rec)){
			for (j in 1:nrow(rec)) {
				if (rec[i,j]>=1) {
					if (length(degree_weights[[i]][[j]]) > 1) {
						bs_mean_deg_mat[i,j] = sum( sample(unlist(degree_weights[[i]][[j]]), size=rec[j,i], replace=TRUE) )
					}
					else if (length(degree_weights[[i]][[j]]) == 1) {
						bs_mean_deg_mat[i,j] = sum( rep(unlist(degree_weights[[i]][[j]]), rec[j,i]) )
					}
					else {
						bs_mean_deg_mat[i,j] = overall_mean_inv
					}
				}					
			}
		}
		bs_mean_deg = bs.cnum / apply(bs_mean_deg_mat, 1, sum)
		
		E.hat = rds.mcmc.props( bs.props )
		t.dem.adj = sum(rec) * E.hat * bs.props
		t.data.smoothed = (t.dem.adj + t(t.dem.adj))/2
		bs_prop_DS = prop.table(t.data.smoothed+.0001, 1)

		bs_results[t_num,] = unlist(rds.mcmc.props(bs_prop_DS, bs_mean_deg))
		#print(bs_results[t_num,])
	}
	#print(bs_results)
	#bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	return(bs_results)
	#return(bs_ci)
}

rds.bootstrap_slow = function(o_variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, groups=NULL, minXrec=.0001, est.only=FALSE, trials=1000, one_tail_alpha=.025, ...) 
{
	# This function may exhibit unexpected behavior when ID is not of mode "numeric"
	# Levels of var should be coded 1,2,3,..
	# Fix it with code below:
	if (is.null(groups)) {
		variable = factor(o_variable)
		levels(variable) = 1:nlevels(variable)
	} else {
		variable = factor(o_variable)
		groups = 1:length(groups)
		levels(variable) = groups
	}
	# Generate Point Estimate for Variable
	actual = rds.set_groups_estimate(variable, ID, recruiterID, degree, method=method, group.size=group.size, 
									 groups=groups, minXrec=minXrec)
	ID = actual$ID
	recruiterID = actual$RID
	
	# Generate degree_weights list of lists
	# Rec of i by j
	# j recruted i
	degree_weights = vector(mode="list", length=length(actual$groupNames))
	for (i in 1:length(actual$groupNames)) {
		group_by_group = vector(mode="list", length=length(actual$groupNames))
		for ( j in 1:length( actual$groupNames ) ) {
			temp_list_value <- actual$imputed.degree[which(   (actual$rVar==actual$groupNames[j])
					   				        	  			& (variable==actual$groupNames[i])  )]			
			group_by_group[j] = list(temp_list_value)
		}
		degree_weights[i] <- list(group_by_group)
	}
	S_bs = prop.table(actual$sample.recruitment+minXrec, 1)
	
	sample_size = length(actual$ID)
	overall_mean = mean(actual$mean.degree)
	
	sample_degrees = function(x,y) {
		sample(unlist(degree_weights[[ x ]][[ y ]]), 1)
	}

	bs_results = matrix(nrow=trials, ncol=ncol(S_bs))
	state_count = nrow(S_bs) 
	bs_sample_states = generate_state_seq(seq_len=sample_size*trials, probs=S_bs)
	bs_sample_degree = 	mapply( sample_degrees, head(bs_sample_states,-1) , tail(bs_sample_states,-1) )
	start_index = seq(from=1, to=sample_size*trials, by=sample_size )
	
	for (t_num in 1:trials) {
		#Bootstrap a sample composition and transition probs
		#bs_sample_states = generate_state_seq(seq_len=sample_size, probs=S_bs)		
		#bs_sample_degree = array(NA, sample_size)
		#bs_sample_degree[1] = sample(unlist(degree_weights[[ bs_sample_states[1] ]]), 1)
		#rec = matrix(0, state_count, state_count)
		#for (i in 1:(sample_size-1) ) {
		#	rec[ bs_sample[start_index[t_num]], bs_sample[start_index[t_num]+i] ] = rec[ bs_sample[start_index[t_num]], bs_sample[start_index[t_num]+i] ]+1
		#	#bs_sample_degree[i] = sample(unlist(degree_weights[[ bs_sample_states[i] ]][[ bs_sample_states[i-1] ]]), 1)
		#}
		# rec might look like:
		#         [,1] [,2] [,3]
		#	[1,]   87   37   18
		#	[2,]   39   44    7
		#	[3,]   17    8    6
		
		#bs.props = prop.table(rec, 1)
		#bs.cnum  = as.vector(table( factor(bs_sample, levels=c(1:state_count) ) ))

		bs_results[t_num,] = rds.set_groups_estimate(bs_sample_states[start_index[t_num]:(start_index[t_num]+sample_size-1)], 1:sample_size, 
													 c(NA,1:(sample_size-1)), bs_sample_degree[start_index[t_num]:(start_index[t_num]+sample_size-1)],
													 est.only=TRUE)$adj.P.hat
	}
	#print(bs_results)
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	#return(bs_results)
	return(bs_ci)
}


rds.bootstrap = function( variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, groups=NULL, trials=1000, ...) 
{
	# This function may exhibit unexpected behavior when ID is not of mode "numeric"
	# Levels of var should be coded 1,2,3,..
	recruiterID = apply(as.array(recruiterID),1,function(x) match(x,ID))
	ID = 1:length(ID)

	
	# Generate Point Estimate for Variable
	actual = rds.set_groups_estimate(variable, ID, recruiterID, degree, method, group.size, groups=groups )
	
	# Generate lists of people according to recruiter's variable
	index=list()
	seeds=list()
	for (i in 1:length(actual$groupNames) ) {
		index[i] = list(ID[which((actual$rVar==actual$groupNames[i]) & !is.na(variable) )])
		seeds[i] = list(ID[which((variable==actual$groupNames[i]) & is.na(actual$rVar) )])
	}
	
	seeds=(unlist(seeds))
	seedCount = length(seeds)
	
	# Generate list to hold the results
	results = list()
	results$P.hat = matrix(nrow=trials,ncol=length(actual$groupNames))
	results$adj.P.hat = matrix(nrow=trials,ncol=length(actual$groupNames))

	#data = as.data.frame(cbind(variable, ID, recruiterID, degree))
	#names(data) = c("variable", "ID", "recruiterID", "degree")
	recruitable = unlist(index)
	size = length(recruitable)
	
	bs.size = length(seeds)+size
	lcm.size = seedCount*ceiling(bs.size/seedCount)  ##Why?
	
	# Set up variables for resample
	re.ID  = seq(1,bs.size)
	re.var = vector("character",bs.size)
	re.RID = c(rep(NA,seedCount), seq(1,bs.size-seedCount))
	re.degree = vector("numeric", bs.size )
	
	re.RID[1:seedCount] = NA
	curVar = as.vector(variable[seeds])
	curWave= as.vector(seeds)
	
	
	# Need to take sample from all seeds simlultaneously.
	#  - determine how to terminate when enough seeds are added
	#
	#
	
	# Generate samples, calculate and record the estimates
	for ( i in 1:trials) {
		re.samp = matrix(vector("numeric", length=lcm.size),nrow=seedCount)
		re.seeds = sample(seeds)
		re.samp[1:seedCount] = re.seeds
		curVar = variable[re.seeds]
		
		re.var[1:seedCount]=curVar

		
		
		
		# select sample from seed
		for ( j in 2:(lcm.size/seedCount)) {
			#print(re.samp)
			curWave = (sapply(curVar, function(x) sample(index[[x]],1)))
			re.samp[,j] = curWave
			curVar = variable[curWave]
			
		}
		re.samp.vec = as.vector(re.samp)[1:bs.size]
		re.var = variable[re.samp.vec]
		re.degree = degree[re.samp.vec]
		
		# create data.frame
		
		#rd = as.data.frame(cbind(re.ID,re.var,re.RID,re.degree))
		foo = rds.set_groups_estimate(re.var, re.ID, re.RID, re.degree, method, group.size, groups=groups, est.only=TRUE)
		results$P.hat[i,] = foo$P.hat
		results$adj.P.hat[i,] = foo$adj.P.hat

		#print(i)
	}
	results.ci = lapply(results, function(x) apply(x,2,"quantile", seq(0,1,.025)[c(1:3,39:41)], na.rm=TRUE ) )
	c( results.ci, list( trials=trials, na.P.hat=sum(is.na(results$P.hat[,1])),
						 na.adj.P.hat=sum(is.na(results$adj.P.hat[,1]))					
					   ) 
	) 
}

rds.estimate.fast = function( variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, minXrec=0, est.only=FALSE, ...) 
{
	#ID == index for this version
	# ID = 1:length(ID)
	# RID = apply(as.array(recruiterID),1,function(x) match(x,ID))

	#Should add some error checking
	# - args should be the same length
	# method should be recognized
	
	ok.methods = c("data.smoothing", "least.squares")
	if (!(method %in% ok.methods)) {
		stop(paste("*** Unimplemented Method ***"))
	}
	dataLen = length(ID)
	dataRng = 1:dataLen
	
	# How many levels of grouping variable?
	
	u=as.factor(variable)
	groupCount = nlevels(u)
	groupNames = levels(u)
	grpRng = 1:groupCount

	rVar = factor(apply(as.array(recruiterID),1, function(x) variable[x]),levels=groupNames)	

	# Gives raw recruitment
	t=table(rVar, variable)
	
	# Proportional Recrutiment
	S = prop.table(t,1)
	S[which(S<=0)]=minXrec
	S[which(is.na(S))]=minXrec
	
	# Total Recruitment 
	#  of Group- RO
	#  by Group- RB
	RO = apply(t,2,sum)
	RB = apply(t,1,sum)
	

	#Recruitment Proportion - R
	R = RO/sum(t)
	


	
	# Sample Composition & Sample Degree Estimates
	Cc = rep(0 , groupCount)
	names(Cc) = groupNames
	
	seeds = rep(0 , groupCount)
	names(seeds) = groupNames
	
	meanDegree = rep(0 , groupCount)
	names(meanDegree) = groupNames
	wt=1/degree

	seed = is.na(recruiterID)
		
	for (i in grpRng ) {
		inGroupi = (variable==groupNames[i])
		Cc[i] = sum(inGroupi,na.rm=TRUE)
		seeds[i] = sum( (inGroupi & seed) )
		
		indexGroupi = which(( inGroupi & !(seed) ))
		meanDegree[i] = weighted.mean(degree[indexGroupi],wt[indexGroupi], na.rm=TRUE )

	}
	
	C = Cc/sum(Cc)	
	
	
	E.hat = rds.mcmc.props( S )
	t.dem.adj = sum( RB) * E.hat * S
	t.data.smoothed = (t.dem.adj + t(t.dem.adj))/2
	if ( method == "data.smoothing" ) {
		# Need to recalculate
		# row sum, column sums, selection prop, equilibria
		
		# Proportional Recrutiment
		S = prop.table(t.data.smoothed,1)
		S[which(S<=0)]=minXrec
		S[which(is.na(S))]=minXrec
		S = prop.table(S,margin=1)
		
		# Total Recruitment 
		#  of Group- RO
		#  by Group- RB
		#RO = apply(t.data.smoothed,2,sum)
		#RB = apply(t.data.smoothed,1,sum)
	
		#Recruitment Proportion - R
		R = RO/sum(t.data.smoothed)
		
		#Equilibrium		
		E.hat = rds.mcmc.props( S )
	}
	
	if (!(est.only)) {
		rec = list(sample.recruitment=t, recruitment.dem.adj=t.dem.adj, recruitment.smoothed=t.data.smoothed, proportion.recruitment=S, recruitment.of=RO, recruitment.by=RB,
				recruitment.proportion = R, comp.props=C, composition=Cc, seeds=seeds, groupNames = groupNames, method=method, rVar = rVar)
	}	
	

	
	# Population Estimates & Equilibrium
	
	#   Population Estimates			
	P.hat = rds.mcmc.props( S, meanDegree, method)

	# Store Estimate Variables
	est = list( E.hat.equilibrium=E.hat, P.hat.population.proportion=P.hat,
				multiplicity.mean.degree = meanDegree)	
				
				
				
	#Homophily
	if (FALSE) {
	for ( i in grpRng ) {
		if ( P.hat[i] <= S[i,i] ) {
			homophily.2002 = (P.hat[i] - S[i,i])/(P.hat[i]-1)
			#ah = ()/() 
		
		} else {
			homophily.2002 = (S[i,i])/(P.hat[i])-1
	
		}
	}
	}

	 
	# Group Sampling Weights
	# 	ratio of pop estimate P and sample proportion composition
	sampling.weights = rep(0,groupCount)
	names(sampling.weights) = groupNames
	#--Group Dual Weights
	#----Recruitment Component
	recruitment.component = rep(0 , groupCount)
	names(recruitment.component) = groupNames
	
	#----Degree Component
	degree.component = rep(0 , groupCount)
	names(degree.component) = groupNames

	for (i in grpRng ) {
		sampling.weights[i] = P.hat[i]/C[i]
		recruitment.component[i] = E.hat[i]/C[i]	
		degree.component[i] = P.hat[i]/E.hat[i]
	}
	
						 
	#Individual Weight Components
	
	# Imputation Setup
	#   Set Seeds to their group mean
	#   Set missing values to their group means
	
	# Calculate the Constants
	#  Recruitment Components
	RC = rep(0,dataLen)
	degree.imp = degree
	
	for (i in grpRng ) {
		inGroup = (variable==groupNames[i])
		degree.imp[which( seed & inGroup )] = meanDegree[i]
		degree.imp[which( is.na(degree) & inGroup )] = meanDegree[i]
		#  People with zero degree are treated as missing data
		degree.imp[which( degree <= 0 & inGroup )] = meanDegree[i]

		RC[which(inGroup)] = recruitment.component[i]
	}
	
		
	#  K is an estimate for the overall mean degree for respondents
	K = dataLen/sum(RC/degree.imp)
	#  DW is the individualized Dual Weight
	dual.weights = RC/degree.imp*K
	 	
	if (!(est.only)) {
		est = c(est, list(   imputed.degree = degree.imp,
						 group.sampling.weights = sampling.weights,
						 group.recruitment.component = recruitment.component, 
						 group.degree.component = degree.component,
						 weights.dual.component = dual.weights,
						 recruitment.components = RC,
						 K.constant = K,
						 degree.components = K/degree.imp ) )
	}
	
	
	# Adjusted Population Estimates
	parts = (length(na.omit(degree)) / group.size)^(.5)
	
	if (parts < 2) {
		parts = 2
	} else {
		parts = floor(parts)
	}
	
	breaks = quantile(degree, probs=seq(1/parts,1,1/parts) ,na.rm=TRUE)
	#print(breaks)
	
	deg.groupNames = 1:length(breaks)

	group = factor(apply(as.array(degree), 1, function(x) c(NA,1)[as.numeric(x >= 1)+1]),levels=deg.groupNames)
	
	for (i in 1:(parts-1)) {
		group[which(degree > breaks[i])] = i+1
	}
	
	rGrp = factor(apply(as.array(recruiterID), 1, function(x) group[x]),levels=deg.groupNames)

	
	# Gives raw recruitment
	deg.t=table(rGrp, group)
	
	# Proportional Recrutiment
	deg.S = prop.table(deg.t,1)
	deg.S[which(deg.S<=0)]=minXrec
	deg.S[which(is.na(deg.S))]=minXrec
		
	# Total Recruitment - RO
	deg.RO = apply(deg.t,2,sum)
	
	#Recruitment Proportion - R
	deg.R = deg.RO/sum(deg.t)
	
	#Sample Composition and Degree
	
	Cg = rep(0 , parts)
	names(Cg) = deg.groupNames
	
	deg.C = rep(0 , dataLen)

	for (i in 1:parts ) {
		Cg[i] = sum(group==i, na.rm=TRUE)
		deg.C[which(group == i)] = Cg[i]

	}
	Cg = Cg/sum(Cg)
	
	
 	# Degree Adjusted Equilibrium
	deg.E.hat = rds.mcmc.props( deg.S )

 	# Produce Vector of equilibiria values
 	deg.eq = rep(NA,length(degree))
 	for ( i in deg.groupNames) {
 		deg.eq[which(group == deg.groupNames[i])] = deg.E.hat[i]
 	}
	
	# Recruitment Component of Degree RCD
	deg.RCD = deg.eq/deg.C
	deg.RCD[which(seed)] = NA
		
	# Adjusted Degree
	RCDtoD = deg.RCD/degree
	aggRCDtoD = sapply(groupNames,function(x) sum((RCDtoD)[which(variable==x)],na.rm=TRUE) )
	aggRCD = sapply(groupNames,function(x) sum((deg.RCD)[which(variable==x)],na.rm=TRUE) )
	
	adj.degree = c(aggRCD/aggRCDtoD)

			
 	# Degree Adjusted Population Estimates 
	adj.P.hat = rds.mcmc.props( S, adj.degree) 
	
	if (!(est.only)) {
		adj = list( degree.partition.count=parts, adj.degree=adj.degree, 
				adj.E.hat=deg.E.hat, adj.P.hat = adj.P.hat )
	}
	if (est.only) {
		results = list(P.hat.population.proportion=P.hat, adj.P.hat = adj.P.hat)
	} else {
		results = c( rec, est, adj )
	}
	results
}


rds.set_groups_recruitment = function( variable, ID, recruiterID, degree, method="data.smoothing", use.weighted.degree=TRUE,
						 group.size = 12, groups=NULL, minXrec=.0001, estVH = FALSE, est.only=FALSE, ...) 
{

	# This function allow the user to specify the groups present in the population. 
	# As a result, this leads to the possibility of null values in the matricies when
	# expected groups are not present. As a convienience to the user, recruitment is
	# given a lower bound and groups with missing mean degree are assigned the mean degree
	# for all groups.

	#Should add some error checking
	# - args should be the same length
	# method should be recognized
	
	ok.methods = c("data.smoothing", "least.squares")
	if (!(method %in% ok.methods)) {
		stop(paste("*** Unimplemented Method ***"))
	}

	# How many levels of grouping variable?
	
	if ( is.null(groups)) {
		u=as.factor(variable)
	} else {
		u=factor(variable, levels=sort(groups))
	}

	groupCount = nlevels(u)
	groupNames = levels(u)
	grpRng = 1:groupCount
	
	
	recruiterID = apply(as.array(recruiterID),1,function(x) match(x,ID))
	ID = 1:length(ID)

	
	dataLen = length(ID)
	dataRng = 1:dataLen
	
	rVar = factor(apply(as.array(recruiterID),1, function(x) variable[x]),levels=groupNames)	

	# Gives raw recruitment
	T=table(rVar, u)
		
	# Proportional Recrutiment
	S = prop.table(T,1)
	S[which(S<=0)]=minXrec
	S[which(is.na(S))]=minXrec
	
	waves.req = rds.wave.est(S)
	waves.max = max(rds.waves(ID, recruiterID) )
	
	# Total Recruitment 
	#  of Group- RO
	#  by Group- RB
	RO = apply(T,2,sum)
	RB = apply(T,1,sum)
	
	#Recruitment Proportion - R
	R = RO/sum(T)
	
	# Sample Composition & Sample Degree Estimates
	Cc = rep(0 , groupCount)
	names(Cc) = groupNames
	
	seeds = rep(0 , groupCount)
	names(seeds) = groupNames
	
	meanDegree = rep(0 , groupCount)
	names(meanDegree) = groupNames
	
	if (use.weighted.degree) {
		wt=1/degree
	} else {
		wt=rep(1,length(degree))
	}

	seed = is.na(recruiterID)
		
	for (i in grpRng ) {
		inGroupi = (variable==groupNames[i])
		Cc[i] = sum(inGroupi,na.rm=TRUE)
		seeds[i] = sum( (inGroupi & seed) )
		
		indexGroupi = which(( inGroupi & !(seed) ))
		meanDegree[i] = weighted.mean(degree[indexGroupi],wt[indexGroupi], na.rm=TRUE )

	}
	meanDegree[which(is.na(meanDegree))]=mean(meanDegree, na.rm=TRUE)
	
	C = Cc/sum(Cc)
	
		S.raw = S
	S.raw[which(S.raw <= minXrec)]=0
	
	E.hat = rds.mcmc.props( S )
	t.dem.adj = sum( RB) * E.hat * S
	t.data.smoothed = (t.dem.adj + t(t.dem.adj))/2
	if ( method == "data.smoothing" ) {
		# Need to recalculate
		# row sum, column sums, selection prop, equilibria
		
		# Proportional Recrutiment
		S = prop.table(t.data.smoothed,1)
		S[which(S<=0)]=minXrec
		S[which(is.na(S))]=minXrec		
		S = prop.table(S,margin=1)

		# Total Recruitment 
		#  of Group- RO
		#  by Group- RB
		#RO = apply(t.data.smoothed,2,sum)
		#RB = apply(t.data.smoothed,1,sum)
	
		#Recruitment Proportion - R
		R = RO/sum(t.data.smoothed)
		
		#Equilibrium		
		E.hat = rds.mcmc.props( S )
	}
	
	rec = list(ID=ID, RID=recruiterID, sample.recruitment=T, recruitment.dem.adj=t.dem.adj, recruitment.smoothed=t.data.smoothed, proportion.recruitment=S, S.raw=S.raw, recruitment.of=RO, recruitment.by=RB,
				recruitment.proportion = R, comp.props=C, composition=Cc, seeds=seeds, mean.degree=meanDegree, groupNames = groupNames, method=method, rVar = rVar, waves.req = waves.req, waves.max=waves.max)
	return(c(rec))
}


rds.set_groups_estimate = function( variable, ID, recruiterID, degree, method="data.smoothing", use.weighted.degree=TRUE, group.size = 12, groups=NULL, minXrec=.0001, estVH = FALSE, est.only=FALSE, ...) 
{

	# This function allow the user to specify the groups present in the population. 
	# As a result, this leads to the possibility of null values in the matricies when
	# expected groups are not present. As a convienience to the user, recruitment is
	# given a lower bound and groups with missing mean degree are assigned the mean degree
	# for all groups.

	#print(paste("Est received:", group.size))
	#Should add some error checking
	# - args should be the same length
	# method should be recognized
	
	ok.methods = c("data.smoothing", "least.squares")
	if (!(method %in% ok.methods)) {
		stop(paste("*** Unimplemented Method ***"))
	}

	# How many levels of grouping variable?
	
	if ( is.null(groups)) {
		u=as.factor(variable)
	} else {
		u=factor(variable, levels=sort(groups))
	}

	groupCount = nlevels(u)
	groupNames = levels(u)
	grpRng = 1:groupCount
	
	
	recruiterID = apply(as.array(recruiterID),1,function(x) match(x,ID))
	ID = 1:length(ID)

	
	dataLen = length(ID)
	dataRng = 1:dataLen
	
	rVar = factor(apply(as.array(recruiterID),1, function(x) variable[x]),levels=groupNames)	

	# Gives raw recruitment
	T=table(rVar, u)
		
	# Proportional Recrutiment
	S = prop.table(T,1)
	S[which(S<=0)]=minXrec
	S[which(is.na(S))]=minXrec

	waves.req = rds.wave.est(S)
	waves.byseed = rds.waves(ID, recruiterID)
	waves.max = max( waves.byseed )

	# Total Recruitment 
	#  of Group- RO
	#  by Group- RB
	RO = apply(T,2,sum)
	RB = apply(T,1,sum)
	
	#Recruitment Proportion - R
	R = RO/sum(T)
	
	# Sample Composition & Sample Degree Estimates
	Cc = rep(0 , groupCount)
	names(Cc) = groupNames
	
	seeds = rep(0 , groupCount)
	names(seeds) = groupNames
	
	meanDegree = rep(0 , groupCount)
	names(meanDegree) = groupNames
	
	if (use.weighted.degree) {
		wt=1/degree
	} else {
		wt=rep(1,length(degree))
	}

	seed = is.na(recruiterID)
		
	for (i in grpRng ) {
		inGroupi = (variable==groupNames[i])
		Cc[i] = sum(inGroupi,na.rm=TRUE)
		seeds[i] = sum( (inGroupi & seed) )
		
		indexGroupi = which(( inGroupi & !(seed) ))
		meanDegree[i] = weighted.mean(degree[indexGroupi],wt[indexGroupi], na.rm=TRUE )

	}
	meanDegree[which(is.na(meanDegree))]=mean(meanDegree, na.rm=TRUE)
	
	C = Cc/sum(Cc)	
	
	#browser()
	S.raw = S
	S.raw[which(S.raw <= minXrec)]=0
	
	E.hat = rds.mcmc.props( S )
	t.dem.adj = sum( RB) * E.hat * S
	t.data.smoothed = (t.dem.adj + t(t.dem.adj))/2
	if ( method == "data.smoothing" ) {
		# Need to recalculate
		# row sum, column sums, selection prop, equilibria
		
		# Proportional Recrutiment
		S = prop.table(t.data.smoothed,1)
		S[which(S<=0)]=minXrec
		S[which(is.na(S))]=minXrec		
		S = prop.table(S,margin=1)

		# Total Recruitment 
		#  of Group- RO
		#  by Group- RB
		#RO = apply(t.data.smoothed,2,sum)
		#RB = apply(t.data.smoothed,1,sum)
	
		#Recruitment Proportion - R
		R = RO/sum(t.data.smoothed)
		
		#Equilibrium		
		E.hat = rds.mcmc.props( S )
	}
	
	if (!(est.only)) {
		rec = list(ID=ID, RID=recruiterID, sample.recruitment=T, recruitment.dem.adj=t.dem.adj, recruitment.smoothed=t.data.smoothed, proportion.recruitment=S, S.raw=S.raw, recruitment.of=RO, recruitment.by=RB,
				recruitment.proportion = R, comp.props=C, composition=Cc, seeds=seeds, mean.degree=meanDegree, groupNames = groupNames, method=method, rVar = rVar, waves.req = waves.req, waves.max=waves.max, waves.bychain=waves.byseed)
	}	
	

	
	# Population Estimates & Equilibrium
	
	#   Population Estimates			
	P.hat = rds.mcmc.props( S, meanDegree, method)

	#Homophily
	homophily.2002 = rep(0,groupCount)
	for ( i in grpRng ) {
		if ( !( is.na(P.hat[i]) | is.na(S[i,i]) ) ) {
			if ( P.hat[i] <= S[i,i] ) {
				homophily.2002[i] = (P.hat[i] - S[i,i])/(P.hat[i]-1)
				#ah = ()/() 
			} else {
				homophily.2002[i] = (S[i,i])/(P.hat[i])-1	
			}
			if (S.raw[i,i] == sum(S.raw[,i])) {
				homophily.2002[i] = NA
			}
		}
	}
	
	# Store Estimate Variables
	if (estVH) {
		est.VH = rds.VH.estimate( variable, ID, recruiterID, degree, groups )
		
		est = list( E.hat.equilibrium=E.hat, P.hat.population.proportion=P.hat,
					VH.P.hat = est.VH$RDS.VH.estimate,
				    multiplicity.mean.degree = meanDegree, Homophily = homophily.2002)
	} else {
		est = list( E.hat.equilibrium=E.hat, P.hat.population.proportion=P.hat,
				multiplicity.mean.degree = meanDegree, Homophily = homophily.2002)	
	}
	
	# Group Sampling Weights
	# 	ratio of pop estimate P and sample proportion composition
	sampling.weights = rep(0,groupCount)
	names(sampling.weights) = groupNames
	#--Group Dual Weights
	#----Recruitment Component
	recruitment.component = rep(0 , groupCount)
	names(recruitment.component) = groupNames
	
	#----Degree Component
	degree.component = rep(0 , groupCount)
	names(degree.component) = groupNames

	for (i in grpRng ) {
		sampling.weights[i] = P.hat[i]/C[i]
		recruitment.component[i] = E.hat[i]/C[i]	
		degree.component[i] = P.hat[i]/E.hat[i]
	}
	
						 
	#Individual Weight Components
	
	# Imputation Setup
	#   Set Seeds to their group mean
	#   Set missing values to their group means
	
	# Calculate the Constants
	#  Recruitment Components
	RC = rep(0,dataLen)
	degree.imp = degree
	
	for (i in grpRng ) {
		inGroup = (variable==groupNames[i])
		degree.imp[which( seed & inGroup )] = meanDegree[i]
		degree.imp[which( is.na(degree) & inGroup )] = meanDegree[i]
		#  People with zero degree are treated as missing data
		degree.imp[which( degree <= 0 & inGroup )] = meanDegree[i]

		RC[which(inGroup)] = recruitment.component[i]
	}
	
		
	#  K is an estimate for the overall mean degree for respondents
	K = dataLen/sum(RC/degree.imp)
	#  DW is the individualized Dual Weight
	dual.weights = RC/degree.imp*K
	 	
	if (!(est.only)) {
		est = c(est, list(   imputed.degree = degree.imp,
						 group.sampling.weights = sampling.weights,
						 group.recruitment.component = recruitment.component, 
						 group.degree.component = degree.component,
						 weights.dual.component = dual.weights,
						 recruitment.components = RC,
						 K.constant = K,
						 degree.components = K/degree.imp ) )
	}
	
	
	# Adjusted Population Estimates
	parts = (length(na.omit(degree)) / group.size)^(.5)
	if (parts < 2) {
		parts = 2
	} else {
		parts = floor(parts)
	}
	
	breaks = quantile(degree, probs=seq(1/parts,1,1/parts) ,na.rm=TRUE)
	#print(breaks)
	
	deg.groupNames = 1:length(breaks)

	group = factor(apply(as.array(degree), 1, function(x) c(NA,1)[as.numeric(x >= 1)+1]),levels=deg.groupNames)
	
	for (i in 1:(parts-1)) {
		group[which(degree > breaks[i])] = i+1
	}
	
	rGrp = factor(apply(as.array(recruiterID), 1, function(x) group[x]),levels=deg.groupNames)

	
	# Gives raw recruitment
	deg.t=table(rGrp, group)
	
	# Proportional Recrutiment
	deg.S = prop.table(deg.t,1)
	deg.S[which(deg.S<=0)]=minXrec
	deg.S[which(is.na(deg.S))]=minXrec	
	
	# Total Recruitment - RO
	deg.RO = apply(deg.t,2,sum)
	
	#Recruitment Proportion - R
	deg.R = deg.RO/sum(deg.t)
	
	#Sample Composition and Degree
	
	Cg = rep(0 , parts)
	names(Cg) = deg.groupNames
	
	deg.C = rep(0 , dataLen)

	for (i in 1:parts ) {
		Cg[i] = sum(group==i, na.rm=TRUE)
		deg.C[which(group == i)] = Cg[i]

	}
	Cg = Cg/sum(Cg)
	
	
 	# Degree Adjusted Equilibrium
	deg.E.hat = rds.mcmc.props( deg.S )

 	# Produce Vector of equilibiria values
 	deg.eq = rep(NA,length(degree))
 	for ( i in deg.groupNames) {
 		deg.eq[which(group == deg.groupNames[i])] = deg.E.hat[i]
 	}
	
	# Recruitment Component of Degree RCD
	deg.RCD = deg.eq/deg.C
	deg.RCD[which(seed)] = NA
		
	# Adjusted Degree
	RCDtoD = deg.RCD/degree
	aggRCDtoD = sapply(groupNames,function(x) sum((RCDtoD)[which(variable==x)],na.rm=TRUE) )
	aggRCD = sapply(groupNames,function(x) sum((deg.RCD)[which(variable==x)],na.rm=TRUE) )
	
	adj.degree = c(aggRCD/aggRCDtoD)

	adj.degree[which(is.na(adj.degree))]=mean(adj.degree, na.rm=TRUE)
		
 	# Degree Adjusted Population Estimates 
	adj.P.hat = rds.mcmc.props( S, adj.degree) 
	
	# Adjusted Homophily
	ahomophily.2002 = rep(0,groupCount)
	for ( i in grpRng ) {
		if ( !(is.na(P.hat[i]) | is.na(S[i,i])) ) {
	
			if ( P.hat[i] <= S[i,i] ) {
				ahomophily.2002[i] = (adj.P.hat[i] - S[i,i])/(adj.P.hat[i]-1)
				#ah = ()/() 
			} else {
				ahomophily.2002[i] = (S[i,i]/adj.P.hat[i])-1	
			}
			if (S.raw[i,i] == sum(S.raw[,i])) {
				homophily.2002[i] = NA
			}
		}
	}
	
	if (!(est.only)) {
		adj = list( degree.partition.count=parts, adj.degree=adj.degree, 
				adj.E.hat=deg.E.hat, adj.P.hat = adj.P.hat, adj.Homophily = ahomophily.2002 )
	}
	if (est.only) {
		results = list(P.hat.population.proportion=P.hat, adj.P.hat = adj.P.hat)
	} else {
		results = c( rec, est, adj )
	}
	results
}

rds.set_groups_bootstrap = function( variable, ID, recruiterID, degree, method="data.smoothing",
						 group.size = 12, groups=NULL, minXrec=.0001, est.only=FALSE,
             trials=1000, ci.alpha=.025, ...) 
{
	# This function may exhibit unexpected behavior when ID is not of mode "numeric"
	# Levels of var should be coded 1,2,3,..
	if (is.null(groups)) {
		variable = factor(variable)
		levels(variable) = 1:nlevels(variable)
	} else {
		variable = factor(variable)
		groups = 1:length(groups)
		levels(variable) = groups
	}
	# Generate Point Estimate for Variable
	actual = rds.set_groups_estimate(variable, ID, recruiterID, degree, method=method, group.size=group.size, 
									 groups=groups, minXrec=minXrec)
	ID = actual$ID
	recruiterID = actual$RID
	
	
	srsVar = sapply(actual$P.hat, function(x) x*(1-x)/length(ID) )
	srsVar.adj = sapply(actual$adj.P.hat, function(x) x*(1-x)/length(ID) )
	
	# Generate lists of people according to recruiter's variable
	index=list()
	seeds=list()
	for (i in 1:length(actual$groupNames) ) {
		index[i] = list(ID[which((actual$rVar==actual$groupNames[i]) & !is.na(variable) )])
		seeds[i] = list(ID[which((variable==actual$groupNames[i]) & is.na(actual$rVar) )])
		if (length(index[[i]]) < 1 ) { index[i] = list(ID[which(!is.na(variable))]) }
	}
	
	seeds=(unlist(seeds))
	seedCount = length(seeds)
	
	# Generate list to hold the results
	results = list()
	results$P.hat = matrix(nrow=trials,ncol=length(actual$groupNames))
	results$adj.P.hat = matrix(nrow=trials,ncol=length(actual$groupNames))

	#data = as.data.frame(cbind(variable, ID, recruiterID, degree))
	#names(data) = c("variable", "ID", "recruiterID", "degree")
	recruitable = unlist(index)
	size = length(recruitable)
	
	bs.size = length(seeds)+size
	lcm.size = seedCount*ceiling(bs.size/seedCount)  ##Why?
	
	# Set up variables for resample
	re.ID  = seq(1,bs.size)
	re.var = vector("character",bs.size)
	re.RID = c(rep(NA,seedCount), seq(1,bs.size-seedCount))
	re.degree = vector("numeric", bs.size )
	
	re.RID[1:seedCount] = NA
	curVar = as.vector(variable[seeds])
	curWave= as.vector(seeds)
	
	
	# Need to take sample from all seeds simlultaneously.
	#  - determine how to terminate when enough seeds are added
	#
	#
	
	# Generate samples, calculate and record the estimates
	for ( i in 1:trials) {
		re.samp = matrix(vector("numeric", length=lcm.size),nrow=seedCount)
		re.seeds = sample(seeds)
		re.samp[1:seedCount] = re.seeds
		curVar = variable[re.seeds]
		
		re.var[1:seedCount]=curVar

		
		
		
		# select sample from seed
		for ( j in 2:(lcm.size/seedCount)) {
			#print(re.samp)
			#print(index)
			#print(curWave)
			#print(curVar)

			curWave = (sapply(curVar, function(x) sample(index[[x]],1)))
			re.samp[,j] = curWave
			curVar = variable[curWave]
			
		}
		re.samp.vec = as.vector(re.samp)[1:bs.size]
		re.var = variable[re.samp.vec]
		re.degree = degree[re.samp.vec]
		
		# create data.frame
		
		#rd = as.data.frame(cbind(re.ID,re.var,re.RID,re.degree))
		foo = rds.set_groups_estimate(re.var, re.ID, re.RID, re.degree, method=method, group.size=group.size, 
									 groups=groups, minXrec=minXrec, est.only=TRUE)
		results$P.hat[i,] = foo$P.hat
		results$adj.P.hat[i,] = foo$adj.P.hat

		#print(i)
	}
	results.ci = lapply(results, function(x) apply(x,2,"quantile", c(ci.alpha,1-ci.alpha), na.rm=TRUE ))
	results.var = lapply(results, function(x) apply(x,2,"var",na.rm=TRUE) )
	DE=list()
	DE$P.hat = results.var$P.hat/srsVar
	DE$adj.P.hat = results.var$adj.P.hat/srsVar.adj
	c( ci = results.ci, rds.var = results.var, DE = DE,
						list( trials=trials, na.P.hat=sum(is.na(results$P.hat[,1])),
						 na.adj.P.hat=sum(is.na(results$adj.P.hat[,1]))					
					   ) 
	) 
}	


# Homophily Measures
# need to create and load an edgelist
# also need data loaded and indexed correctly
#
# from = apply(as.array(edges$from),1,function(x) match(x,fb$id))
# to = apply(as.array(edges$to),1,function(x) match(x,fb$id))
#
# Prints to terminal
 

#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


if( FALSE ) {

rdsData = read.table("/Users/cjc73/Documents/Education/Cornell/Research/RDS/Datasets/rdsData.txt",header=TRUE)
rdsDataB = read.table("/Users/cjc73/Documents/Education/Cornell/Research/RDS/Datasets/rdsDataB.txt",header=TRUE)
jazz = read.rds("/Users/cjc73/Documents/Education/Cornell/Research/RDS/Datasets/nyjazzFinal.txt")

rdsData = as.data.frame(rdsData)

source("/Users/cjc73/Documents/Education/Cornell/Research/RDS/socMet2007/RDS.R")

source("/Users/cjc73/Documents/RDS/RDS.R")
 
 
 
 
# Homophily Measures
#
# Standard Homophily 
#  if Px <= Sxx :  (Px_hat - Sxx)/( Px_hat - 1) 
#  if Px > Sxx : (Sxx/Px) - 1
#
 
 



rDeg = rep(NA, length(jazz$ID))

for ( i in (1:length(jazz$ID)) ) {
	rDeg[i] = length(which(jazz$RID==jazz$ID[i]))
}

rcDeg = rep(NA, length(jazz$ID))
for ( i in (1:length(jazz$ID)) ) {
	rcDeg[i] = rDeg[match(jazz$RID[i], jazz$ID)]
}


length(which(jazz$RID==1))



urDeg = rep(NA, length(rdsData$ID))
for ( i in (1:length(rdsData$ID)) ) {
	urDeg[i] = length(which(rdsData$RID==rdsData$ID[i]))
}

urcDeg = rep(NA, length(rdsData$ID))
for ( i in (1:length(rdsData$ID)) ) {
	urcDeg[i] = urDeg[match(rdsData$RID[i], rdsData$ID)]
}






}


































