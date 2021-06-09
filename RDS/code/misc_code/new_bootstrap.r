require(compiler)
enableJIT(3)

library(MASS)

generate_state_seq = function( seq_len = 10, states = c('A', 'B'), probs = matrix(NA, 2, 2), initial_state_pos=NA ) {
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

rds.mcmc.props = function( S, weights=NULL, method="data.smoothing")
{
	# Returns the Fixed Vector (W) for Transition Probs when weights=NULL
	#
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

fundamental_matrix = function( trans_matrix ) {
	# Calculate the fundamental matrix of transition matrix
	#  Z = (I-P+W)^-1
	W = matrix(rep(rds.mcmc.props(trans_matrix), nrow(trans_matrix)), nrow=nrow(trans_matrix), ncol=nrow(trans_matrix), byrow=TRUE)
	z_inv = diag(x=1, nrow=nrow(trans_matrix), ncol=nrow(trans_matrix) ) - trans_matrix + W
	
	return( ginv(z_inv) )
}

#asym_mean_count = n*w_j where n is sample size
asym_var_count = function(probs) {
	w = rds.mcmc.props(probs)
	z = fundamental_matrix(probs)
	return( 2* (w*diag(z)) - w - w^2 )
}

# Generate a bootstrapped CI for estimate (no variance in transition probs)
simple_bs = function(S, degree_weights_lists, trials=100, one_tail_alpha=.025 ) {
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(unlist(degree_weights_lists))
	bs_mean_deg = unlist(lapply(lapply(degree_weights_lists, mean), '^', -1))
	overall_mean = mean(bs_mean_deg)

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
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	
	return(bs_ci)
}

# Generate a bootstrapped CI for estimate (with variance in transition probs)
trans_var_bs = function(S, degree_weights_lists, trials=100, one_tail_alpha=.025, fudge=1) {
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(unlist(degree_weights_lists))
	bs_mean_deg = unlist(lapply(lapply(degree_weights_lists, mean), '^', -1))
	overall_mean = mean(bs_mean_deg)
	state_count = nrow(S) 
	#print(bs_results)
	#print(sample_size)
	#print(bs_mean_deg)
	
	
	expected_sd = apply(S, c(1,2), function(x) sqrt(x*(1-x)/(sample_size/fudge)) )
	
	
	for (j in 1:trials) {
		# Add some variation in the S matrix
		# Create some noise using expected sd
		noise = apply(expected_sd, c(1,2), function(x) rnorm(1, sd=x))
		
		#if (FALSE) {
			## Correct for dependence of proportions 
			## Might only work for 2 states, non-zero recruitment
			noise[,state_count] = noise[,1:(state_count-1)]*-1
			
			# Re-normalize S after applying noise
			S_noise = S+noise
			S_noise[S_noise<0] = 0 
		#}
		
		
		S_p = prop.table(S+noise, 1)
		
		#Bootstrap a sample composition and transition probs
		bs_sample = generate_state_seq(seq_len=sample_size, probs=S_p)		
		
		rec = matrix(0, state_count, state_count)
		for (i in 2:length(bs_sample)) {
			rec[bs_sample[i-1], bs_sample[i]] = rec[bs_sample[i-1], bs_sample[i]]+1
		}
		
		bs.props = prop.table(rec, 1)
		bs.cnum  = as.vector(table( factor(bs_sample, levels=c(1:state_count) ) ))
		#print(bs.props)
		#print(bs.cnum)
		
		#Bootstrap a degree for each group, based on the bootstrapped sample composition 
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
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	
	return(bs_ci)
}

simple_wts_bs = function(vals_list, weights_list, trials=100, one_tail_alpha=.025 ) {	
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(weights_list)
	state_count = length( unique(vals_list) ) 
	states = sort(unique(vals_list)) 
	for (j in 1:trials) {
		#Bootstrap a sample 
		bs_sample = sample(vals_list, size=sample_size, replace=TRUE , prob=weights_list )	

		bs_results[j,] = prop.table( table(factor(bs_sample, levels=states)) )
	}
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	
	return(bs_ci)
}

var_wts_bs = function(vals_list, rc_weights_list, dc_weights_list, trials=100, one_tail_alpha=.025 ) {	
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(weights_list)
	states = sort(unique(vals_list))
    state_count = length( states ) 
	
	# use vals to partition weights list and get a distribution for each unique val
	rc_lists = list()
	dc_lists - list()
	
	for (i in state_count) {
		bool_membership = vals_list==state[i]
		rc_lists[[i]] = rc_weights_list[ bool_membership ]
		dc_lists[[i]] = dc_weights_list[ bool_membership ]	
	}
	
	#For each trial
	# permute the weights list within groups
	
	for (j in 1:trials) {
		#Bootstrap a sample 
		bs_sample = sample(vals_list, size=sample_size, replace=TRUE , prob=weights_list )	

		bs_results[j,] = prop.table( table(factor(bs_sample, levels=states)) )
	}
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	
	return(bs_ci)
}

# Generate a bootstrapped CI for estimate (with variance in transition probs)
two_step_trans_var_bs = function(S, degree_weights_lists, trials=100, one_tail_alpha=.05) {
	bs_results = matrix(nrow=trials, ncol=ncol(S))
	sample_size = length(unlist(degree_weights_lists))
	bs_mean_deg = unlist(lapply(lapply(degree_weights_lists, mean), '^', -1))
	state_count = nrow(S) 
	#print(bs_results)
	#print(sample_size)
	#print(bs_mean_deg)
	
	
	#Generate a set of draws for n_i and n_ij
	w = rds.mcmc.props(S)
	asym_var = asym_var_count(probs)
	
	# random permutations of the transition matrix S
	S_p = replicate(trials, S)   # random permutations of the transition matrix S
	bs_mean_deg_replicates = replicate(trials, bs_mean_deg)
	
	
	# Populate the permutations
	for (i in 1:nrow(S)) {
		# Sample the number of each state in sample
		n_sub_i = round(rnorm(trials, sd=sqrt(asym_var[i] * sample_size))+ sample_size*w[i])
		
		# Sample the mean degree for each state group
		bs_mean_deg_replicates[i,] = sapply( n_sub_i,  function(x) x / sum( sample(degree_weights_lists[[i]], size=x, replace=TRUE) ) )

		# Sample the transitions from each group ( n_ij, then calculate permuted transitions S )
		S_p[i,,] = prop.table(sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=S[i,] )), 2)
	}
	head(bs_mean_deg_replicates)
	head(S_p)
	
	for (j in 1:trials) {
		bs_results[j,] = rds.mcmc.props(S_p[,,j], bs_mean_deg_replicates[,j])
	}
	#print(bs_results)
	bs_ci = as.vector(apply( bs_results, 2, quantile, c(one_tail_alpha, 1-one_tail_alpha), na.rm=TRUE))
	
	return(bs_ci)
}


sim_transition_probs = function(num_trials, sample_size, probs ) {
	# We think the variance for resampled markov chains should be p*(1-p)/n_i when n_i is fixed
	# Need to test that theory with MCMC samples
	results = list()
	
	for(k in 1:nrow(probs)) {
		results[[k]] = matrix(data=NA, nrow=num_trials, ncol=ncol(probs))
	}
	
	
	for (trial in 1:num_trials ) {
		#Generate State Sequence
		foo = generate_state_seq(sample_size, probs=probs)
		
		state_count = length(unique(foo))
		rec = matrix(0, state_count, state_count)
		
		for (i in 2:length(foo)) {
			rec[foo[i-1], foo[i]] = rec[foo[i-1], foo[i]]+1
		}
		
		S = prop.table(rec, 1)
		
		for(k in 1:nrow(probs)) {
		    results[[k]][trial,] = S[k,]
	    }
	
	}
	results_list = lapply(results,function(x) apply(x, 2, var)) 
	return( matrix(unlist(results_list), nrow=nrow(probs), ncol=ncol(probs), byrow=TRUE))	
}

sim_transition_probs_mean = function(num_trials, sample_size, probs ) {
	# We think the variance for resampled markov chains should be p*(1-p)/n_i when n_i is fixed
	# Need to test that theory with MCMC samples
	results = list()
	
	for(k in 1:nrow(probs)) {
		results[[k]] = matrix(data=NA, nrow=num_trials, ncol=ncol(probs))
	}
	
	
	for (trial in 1:num_trials ) {
		#Generate State Sequence
		foo = generate_state_seq(sample_size, probs=probs)
		
		state_count = length(unique(foo))
		rec = matrix(0, state_count, state_count)
		
		for (i in 2:length(foo)) {
			rec[foo[i-1], foo[i]] = rec[foo[i-1], foo[i]]+1
		}
		
		S = prop.table(rec, 1)
		
		for(k in 1:nrow(probs)) {
		    results[[k]][trial,] = S[k,]
	    }
	
	}
	results_list = lapply(results,function(x) apply(x, 2, mean)) 
	return( matrix(unlist(results_list), nrow=nrow(probs), ncol=ncol(probs), byrow=TRUE))	
}

sim_state_count = function(num_trials, sample_size, probs ) {
	# We think the variance for resampled markov chains should be p*(1-p)/n_i when n_i is fixed
	# Need to test that theory with MCMC samples
	
	results = matrix(data=NA, nrow=num_trials, ncol=ncol(probs))

	for (trial in 1:num_trials ) {
		#Generate State Sequence
		foo = generate_state_seq(sample_size, probs=probs)
		
		state_count = length(unique(foo))
		rec = matrix(0, state_count, state_count)
		
		for (i in 2:length(foo)) {
			rec[foo[i-1], foo[i]] = rec[foo[i-1], foo[i]]+1
		}
		
		S = rowSums(rec)
		
		results[trial,] = S
	
	}
	
	return( apply(results, 2, var) )
	
}

two_step_var_est = function(num_replications, sample_size, probs) {
	w = rds.mcmc.props(probs)
	results = matrix(NA,nrow=nrow(probs), ncol=nrow(probs))
	asym_var = asym_var_count(probs)
	
	for (i in 1:nrow(probs)) {
			n_sub_i = round(rnorm(num_replications, sd=sqrt(asym_var[i] * sample_size))+ sample_size*w[i])
			results[i,] = apply(prop.table(sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=probs[i,] )), 2), 1, var )
	}
	return(results)
}

two_step_var_est_const_sample_size = function(num_replications, sample_size, probs) {
	#This version holds sample size constant by setting the size of the last group as a function of 
	# the other groups. Might still produce large samples if the first n-1 groups get large draws
	w = rds.mcmc.props(probs)
	results = matrix(NA,nrow=nrow(probs), ncol=nrow(probs))
	asym_var = asym_var_count(probs)

	n_sub = matrix(0,nrow=nrow(probs),ncol=num_replications)
	for (i in 1:nrow(probs)) {
		if(i==nrow(probs)){
			n_sub[i,] = apply(n_sub, 2, function(x) max(0, sample_size-sum(x)))
		} else {
		    n_sub[i,] = round(rnorm(num_replications, sd=sqrt(asym_var[i] * sample_size))+ sample_size*w[i])
		}
		results[i,] = apply(prop.table(sapply(n_sub[i,], function(x) rmultinom(1, size=x, prob=probs[i,] )), 2), 1, var )
	}	
	return(results)
}

two_step_mean_est = function(num_replications, sample_size, probs) {
	w = rds.mcmc.props(probs)
	results = matrix(NA,nrow=nrow(probs), ncol=nrow(probs))
	asym_var = asym_var_count(probs)
	
	for (i in 1:nrow(probs)) {
			n_sub_i = round(rnorm(num_replications, sd=sqrt(asym_var[i] * sample_size))+ sample_size*w[i])
			results[i,] = apply(prop.table(sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=probs[i,] )), 2), 1, mean )
	}
	return(results)
}



run_interactive = function() {
# --------------------------------------------------------------------------------------------
# ---- this code is protected by the function -------------------------------
# --------------------------------------------------------------------------------------------

degree_listA = c( 350,  150, 100, 300, 700, 200, 200, 300, 300, 200, 100, 400, 200, 150, 100, 200, 50, 500, 30, 500, 500, 25, 150, 50, 500, 150, 300,  250, 400,  250, 100, 200,  100, 150, 150, 60, 500, 100, 200, 200, 150, 20,  200, 500, 100, 700, 300, 200, 500, 125, 500, 250, 100, 200, 30,  150, 400,  100,  100, 100, 200, 100, 250, 50, 60, 100, 150, 500, 200, 200, 100, 100,  100, 150,  70, 400, 25, 50, 200, 300, 250, 50, 200, 100, 500, 150, 600, 100, 500, 150, 200, 300, 200, 150, 300, 100, 500, 200, 200, 100, 125, 50, 500, 50, 200, 500, 500, 300, 200, 100, 70, 300, 75, 300,  500, 100, 100, 125, 500,  200, 200, 500, 25, 80, 150, 120, 150,  500, 250,  150, 25, 30, 50, 30, 200,  70,  100, 100, 50, 100, 100, 100, 50, 100, 400, 25, 200, 800, 30, 400, 100, 100, 150, 40, 250, 300, 200,  300, 200, 800, 400, 400, 500, 450, 600, 300, 70, 100, 100, 300, 75, 500, 40, 400 )

degree_listB = c(585, 400, 300, 100, 383, 700, 80, 150, 300, 850,  200, 300, 150, 300, 300, 200, 150, 100, 150, 600, 300, 50, 300, 100, 700, 300, 100, 35, 70, 500, 150, 200,  20, 250, 100, 500, 20, 150, 90, 200, 20, 150,  30, 100, 200, 100, 60, 600, 100,  375, 50, 200, 300, 100, 100, 200, 75, 100, 220, 300, 500, 400, 100, 50)


degree_weights_list = list(degree_listA^-1, degree_listB^-1)

	
afterA_probs = c(0.8414634, 0.1585366)
afterB_probs = c(0.5250000, 0.4750000)

probs = matrix( c(afterA_probs, afterB_probs), 2,2,byrow=TRUE)

sample_size = 246



#Get the observed trans
foo = generate_state_seq(sample_size, probs=probs)

state_count = length(unique(foo))
rec = matrix(0, state_count, state_count)

for (i in 2:length(foo)) {
	rec[foo[i-1], foo[i]] = rec[foo[i-1], foo[i]]+1
}

S = prop.table(rec, 1)


cnum = as.vector(table(foo))
# Get Degree Seq by group, invert, bootstrap based on the counts in bootstrapped state_seq

# deg_wt_list is a list of 1/degrees by group, so deg_list[[1]] is the degrees of state index 1  
bs_mean_deg = unlist(lapply(lapply(deg_wt_list, mean), '^', -1))  #initialize with group means, probably not necessary

for (i in 1:length(bs_mean_deg)){
	bs_mean_deg[i] = cnum[i] / sum( sample(deg_wt_list[[i]], size=cnum[i], replace=TRUE) )
} 




bs_results = c()
sample_size = c()
bs_mean_deg = c()


w_2 = rds.mcmc.props(S)




sim_transition_probs(100000, sample_size, probs)

# [1] 0.0007175294 0.0007175294
# [1] 0.004568726 0.004568726

apply(prop.table(rmultinom(1000000, size = 195, prob = w_2),2), 1, var)
#           1            2 
#0.0008414084 0.0008414084 

apply(prop.table(rmultinom(1000000, size = 195, prob = afterA_probs),2), 1, var)
# 0.0006855071

apply(prop.table(rmultinom(1000000, size = 50, prob = afterB_probs),2), 1, var)
# 0.004999966



sim_transition_probs(10000, sample_size, probs=matrix(.5,nrow=2, ncol=2))
#0.002080681

apply(prop.table(rmultinom(1000000, size = 195, prob = c(.5, .5)),2), 1, var)
# 0.001282336

apply(prop.table(rmultinom(1000000, size = 50, prob = c(.5, .5)),2), 1, var)
# 0.005005388






#Need to sample number from each group
# VAR[S_j(n)] ~ 2 * w_j * z_jj - w_j- w_j^2

var(rnorm(10000000, sd=sqrt(.343*246))+246*.79 )   #Something like the expected variation of count in each state
# 84.397

var(rnorm(10000000, sd=sqrt(asym_var_count(probs)*246))+246*rds.mcmc.props(probs)[1] )

sim_state_count(100000, 246, probs)
# 83.481

var(rnorm(10000000, sd=sqrt(asym_var_count(probs)*1000))+1000*rds.mcmc.props(probs)[1] )

sim_state_count(10000, 1000, probs)
# 83.481

#Lets simulate the variance of the ratio of n_ij / n_i
#First get the number of n_i:
sample_size = 246
asym_var = asym_var_count(probs)
e_hats = rds.mcmc.props(probs)

n_sub_i = round(rnorm(100000, sd=sqrt(asym_var[1] * sample_size))+ sample_size*e_hats[1])

#n_sub_ij = sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=afterA_probs)

#var_prop = apply(prop.table(sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=afterA_probs) ), 2), 1, var )
apply(prop.table(sapply(n_sub_i, function(x) rmultinom(1, size=x, prob=afterA_probs) ), 2), 1, var )

#.0007 vs .00068  --- not bad, need to check the p=c(.5, .5)


two_step_var_est(100000, 300, matrix(.5,2,2))
#[1,] 0.0008405214 0.0008405214
#[2,] 0.0008378114 0.0008378114


sim_transition_probs(10000, 300, matrix(.5,2,2))
#[1] 0.0008397932 0.0008397932
#[1] 0.0008414575 0.0008414575

# Looks like variance is accurate to within ~2%



# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

}

# Test the percent error in the modeled variance compared to MC 
percent_errors_2_groups = function( reps=100, size=300, p_ij=c(.1, .25, .5, .75, .90) ){
	results = matrix(nrow=length(p_ij), ncol=length(p_ij), dimnames=list(p_ij, p_ij))
	
	for( i in 1:length(p_ij) ) {
		for( j in 1:length(p_ij) ){
			probs = matrix( c(1-p_ij[i], p_ij[j], p_ij[i], 1-p_ij[j]) , 2,2)
			#print(probs)
			mc = sim_transition_probs(reps, size, probs)
			model = two_step_var_est_const_sample_size(reps, size, probs)
			results[i,j] = max((mc-model)/mc)
		}
	}
	results
}




