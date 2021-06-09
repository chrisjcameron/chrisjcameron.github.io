library(Matrix)
library(igraph)
source('C:\\RDS\\code\\RDS3.R')

#Function to exponentiate a matrix
# r-help, https://stat.ethz.ch/pipermail/r-help/2006-September/113542.html
# This one has trouble with numerical stability. 
# expM <- function(X,e) { v <- La.svd(X); v$u %*% diag(v$d^e) %*% v$vt }

# Slower but more numerically stable
mat_pow = function(G, e) {
	result = matrix(G, nrow=nrow(G), ncol=ncol(G))
	if(e == 1) { return(result)}
	for (i in 2:e) {
		result = result %*% G
		#print("EXP Mat")
	}
    return( result )
}
	


var_rw = function(g, steps, covar_fn='RW') {
	# covar_fn is 
	#  'RW'  for random walk
	#  'RDS' for RDS weighted
	#cov_mat = matrix(NA, nrow=steps, ncol=steps)
	#cov_mat = outer(1:steps, 1:steps, FUN=covar_fn, g=g, cv_type=covar_fn )
	
	# Set up a matrix for results:
	node_count = vcount(g)
	cov_mat = matrix(NA, nrow=steps, ncol=steps)
	
	# Precompute some values:
	W = outer(degree(g), degree(g))^-1
	sum_W = sum(W)
	#norm_W = sum_W/W
	norm_W = W / sum_W

	
	val_Y = V(g)$aids   ## This depends on name of attibute
	
	P_s = degree(g)/sum(degree(g))   # assume node is selected at random proportional to degree
	mean_Y = t(P_s) %*% val_Y
	Y = outer(val_Y-mean_Y, val_Y-mean_Y)
	
	M_tran_probs = get.adjacency(g) / degree(g)
	
	
	#cov_mat = norm_W * P_s * P_q * Y
	
	diag(cov_mat) = P_s %*% (val_Y - mean_Y)^2
	# cov_mat_pre = norm_W * P_s * Y #( P_q * Y )
	if( identical(covar_fn, 'RDS_WRONG') ){
        cov_mat_pre = norm_W * P_s * Y
	} else {  #Use the RW - random walk model
		cov_mat_pre = P_s * Y
	}
	
	step_diff = abs(outer(1:steps, 1:steps, '-'))
	mat_pow_res = M_tran_probs
	
	for(i in 1:(steps-1)) {
		P_q = diag(1, node_count, node_count) %*% mat_pow_res
		cov_mat[which(step_diff==i)] = sum( cov_mat_pre * P_q )
		if( i != (steps-1) ) {
			mat_pow_res = mat_pow_res %*% M_tran_probs	
		}
	}
	
	# for(i in 1:steps){
		# for(j in i:steps){
			# if(i!=j) {
				# # P_q[i,j] gives probability of reaching j at q-s steps starting at node i 
				# P_q = diag(1, node_count, node_count) %*% mat_pow( M_tran_probs, abs(j-i) )
				# cov_mat[i,j] = sum( cov_mat_pre * P_q)
				# cov_mat[j,i] = cov_mat[i,j]
			# }
		# }
	# }
	
	return( sum(cov_mat)/(steps^2) )
}

var_rw_srs = function(g, steps) {
	#cov_mat = matrix(NA, nrow=steps, ncol=steps)
	#cov_mat = outer(1:steps, 1:steps, FUN=covar_fn, g=g, cv_type=covar_fn )
	
	# Set up a matrix for results:
	node_count = vcount(g)
	cov_mat = matrix(NA, nrow=steps, ncol=steps)
	
	# Precompute some values:
	# W = outer(degree(g), degree(g))^-1
	# sum_W = sum(W)
	# norm_W = sum_W/W
	
	
	val_Y = V(g)$aids   ## This depends on name of attibute
	
	#P_s = as.vector(rep(1/vcount(g), vcount(g)))   # assume node is selected at random (NOT proportional to degree)
	P_s = degree(g)/sum(degree(g))   # assume node is selected at random proportional to degree
	mean_Y = t(P_s) %*% val_Y
	Y = outer(val_Y-mean_Y, val_Y-mean_Y)
	
	M_tran_probs = get.adjacency(g) / degree(g)
	##M_tran_probs = matrix(P_s, vcount(g), vcount(g), byrow=TRUE)
	
	#cov_mat = norm_W * P_s * P_q * Y
	
	diag(cov_mat) = P_s %*% (val_Y - mean_Y)^2
	#cov_mat_pre = norm_W * P_s * Y
	cov_mat_pre = P_s * Y

	
	step_diff = abs(outer(1:steps, 1:steps, '-'))
	mat_pow_res = M_tran_probs
	
	for(i in 1:(steps-1)) {
		P_q = diag(1, node_count, node_count) %*% mat_pow_res
		cov_mat[which(step_diff==i)] = sum( cov_mat_pre * P_q )
		if( i != (steps-1) ) {
			mat_pow_res = mat_pow_res %*% M_tran_probs	
		}
	}
	
	# for(i in 1:steps){
		# for(j in i:steps){
			# if(i!=j) {
				# # P_q[i,j] gives probability of reaching j at q-s steps starting at node i 
				# P_q = diag(1, node_count, node_count) %*% mat_pow( M_tran_probs, abs(j-i) )
				# cov_mat[i,j] = sum( cov_mat_pre * P_q)
				# cov_mat[j,i] = cov_mat[i,j]
			# }
		# }
	# }
	
	return( sum(cov_mat)/(steps^2) )
}

var_rds = function(g, steps, covar_fn='RDS') {
	# covar_fn is 
	#  'RW'  for random walk
	#  'RDS' for RDS weighted
	#cov_mat = matrix(NA, nrow=steps, ncol=steps)
	#cov_mat = outer(1:steps, 1:steps, FUN=covar_fn, g=g, cv_type=covar_fn )
	
	# Set up a matrix for results:
	node_count = vcount(g)
	cov_mat = matrix(NA, nrow=steps, ncol=steps)
	
	# Precompute some values:
	W = degree(g)^-1
	sum_W = sum(W)
	#norm_W = sum_W/W
	norm_W = 1 / sum_W

	
	val_Y = V(g)$aids   ## This depends on name of attibute
	
	P_s = degree(g)/sum(degree(g))   # assume node is selected at random proportional to degree
	mean_Y = mean( norm_W*(val_Y/degree(g)) )
	Y = outer((norm_W*val_Y/degree(g))-mean_Y, (norm_W*val_Y/degree(g))-mean_Y)
	
	M_tran_probs = get.adjacency(g) / degree(g)
	
	
	#cov_mat = norm_W * P_s * P_q * Y
	
	diag(cov_mat) = mean((val_Y - mean_Y)^2)
	# cov_mat_pre = norm_W * P_s * Y #( P_q * Y )
	if( identical(covar_fn, 'RDS') ){
        cov_mat_pre = P_s * Y
	} else {  #Use the RW - random walk model
		cov_mat_pre = P_s * Y
	}
	
	step_diff = abs(outer(1:steps, 1:steps, '-'))
	mat_pow_res = M_tran_probs
	
	for(i in 1:(steps-1)) {
		P_q = diag(1, node_count, node_count) %*% mat_pow_res
		cov_mat[which(step_diff==i)] = sum( cov_mat_pre * P_q )
		if( i != (steps-1) ) {
			mat_pow_res = mat_pow_res %*% M_tran_probs	
		}
	}
	
	# for(i in 1:steps){
		# for(j in i:steps){
			# if(i!=j) {
				# # P_q[i,j] gives probability of reaching j at q-s steps starting at node i 
				# P_q = diag(1, node_count, node_count) %*% mat_pow( M_tran_probs, abs(j-i) )
				# cov_mat[i,j] = sum( cov_mat_pre * P_q)
				# cov_mat[j,i] = cov_mat[i,j]
			# }
		# }
	# }
	
	return( mean(cov_mat) )
}

var_srs = function(g, steps) {
	#cov_mat = matrix(NA, nrow=steps, ncol=steps)
	#cov_mat = outer(1:steps, 1:steps, FUN=covar_fn, g=g, cv_type=covar_fn )
	
	# Set up a matrix for results:
	node_count = vcount(g)
	cov_mat = matrix(NA, nrow=steps, ncol=steps)
	
	# Precompute some values:
	# W = outer(degree(g), degree(g))^-1
	# sum_W = sum(W)
	# norm_W = sum_W/W
	
	
	val_Y = V(g)$aids   ## This depends on name of attibute
	
	P_s = as.vector(rep(1/vcount(g), vcount(g)))   # assume node is selected at random (NOT proportional to degree)
	mean_Y = t(P_s) %*% val_Y
	Y = outer(val_Y-mean_Y, val_Y-mean_Y)
	
	#M_tran_probs = get.adjacency(g) / degree(g)
	M_tran_probs = matrix(P_s, vcount(g), vcount(g), byrow=TRUE)
	
	#cov_mat = norm_W * P_s * P_q * Y
	
	diag(cov_mat) = P_s %*% (val_Y - mean_Y)^2
	#cov_mat_pre = norm_W * P_s * Y
	cov_mat_pre = P_s * Y

	
	step_diff = abs(outer(1:steps, 1:steps, '-'))
	mat_pow_res = M_tran_probs
	
	for(i in 1:(steps-1)) {
		P_q = diag(1, node_count, node_count) %*% mat_pow_res
		cov_mat[which(step_diff==i)] = sum( cov_mat_pre * P_s )
		if( i != (steps-1) ) {
			mat_pow_res = mat_pow_res %*% M_tran_probs	
		}
	}
	
	# for(i in 1:steps){
		# for(j in i:steps){
			# if(i!=j) {
				# # P_q[i,j] gives probability of reaching j at q-s steps starting at node i 
				# P_q = diag(1, node_count, node_count) %*% mat_pow( M_tran_probs, abs(j-i) )
				# cov_mat[i,j] = sum( cov_mat_pre * P_q)
				# cov_mat[j,i] = cov_mat[i,j]
			# }
		# }
	# }
	
	return( sum(cov_mat)/(steps^2) )
}


covar_fn = function(s, q, g, cv_type='covar_fn') {
	# Set up a matrix for results:
	node_count = vcount(g)
	cov_mat = matrix(NA, nrow= node_count, ncol= node_count)
	
	# Precompute some values:
	W = outer(degree(g), degree(g))^-1
	sum_W = sum(W)
	norm_W = sum_W/W
	
	
	val_Y = V(g)$aids   ## This depends on name of attibute
	mean_Y = mean(Y)
	Y = outer(val_Y-mean_Y, val_Y-mean_Y)
	
	P_s = degree(g)/sum(degree(g))   # assume node is selected at random
	M_tran_probs = get.adjacency(g) / degree(g)
	# P_q[i,j] gives probability of reaching j at q-s steps starting at node i 
	P_q = diag(1, node_count, node_count) %*% mat_pow( M_tran_probs, abs(q-s) )
	
	#for(i in 1:node_count){
	#	for(j in 1:node_count){
	#		cov_mat[i,j] = norm_W[i,j] * P_s[i] * P_q[i,j] * Y[i,j]
	#	}
	#}
	
	cov_mat = norm_W * P_s * P_q * Y
	return(sum(cov_mat))
}


generate_node_seq = function( M_trans_probs,  seq_length = 10, initial_node = NA ) {
	ret_seq = rep(0, seq_length)
	rand_list = runif(seq_length)
	if (is.na(initial_node[1])) { ret_seq[1]=sample.int(nrow(M_trans_probs), 1) }
	else{
		if(length(initial_node)>1){ ret_seq[1]=sample(nrow(M_trans_probs), 1, prob=initial_node)    }
		else{ ret_seq[1] = initial_node }
	}
	cum_prob = t(apply(M_trans_probs, 1, cumsum))
	cum_prob[,nrow(cum_prob)] = 1  #Set the last column to 1 to avoid rounding errors
	for (i in 2: seq_length){
		 ret_seq[i] = 1+sum(cum_prob[ret_seq[i-1],] <= rand_list[i] )
	}
	# Could add code to replace the state_index with the state code (as.factor)
	return(ret_seq)
	
}


sim_rds_var = function(ggraph, trials=10, seq_length = 10 ) {
	M_trans_probs = get.adjacency(ggraph)/degree(ggraph)
	results=matrix(nrow=trials,ncol=2)
	for( i in seq(trials) ) {
		sample_nodes = generate_node_seq(M_trans_probs, seq_length, initial_node=degree(ggraph)/sum(degree(ggraph)))
		sample_attr = V(ggraph)$aids[sample_nodes]
		sample_deg = degree(ggraph)[sample_nodes]
		sample_rid = c(NA, head(sample_nodes, -1))
		
		results[i,] = rds.set_groups_estimate(sample_attr, sample_nodes, sample_rid, sample_deg, groups=c('0','1'), est.only=TRUE, minXrec=0)$P.hat
	}
	#return(diag(var(results)))
	return(results)
}

sim_rw_var = function(ggraph, trials=10, seq_length = 10 ) {
	M_trans_probs = get.adjacency(ggraph)/degree(ggraph)
	results=matrix(nrow=trials,ncol=2)
	for( i in seq(trials) ) {
		sample_nodes = generate_node_seq(M_trans_probs, seq_length, initial_node=degree(ggraph)/sum(degree(ggraph)))
		sample_attr = V(ggraph)$aids[sample_nodes]
		#sample_deg = degree(ggraph)[sample_nodes]
		#sample_rid = c(NA, tail(sample_nodes, -1))
		
		results[i,] = prop.table(table(factor(sample_attr, levels=c(0,1))))
	}
	#return(diag(var(results)))
	return(results)
}

sim_srs_var = function(ggraph, trials=10, seq_length = 10 ) {
	#M_trans_probs = get.adjacency(ggraph)/degree(ggraph)
	results=matrix(nrow=trials,ncol=2)
	for( i in seq(trials) ) {
		sample_nodes = sample(V(ggraph), size= seq_length, replace=TRUE)
		sample_attr = V(ggraph)$aids[sample_nodes]
		#sample_deg = degree(ggraph)[sample_nodes]
		#sample_rid = c(NA, tail(sample_nodes, -1))
		
		results[i,] = prop.table(table(factor(sample_attr, levels=c(0,1))))
	}
	return(diag(var(results)))
	#return(results)
}

sim_vh_var = function(ggraph, trials=10, seq_length = 10 ) {
	M_trans_probs = get.adjacency(ggraph)/degree(ggraph)
	results=matrix(nrow=trials,ncol=2)
	for( i in seq(trials) ) {
		sample_nodes = generate_node_seq(M_trans_probs, seq_length, initial_node=degree(ggraph)/sum(degree(ggraph)))
		sample_attr = V(ggraph)$aids[sample_nodes]
		sample_deg = degree(ggraph)[sample_nodes]
		sample_rid = c(NA, head(sample_nodes, -1))
		
		results[i,] = rds.VH.estimate(sample_attr, sample_nodes, sample_rid, sample_deg, groups=c('0','1'))$RDS.VH
	}
	#return(diag(var(results, na.rm=TRUE)))
	return(results)
}









a_edge_list = c( 
1,2,
1,3,
1,5,
2,1,
2,4,
2,6,
3,1,
3,5,
4,2,
4,6,
5,1,
5,3,
5,6,
5,7,
6,2,
6,4,
6,5,
7,5,
7,8,
7,9,
7,12,
8,7,
8,10,
8,11,
9,7,
9,10,
9,11,
10,8,
10,9,
10,12,
11,8,
11,9,
11,12,
12,7,
12,10,
12,11,
12,14,
13,14,
13,15,
13,17,
14,12,
14,13,
14,16,
14,18,
15,13,
15,17,
16,14,
16,18,
17,13,
17,15,
17,18,
18,14,
18,16,
18,17
)

#edge_list_a = matrix(a_edge_list, ncol=2, byrow=TRUE)

attrib_a = rep(0,18)
attrib_a[c(8,9,12:18)] = 1

g_a = graph.empty(directed=FALSE)
g_a = add.vertices( g_a, 18, aids=attrib_a)


g_a = add.edges(g_a, a_edge_list)


g_a = simplify(g_a)





b_edge_list = c( 
1,2,
1,9,
1,10,
2,1,
2,3,
2,11,
3,2,
3,5,
3,12,
4,5,
4,13,
5,3,
5,7,
5,14,
6,7,
6,15,
7,5,
7,6,
7,8,
7,16,
8,7,
8,9,
8,17,
9,1,
9,8,
9,18,
10,1,
10,11,
10,18,
11,2,
11,10,
11,12,
12,3,
12,11,
12,14,
13,4,
13,12,
14,5,
14,12,
14,15,
14,16,
15,6,
15,14,
16,7,
16,17,
17,8,
17,16,
17,18,
18,9,
18,10,
18,17
)

attrib_b = rep(0,18)
attrib_b[10:18] = 1

g_b = graph.empty(directed=FALSE)
g_b = add.vertices( g_b, 18, aids=attrib_b)
g_b = add.edges(g_b, b_edge_list)
g_b = simplify(g_b)




comparison_plot_data = function(ggraph, steps_list=c(10, 20), trials=1000 ){
	compare_list = c("sim_rw", "sim_vh", "sim_rds", "var_srs", "var_rw", "var_vh", "var_rds")
	results = matrix(ncol=length(steps_list), nrow=7, dimnames=list(compare_list, steps_list) )
	for (i in seq(from=length(steps_list)) ){
		results[1:3,i] = sim_all_var(ggraph, trials, steps_list[i] )
		results[4,i] = var_srs(ggraph, steps_list[i]) 
		results[5,i] = var_rw(ggraph, steps_list[i]) 
		results[6,i] = var_rds(ggraph, steps_list[i]) 
		results[7,i] = var_rds(ggraph, steps_list[i]) 
	}
	return(results)
}


sim_all_var = function(ggraph, trials=10, seq_length = 10 ) {
	M_trans_probs = get.adjacency(ggraph)/degree(ggraph)
	compare_list = c("sim_rw", "sim_vh", "sim_rds")
	results=matrix(nrow=trials,ncol=3)
	for( i in seq(trials) ) {
		sample_nodes = generate_node_seq(M_trans_probs, seq_length, initial_node=degree(ggraph)/sum(degree(ggraph)))
		sample_attr = V(ggraph)$aids[sample_nodes]
		sample_deg = degree(ggraph)[sample_nodes]
		sample_rid = c(NA, head(sample_nodes, -1))
		
		results[i,3] = rds.set_groups_estimate(sample_attr, sample_nodes, sample_rid, sample_deg, groups=c('0','1'), est.only=TRUE, minXrec=0)$P.hat[1]
		results[i,2] = rds.VH.estimate(sample_attr, sample_nodes, sample_rid, sample_deg, groups=c('0','1'))$RDS.VH[1]
		results[i,1] = prop.table(table(factor(sample_attr, levels=c(0,1))))[1]
	}
	return(apply(results, 2, var, na.rm=TRUE))
	#return(results)
}

comparison_plot_data = function(ggraph, steps_list=c(10, 20), trials=1000 ){
	compare_list = c("sim_rw", "sim_vh", "sim_rds", "var_srs", "var_rw", "var_vh", "var_rds")
	results = matrix(ncol=length(steps_list), nrow=7, dimnames=list(compare_list, steps_list) )
	for (i in seq(from=length(steps_list)) ){
		results[1:3,i] = sim_all_var(ggraph, trials, steps_list[i] )
		results[4,i] = var_srs(ggraph, steps_list[i]) 
		results[5,i] = var_rw(ggraph, steps_list[i]) 
		results[6,i] = var_rds(ggraph, steps_list[i]) 
		results[7,i] = var_rds(ggraph, steps_list[i]) 
	}
	return(results)
}

plot(x=1, type='n',xlim=c(0,100), ylim=c(0,.15), xlab="Sequence Length", ylab="Variance")
plot_lines = function(plot_data) {	
	x_cords = as.numeric(dimnames(plot_data)[[2]])
	for (i in 1:nrow(plot_data)){
		lines(x_cords, plot_data[i,], col=c("lightblue", "blue", "darkblue", "red", "lightgreen", "green", "darkgreen")[i])
	}
}

plot_cf_lines = function(plot_data_a, plot_data_b) {	
	titles = dimnames(plot_data_a)[[1]]
	x_cords = as.numeric(dimnames(plot_data_b)[[2]])
	for (i in 1:nrow(plot_data_a)){
		plot(x=1, type='n',xlim=c(0,200), ylim=c(0,.25), xlab="Sequence Length", ylab="Variance", main=titles[i])
		lines(x_cords, plot_data_a[i,], col="blue")
		lines(x_cords, plot_data_b[i,], col="lightblue")
	}
}
