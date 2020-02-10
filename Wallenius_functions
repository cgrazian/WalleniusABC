library(BiasedUrn)
library(gtools)

#############
### ABC
#############

### How to choose epsilon

ABC.wallenius.omega.eps=function(x.obs,n,S,m)
{
	# x.obs	= matrix of observations
	# m		= vector of the numbers of balls for each color
	# n		= total # of balls
	# S		= ABC simulations
	# eps	= tolerance level
	
	# number of replications (i.e. rows in x.obs)
	L=nrow(x.obs)
	
	c=length(m)		# number of colors
	
	sum.obs=matrix(NA,L,c)
	for(u in 1:L)
	{
		sum.obs[u,]=x.obs[u,]/n[u]
	}
	sum.obs=apply(sum.obs,2,mean)		# summary statistics 
										# on the observed data
	omega.post=matrix(NA,nrow=S,ncol=c)
	dist=c()
	
	for(i in 1:S)
	{
		print(i)
		# Simulate omega from the prior
		#omega.post[i,]=as.vector(rgamma(c,shape=alpha,scale=beta))
		#omega.post[i,]=omega.post[i,]/omega.post[i,c]
		omega.post[i,] <- rdirichlet(1,rep(1,c))
		# Simulate new data from the model
		x.sim=matrix(NA,L,c)
		for (u in 1:L)
		{
			x.sim[u,]=rMWNCHypergeo(nran=1, m=m, n=n[u], odds=omega.post[i,], 
			precision = 1E-7)
		}
		# Compute summary statistics
		sum.sim=matrix(NA,L,c)
		for(u in 1:L)
		{
			sum.sim[u,]=x.sim[u,]/n[u]
		}
		sum.sim=apply(sum.sim,2,mean)
		# Compute the distance (distance in variation 
		#		(page 125, Brémaud))
		dist[i]=0.5 * sum( abs(sum.obs-sum.sim) )
	}
	return(list(omega=omega.post,dist=dist))	
}

ABC.wallenius.omega=function(x.obs,m,n,S,eps,a.prior=1)
{
	# x.obs	= matrix of observations
	# m		= # of balls of each color in the urn
	# n		= vector of total #s of balls
	# S		= ABC simulations
	# eps	= tolerance level
	# a.prior = hyperparameter of the hierarchical prior
		
	L=nrow(x.obs) # sample size
	
	c=length(m)		# number of colors
	
	sum.obs=matrix(NA,L,c)
	for(u in 1:L)
	{
		sum.obs[u,]=x.obs[u,]/n[u]
	}
	sum.obs=apply(sum.obs,2,mean)		# summary statistics 
										# on the observed data
	omega.post=matrix(NA,nrow=S,ncol=c)
	
	count=0
	for(i in 1:S)
	{
	 	print(i)
		while(sum(is.na(omega.post[i,]))!=0)
		{
			count=count+1
			if(S/count < eps/10){
				# Simulate omega from the prior
			
				# xi=rexp(1,1)
				xi=1
				if(xi>0.01){

					#omega.prop=rgamma(c,a.prior*xi,xi)
					#omega.prop <- omega.prop/omega.prop[c]
					omega.prop <- rdirichlet(1,rep(1,c))
				
					# Simulate new data from the model
					x.sim=matrix(NA,L,c)
					for (u in 1:L)
					{
						x.sim[u,]=rMWNCHypergeo(nran=1, m=m, n=n[u], odds=omega.prop, 
						precision = 1E-7)
					}

					# Compute summary statistics
					sum.sim=matrix(NA,L,c)
					for(u in 1:L)
					{
						sum.sim[u,]=x.sim[u,]/n[u]
					}
					sum.sim=apply(sum.sim,2,mean)
		
					# Compute the distance (distance in variation 
					#		(page 125, Brémaud))
					dist=0.5 * sum( abs(sum.obs-sum.sim) )
								
				} else {
					dist=Inf
				}
			
				if(dist<eps) 
				{
					omega.post[i,]=omega.prop
					#print(i)
				} else {
					omega.post[i,]=rep(NA,c)	
				} 	
				
			} else {
				omega.post[i,]=apply(omega.post,2,mean,na.rm=T)
			}
		}	
	}
	return(list(omega=omega.post,count=count))	
}
