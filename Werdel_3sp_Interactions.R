model{
####################################
### PRIORS #########################
####################################
# Interaction parameters
for(par in 1:4){
	ppar[par] ~ dt(0, 2.5, 1)
	gpar[par] ~ dt(0, 2.5, 1)
	dpar[par] ~ dt(0, 2.5, 1)
}
##
# inxs persistence
## species x parameter x process array
a[1,1,2] <- 0 # nothing for coyote
a[1,2,2] <- 0 # nothing for coyote
a[2,1,2] <- ppar[1] # intercept swift fox
a[2,2,2] <- ppar[2] # cov swift fox, from mvn
a[3,1,2] <- ppar[3] # intercept badger
a[3,2,2] <- ppar[4] # cov badger, from mvn
##
# persistence covariates
##
pb[1] ~ dt(0,2.5,1) # coyote
pb[2] ~ dt(0,2.5,1) # swift fox, from mvn
pb[3] ~ dt(0,2.5,1) # badger, from mvn
#
##
# inxs colonization
##
a[1,1,1] <- 0 # nothing for coyote
a[1,2,1] <- 0 # nothing for coyote
a[2,1,1] <- gpar[1] # swift fox intercept
a[2,2,1] <- gpar[2] # swift fox cov, from mvn
a[3,1,1] <- gpar[3] # badger intercept
a[3,2,1] <- gpar[4] # badger cov, from mvn
##
# colonization covariates
##
  gb[1] ~ dt(0, 2.5, 1) # coyote
  gb[2] ~ dt(0, 2.5, 1) # swift fox, from mvn
  gb[3] ~ dt(0, 2.5, 1) # badger, from mvn
# detection
##
# cov coyote on swift fox prior detection state
##
da[1,1] <- 0 # nothing for coyote
da[1,2] <- 0 # nothing for coyote
da[2,1] <- dpar[1] # swift fox intercept
da[2,2] <- dpar[2] # swift fox cov, from mvn
da[3,1] <- dpar[3] # badger intercept
da[3,2] <- dpar[4] # badger cov, from mvn
##
# covariates on detection
##
lpb[1] ~ dt(0,2.5,1) # coyote
lpb[2] ~ dt(0, 2.5, 1) # swift fox from mvn
lpb[3] ~ dt(0, 2.5, 1) # badger from mvn
# other parameter for occupancy linear predictor
for(i in 1:3){
##
# priors for intercepts
##
psi0[i] ~ dt(0, 2.5, 1) # initial occupancy
psi_urb[i] ~ dt(0, 2.5, 1) # cov on initial occupancy
pmu[i] ~ dt(0, 2.5,1)               # persistence intercept
lp[i] ~ dt(0, 2.5, 1)               # detection intercept
gmu[i] ~ dnorm(0, 0.3)              # colonization mu 
}
####################################
### LATENT STATE ###################
####################################
	for(k in 1:nsite){
		for(i in 1:nspec){
	##
	# Initial occupancy
	##
	logit(psi[i,k,1]) <- psi0[i] + psi_urb[i]*pcov[k]
	z[i,k,1] ~ dbern(psi[i,k,1])
		}
	for(t in 2:nyear){
	##
	# Mixture of colonization and persistence
	##
		# Linear predictor for coyote
	logit(psi[1,k,t]) <- (z[1,k,t-1] * (pmu[1] + pb[1] * pcov[k])) +
		(1 - z[1,k,t-1]) * (gmu[1] + gb[1] * pcov[k])
		# Linear predictor for swift fox
	logit(psi[2,k,t]) <- 
	 (z[2,k,t-1]*(1-z[1,k,t-1])*(pmu[2] + pb[2] * pcov[k])) + # phi w/o coyote
	 (z[2,k,t-1]*z[1,k,t-1]*(inprod(a[2,,2], inxscov[k,]))) + #phi w/ coyote
	 ((1 - z[2,k,t-1])*(1-z[1,k,t-1])*(gmu[2] + gb[2] * pcov[k])) + #gam w/o coyote
	 ((1 - z[2,k,t-1])*z[1,k,t-1]*(inprod(a[2,,1],inxscov[k,]))) # gam w/ coy
	# Linear predictor for badger
	logit(psi[3,k,t]) <- 
		(z[3,k,t-1]*(1-z[1,k,t-1])*(pmu[3] + pb[3] * pcov[k])) + # phi w/o coyote
		(z[3,k,t-1]*z[1,k,t-1]*(inprod(a[3,,2], inxscov[k,]))) + #phi w/ coyote
		((1 - z[3,k,t-1])*(1-z[1,k,t-1])*(gmu[3] + gb[3] * pcov[k])) + #gam w/o coyote
		((1 - z[3,k,t-1])*z[1,k,t-1]*(inprod(a[3,,1],inxscov[k,]))) # gam w/ coy
	##
	# Likelihood
	##
	for(i in 1:nspec){
	z[i,k,t] ~ dbern(psi[i,k,t])
		}
	}
}
####################################
### OBSERVATIONAL PROCESS ##########
####################################

	for(k in 1:nsite){
		for(t in 1:nyear){
		  logit(dprob[1,k,t]) <- lp[1] +  lpb[1]*lpcov[k]
		  logit(dprob[2,k,t]) <- ((lp[2] + lpb[2]*lpcov[k]) *  (1 - z[1,k,t])) + 
		  	                         (inprod(da[2,], inxscov[k,]) * z[1,k,t])
		  logit(dprob[3,k,t]) <- ((lp[3] + lpb[3]*lpcov[k]) *  (1 - z[1,k,t])) + 
		  	                         (inprod(da[3,], inxscov[k,]) * z[1,k,t])
		  for(i in 1:nspec){
		mu[i,k,t] <- z[i,k,t] * dprob[i,k,t]
		y[i,k,t] ~ dbin(mu[i,k,t], J[k,t])
		}
	}
}
}
