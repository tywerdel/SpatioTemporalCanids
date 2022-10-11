#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## FITTING 2-SPECIES DYNAMIC OCCUPANCY MODEL AND OVERLAP ANALYSIS ##
## SCRIPT ASSOCIATED WITH WERDEL ET AL 2022 ECOLOGICAL APPLICATIONS##
## INFLUENCE OF LANDSCAPE COMP ON SPATIOTEMPORAL INTERACTIONS B/W SYMPATRIC CANIDS ##
## BY TY WERDEL ##
## KANSAS STATE UNIVERSITY AND TEXAS A&M UNIVERSITY##
## Last Updated 2022-08-30 ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Load utility created by Gallo et al. 2019 Animal Ecology
source("2019-01-15_gallo_et_al_JAE_utility_script.R")
# Load needed packages and modules
package_load(c("reshape2", "runjags", "rjags", "parallel", "data.table","scales",
               "overlap","stringr","dplyr", "MCMCpack","denstrip"))
load.module('glm')

## Read in data
# Read in the species names (we will not be using Badger Data)
species_names <- read.table("./Data/Werdel_Species_Used.txt", header = TRUE)

# Read in the site names
site_names <- read.table("./Data/Werdel_Sites_Used.txt", header = TRUE, colClasses = "character")
# We do not use data from this site
site_names <- site_names[,] 

## Set up data for three species co-occurence model and vigilance binomial model
# Function that sets up data for JAGs run
vig_JAGS_setup <- function(index, model) {
  
  # Read in z-matrix for the two species
  z <- df_2_array(read.table("./Data/Werdel_Z_Matrix.txt", 
                             header = TRUE, sep = "\t"))[index,,] 
  
  # Build y-array and j-matrix
  y_array <- df_2_array(read.table("./Data/Werdel_Y_Matrix.txt", 
                                   header = TRUE, sep = "\t"))[index,,] 
  j_mat <- read.table("./Data/Werdel_J_Matrix.txt", 
                      header = TRUE, sep = "\t")[,]
  
  # Read in vigilance data for deer
  vig_deer <- df_2_array(read.csv("./Data/WerdelDummyDeerVigilance.csv", header = TRUE))
  # Heads up photos
  hups_deer <- vig_deer[1,,]
  # Total photos
  phot_deer <- vig_deer[2,,]
  #  phot_deer cannot have NA values as it is not a random variable
  #  supplied to JAGS. These values do not actually get supplied to the
  #  model as we skip over them in the analysis.
  phot_deer[is.na(phot_deer)] <- 0.001
  
  # Read in vigilance data for rabbit
  vig_rab <- df_2_array(read.csv("./Data/WerdelDummyRabbitVigilance.csv", header = TRUE))
  # Head up photos
  hups_rab <- vig_rab[1,,]
  # Total photos
  phot_rab <- vig_rab[2,,]
  #  phot_rab cannot have NA values as it is not a random variable
  #  supplied to JAGS. These values do not actually get supplied to the
  #  model as we skip over them in the analysis.
  phot_rab[is.na(phot_rab)] <- 0.001
  
  # Removing any site with only 1 season of data
  #tg <- which(rowSums(is.na(z[1,,]))>1)
  # Remove these sites from all data sets
  z <- z[,,]
  y <- y_array[,,]
  j <- j_mat[,]
  hups_deer <- hups_deer[,]
  phot_deer <- phot_deer[,]
  hups_rab <- hups_rab[,]
  phot_rab <- phot_rab[,]
  
  # Convert head up data to long form
  
  #  hups_loc_deer indexes where we actually have data to put into the
  #  vigilance model and is used to skip over where we have placed 0.001 values
  #  in phot_deer.
  hups_loc_deer <- which(!is.na(hups_deer)==TRUE, arr.ind=TRUE)
  hups_long_deer <- rep(0,nrow(hups_loc_deer))
  for(i in 1:length(hups_long_deer)){
    hups_long_deer[i] <- hups_deer[hups_loc_deer[i,1],hups_loc_deer[i,2]]
  }
  # Same as above but for rabbit
  hups_loc_rab <- which(!is.na(hups_rab)==TRUE, arr.ind=TRUE)
  hups_long_rab <- rep(0,nrow(hups_loc_rab))
  for(i in 1:length(hups_long_rab)){
    hups_long_rab[i] <- hups_rab[hups_loc_rab[i,1],hups_loc_rab[i,2]]
  }
  
  
  ##############
  # Read in site-level habitat covariates
  covdat <- read.csv("./Data/Werdel_4_Covs.csv", header = TRUE)
  # Remove sites with only one season of data
  ############
  ############ Here is where we determine the landscape covariate to run model
  ###(CRPPrp, SGPPrp, RowcropPrp, TotalEdge) ##
  hab <- as.data.frame(covdat$CRPPrp)
  pc <-as.matrix(scale(hab))
  pc
  
  # Set up data for JAGS
  data_list <- list(
    y = y, # detection / non-detection data
    nyear = dim(z)[3], # number of seasons sampled
    nsite = dim(z)[2], # number of sites sampled
    nspec = dim(z)[1], # number of species
    J = as.matrix(j),  # number of days sampled per site and season
    pcov = pc[,1],   # urb covariate for persistance
    lpcov = pc[,1],  # urb covariate for detection
    vcov = pc[,1],   # urb covariate for vigilance
    inxscov = as.matrix(data.frame(a = 1, b = pc[,1])), # design matrix for inxs
    ncov_phi = 1, # number of slope terms for persistence
    phot_deer = phot_deer, # number of deer photos
    phot_rab = phot_rab,   # number of rabbit photos
    hup_deer = hups_long_deer, # number of head up deer photos
    hup_rab = hups_long_rab, # number of head up rabbit photos
    hups_loc_deer=as.matrix(hups_loc_deer), # connects long format data to site
    n_event_deer=nrow(hups_loc_deer), # used to loop through vigilance analysis
    hups_loc_rab=as.matrix(hups_loc_rab), # connects long format data to site
    n_event_rab=nrow(hups_loc_rab) # used to loop through vigilance analysis
  )
  
  # Set up initial values
  inits <- function(chain){
    gen_list <- function(chain = chain){
      list( 
        z = z,
        psi0 = rnorm(3, 0, 1),
        psi_urb = rnorm(3, 0, 1),
        pmu = rnorm(3, 0, 1),
        lp = rnorm(3, 0, 1),
        gmu = rnorm(3, 0, 1),
        ppar = rnorm(4),
        gpar = rnorm(4),
        dpar = rnorm(4),
        .RNG.name = switch(chain,
                           "1" = "base::Wichmann-Hill",
                           "2" = "base::Marsaglia-Multicarry",
                           "3" = "base::Super-Duper",
                           "4" = "base::Mersenne-Twister",
                           "5" = "base::Wichmann-Hill",
                           "6" = "base::Marsaglia-Multicarry",
                           "7" = "base::Super-Duper",
                           "8" = "base::Mersenne-Twister",
                           "9" = "base::Wichmann-Hill",
                           "10" = "base::Marsaglia-Multicarry",
                           "11" = "base::Super-Duper",
                           "12" = "base::Mersenne-Twister"),
        .RNG.seed = sample(1:1e+06, 1)
      )
    }
    return(switch(chain,           
                  "1" = gen_list(chain),
                  "2" = gen_list(chain),
                  "3" = gen_list(chain),
                  "4" = gen_list(chain),
                  "5" = gen_list(chain),
                  "6" = gen_list(chain),
                  "7" = gen_list(chain),
                  "8" = gen_list(chain),
                  "9" = gen_list(chain),
                  "10" = gen_list(chain),
                  "11" = gen_list(chain),
                  "12" = gen_list(chain)
    )
    )
  }
  
  # Parameters to monitor
  tmon <- c("v0", "vig_urb", "vcoy0", "vcoy_urb", "SIF_deer", "SIF_deer_high", 
            "SIF_deer_low","SIF_rab",  "SIF_rab_high", "SIF_rab_low", "a", "da", "psi0",
            "psi_urb", "pmu", "lp", "gmu", "gyr", "tau_yr", "gb", "pb", "lpb", "z")
  
  # Run model
  mout <- run.jags( model = model, 
                    monitor = tmon , 
                    data = data_list ,  
                    inits = inits , 
                    n.chains = 12 , ## For less CPU cores use 2
                    adapt = 10000,  
                    burnin = 10000, 
                    sample = 8334, ## For less CPU cores use 50000
                    thin = 2,
                    summarise = FALSE ,
                    plots = FALSE,
                    method = "parallel")
  
  # Summarize model
  mod_sum <- add.summary(mout)
  
  # MCMC chains of all parameters that we tracked
  mmat <- as.matrix(as.mcmc.list(mout), chains = TRUE)
  
  return(list(mmat=mmat, model_results=mout, mod_sum=mod_sum))
  
}

# Data index for species combos
three <- c(1,2,3)

# Run 3 species model
mod_3sp <- vig_JAGS_setup(three, "Werdel_3sp_Interactions.R")

###Run for the Sif Example code.
saveRDS(mod_3sp$model_results,"model_results_Full_CRP_EcoApps.RDS")

##If you are doing this all in one shot and just switching right over to SIF example code###
mrf <- mod_3sp$model_results

