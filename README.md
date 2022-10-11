# Using a Bayesian two-species occupancy model and temporal overlap to evaluate how landscape characteristics influence spatial interactions

This is model code from:

Werdel et al. (In Review) Influence of landscape composition on spatiotemporal interactions between sympatric canids. Ecological Applications.

It can be used to estimate the influence of landscape characteristics on spatial interactions between species. Additional temporal overlap code can be used to determine if species activity overlapped significantly more or less than expected.

## ***Two-Species Occupancy Model***

## **Scripts:**
There are 4 scripts needed to run this analysis

**2019-01-15_gallo_et_al_JAE_utility_script.R** - script that loads utility functions. From: Gallo, T., M. Fidino, E.W. Lehrer, and S. Magle. 2019. Urbanization alters predator avoidance behaviors. Journal of Animal Ecology.

**Werdel_JAGS_Model_EcoApps.R** - script that loads data sets and formats data to be fit in JAGS model. This script also calls the JAGS model and summarizes the output.

**Werdel_3sp_Interactions.R** - JAGS model used to estimate the co-occurrence in coyotes and swift fox.

**Werdel_JAGS_Summary.R** – the only file that you need to open. Use "mrf <- readRDS("model_results_Full_SGP.RDS")" to input the associated model results of different estimated covariates "model_results_Full_COVARIATE_EcoApps.RDS". Pre-run summaries can also be found in the repository under "parametersummary_COVARIATE_EcoApps.csv".


## **Data Files:**
There are 8 data files needed to run this analysis

**Werdel_Y_Matrix.txt** - The number of days each species was detected and is supplied as data to the JAGS model so that each species detection probability can be calculated. This array is ordered by species by site by season. Species are in the same order as Werdel_Species_Used.txt and sites are in the same order as Werdel_Sites_Used.txt.

**Werdel_Z_Matrix.txt** - Data on whether or not each species were observed at the camera trapping sites each season. If they were detected the cell takes a value of 1, if they were not detected it takes a 0, and if the site was not sampled it takes an NA. This is a species by site by season array in the same orders as Werdel_Y_Matrix.txt.

**Werdel_J_Matrix.txt** - Data on the number of days a camera trap was active at each site and season. Used with the detection data to calculate detection probabilities, and is a site by season matrix. If a site was not sampled a zero is reported.

**Werdel_Sites_Used.txt** - A vector of the site names used in our analysis. 

**Werdel_Species_Used.txt** - A vector of the observed species whose data are contained in Werdel_Y_Matrix.txt and Werdel_Z_Matrix.txt. Note:Badgers were not used in our results

**Werdel_4_Covs.csv** - Covariates used within our analysis: Proportion of CRP, Shortgrass Prairie, and Rowcrop Agriculture. We had an additional covariate of Total Edge between landcover types.

**WerdelDummyDeerVigilance.csv** - Dummy data to indicate vigilance. Not used with our results.

**WerdelDummyRabbitVigilance.csv** - Dummy data to indicate vigilance. Not used with our results.

**Note:** All of these files must be within your working directory for the analysis to work. Our analysis was done in parallel and used 12 cores. Therefore, you will need to adjust the settings annotated within Werdel_JAGS_Model_EcoApps.R accordingly.

