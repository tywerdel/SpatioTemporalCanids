## Clear memory and variables in R
rm(list=ls())

## Set working directory
setwd("C:/Users/tywer/OneDrive - Texas A&M AgriLife/GitHubRepository/SpatiotemporalEcoApps")

library(runjags)
library(plotrix)
library(ggplot2)

# model results file # Doesn't need to be run if you you are running mrf from model script
#mrf <- readRDS("model_results_Full_SGP.RDS")

###this does not need to be ran#### Just to look at summary
#msum <- summary(mrf)


# Assuming you have the same covariate for each species.
mcmc <- do.call("rbind", mrf$mcmc)
# drop z stuff
mcmc <- mcmc[,-grep("^z", colnames(mcmc))]
head(mcmc)

summary(mcmc)


params <- colnames(mcmc)

test<- summary(mrf, vars=params)

#average occ, det, col for swift fox
plogis(
  test[row.names(test)=="psi0[2]",1:3]
)




#init occ swift fox
round(plogis(
  test[row.names(test)=="psi0[2]",1:3]
), 3)

#col sf w/o coyote
round(plogis(
  test[row.names(test)=="gmu[2]",1:3]
), 3)

#pers sf w/o coyote
round(plogis(
  test[row.names(test)=="pmu[2]",1:3]
), 3)

#det sf w/o coyote
round(plogis(
  test[row.names(test)=="lp[2]",1:3]
), 3)

#init occ cy
round(plogis(
  test[row.names(test)=="psi0[1]",1:3]
), 3)

#col cy
round(plogis(
  test[row.names(test)=="gmu[1]",1:3]
), 3)

#pers cy
round(plogis(
  test[row.names(test)=="pmu[1]",1:3]
), 3)

#det cy
round(plogis(
  test[row.names(test)=="lp[1]",1:3]
), 3)

#col of sf w/ coyote
temp <- mcmc[,"gmu[2]"]+mcmc[,"a[2,1,1]"]
round(
  plogis(
    quantile(
      temp,
      probs = c(0.025,0.5,0.975)
    )
  ),
  3
)

#pers of sf w/ coyote
temp <- mcmc[,"pmu[2]"]+mcmc[,"a[2,1,2]"]
round(
  plogis(
    quantile(
      temp,
      probs = c(0.025,0.5,0.975)
    )
  ),
  3
)

#det of sf w/ coyote
temp <- mcmc[,"lp[2]"]+mcmc[,"da[2,1]"]
round(
  plogis(
    quantile(
      temp,
      probs = c(0.025,0.5,0.975)
    )
  ),
  3
)

#occ of sf w/ coyote
temp <- mcmc[,"psi0[2]"]+mcmc[,"da[2,1]"]
round(
  plogis(
    quantile(
      temp,
      probs = c(0.025,0.5,0.975)
    )
  ),
  3
)

write.csv(round(test, 2), "parametersummary_Ag_EcoApps.csv")

# the environmental gradient ### This is the range of scaled CRP, if you change covariates, this needs to change
#Determine scaled range for Sif example code
covdat <- read.csv("./Data/Werdel_4_Covs.csv", header = TRUE)
# Remove sites with only one season of data
hab <- as.data.frame(covdat$CRPPrp)
# Calculate principle component of habitat covariates to be an index of urbanization
#pc <- prcomp(hab, scale. = TRUE)
pc <-as.matrix(scale(hab))
pc
range(pc)

##Input covariate range
x <- cbind(1,seq(-0.8208126, 4.5465323, 0.01))

# inverse logit link
ilogit <- function(x) exp(x) / (1 + exp(x))

# create all the model objects
# Initial occupancy
psi0 <- mcmc[,grep("psi0", colnames(mcmc))]
psi_urb <- mcmc[,grep("psi_urb", colnames(mcmc))]
# inxs stuff
a <- mcmc[,grep("^a", colnames(mcmc))]
# make into an array
a <- array(a, dim = c(nrow(mcmc),3,2,2))
# persistance
pmu <- mcmc[,grep("pmu", colnames(mcmc))]
pb <- mcmc[,grep("pb", colnames(mcmc))]
# colonization
gmu <- mcmc[,grep("gmu", colnames(mcmc))]
gb <- mcmc[,grep("gb", colnames(mcmc))]


psinit <- array(
  NA,
  dim = c(nrow(mcmc), nrow(x), 3)
)

pmup <- array(
  NA,
  dim = c(nrow(mcmc), nrow(x), 3)
)

cdp <- array(
  NA,
  dim = c(nrow(mcmc), nrow(x), 2)
)

gmup <- array(
  NA,
  dim = c(nrow(mcmc), nrow(x), 3)
)

cdg <- array(
  NA,
  dim = c(nrow(mcmc), nrow(x), 2)
)


# coyote occupancy
psinit[,,1] <- ilogit(
  cbind(psi0[,1] , psi_urb[,1])  %*%  t(x)
) 
#Extract means and standard error
mean(psinit[,,1])
mean(std.error(psinit[,,1]))

# deer occupancy
psinit[,,2] <- ilogit(
  cbind(psi0[,2] , psi_urb[,2])  %*%  t(x)
) 
#Extract means and standard error
mean(psinit[,,2])
mean(std.error(psinit[,,2]))

# rabbit occupancy
psinit[,,3] <- ilogit(
  cbind(psi0[,3] , psi_urb[,3])  %*%  t(x)
) 
# coyote persistence
pmup[,,1] <- ilogit(
  cbind(pmu[,1], pb[,1]) %*% t(x)
)
mean(pmup[,,1])
mean(std.error(pmup[,,1]))
# deer persistence w/o coyote
pmup[,,2] <- ilogit(
  cbind(pmu[,2], pb[,2]) %*% t(x)
)
#Extract means and standard error
mean(pmup[,,2])
mean(std.error(pmup[,,2]))

# rabbit persistence w/o coyote
pmup[,,3] <- ilogit(
  cbind(pmu[,3], pb[,3]) %*% t(x)
)
# deer persistence w/ coyote
cdp[,,1] <- ilogit(
  a[,2,1:2,2] %*% t(x)
) 
#Extract means and standard error
mean(cdp[,,1])
mean(std.error(cdp[,,1]))

# rabbit persistence w/ coyote
cdp[,,2] <- ilogit(
  a[,3,1:2,2] %*% t(x)
) 
# coyote colonization
gmup[,,1] <- ilogit(
  cbind(gmu[,1], gb[,1]) %*% t(x)
)
#Extract means and standard error
mean(gmup[,,1])
mean(std.error(gmup[,,1]))

# deer colonization w/o coyote
gmup[,,2] <- ilogit(
  cbind(gmu[,2], gb[,2]) %*% t(x)
)
#Extract means and standard error
mean(gmup[,,2])
mean(std.error(gmup[,,2]))

# rabbit colonization w/o coyote
gmup[,,3] <- ilogit(
  cbind(gmu[,3], gb[,3]) %*% t(x)
)
# deer colonization w/ coyote
cdg[,,1] <- ilogit(
  a[,2,1:2,1] %*% t(x)
) 
#Extract means and standard error
mean(cdg[,,1])
mean(std.error(cdg[,,1]))
#t.test(pmup[,,2],cdp[,,1])

# rabbit colonization w/ coyote
cdg[,,2] <- ilogit(
  a[,3,1:2,1] %*% t(x)
) 
# number of mcmc steps by number of predicted
# values on gradient by number of years
PSIA <- PSIBa <- PSICa <- PSIBA <- PSICA <- 
  PSIB <- PSIC <- PSIAB <- PSIAC <- SIF_deer <- SIF_rab <- array(
    NA,
    dim = c(nrow(mcmc), nrow(x), 3)
  )

# calculating all of the components of the SIF
PSIA[,,1] <-  psinit[,,1] * (pmup[,,1]) + (1 - psinit[,,1]) * gmup[,,1]
PSIBa[,,1] <- psinit[,,2] * (pmup[,,2]) + (1 - psinit[,,2]) * gmup[,,2]
PSICa[,,1] <- psinit[,,3] * (pmup[,,3]) + (1 - psinit[,,3]) * gmup[,,3]
PSIBA[,,1] <- psinit[,,2] * (cdp[,,1]) + (1 - psinit[,,2]) * (cdg[,,1])
PSICA[,,1] <- psinit[,,3] * (cdp[,,2]) + (1 - psinit[,,2]) * (cdg[,,2])
PSIB[,,1] <-  PSIA[,,1] * PSIBA[,,1] + (1 - PSIA[,,1]) * PSIBa[,,1]
PSIC[,,1] <-  PSIA[,,1] * PSICA[,,1] + (1 - PSIA[,,1]) * PSICa[,,1]
PSIAB[,,1] <- PSIA[,,1] * PSIBA[,,1]
PSIAC[,,1] <- PSIA[,,1] * PSICA[,,1]
SIF_deer[,,1] <- PSIAB[,,1]/(PSIA[,,1]*PSIB[,,1])
SIF_rab[,,1] <- PSIAC[,,1]/(PSIA[,,1]*PSIC[,,1])
for(t in 2:3){
  PSIA[,,t] <- PSIA[,,t-1] * (pmup[,,1]) + ((1 - PSIA[,,t-1]) * gmup[,,1])
  PSIBa[,,t] <- PSIBa[,,t-1] * (pmup[,,2]) + ((1 - PSIBa[,,t-1]) * gmup[,,2])
  PSICa[,,t] <- PSICa[,,t-1] * (pmup[,,3]) + ((1 - PSICa[,,t-1]) * gmup[,,3])
  PSIBA[,,t] <- PSIBA[,,t-1] * (cdp[,,1]) + ((1 - PSIBA[,,t-1]) * (cdg[,,1]))
  PSICA[,,t] <- PSICA[,,t-1] * (cdp[,,2]) + ((1 - PSICA[,,t-1]) * (cdg[,,2]))
  PSIB[,,t] <- PSIA[,,t] * PSIBA[,,t] + (1 - PSIA[,,t]) * PSIBa[,,t]
  PSIC[,,t] <- PSIA[,,t] * PSICA[,,t] + (1 - PSIA[,,t]) * PSICa[,,t]
  PSIAB[,,t] <- PSIA[,,t] * PSIBA[,,t]
  PSIAC[,,t] <- PSIA[,,t] * PSICA[,,t]
  SIF_deer[,,t] <- PSIAB[,,t]/(PSIA[,,t]*PSIB[,,t])
  SIF_rab[,,t] <- PSIAC[,,t]/(PSIA[,,t]*PSIC[,,t])
}

mean(PSIAB)
mean(std.error(PSIAB))

# get quantiles
# dimensions,
# 1. 0.025,0.5,0.975 quantiles
# 2. the gradient
# 3. the year
sif_deer_quants <- apply(
  SIF_deer,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

sif_rab_quants <- apply(
  SIF_rab,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

## FOR Occupancy
PSIA_quants <- apply(
  PSIA,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

PSIAB_quants <- apply(
  PSIAB,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

PSIB_quants <- apply(
  PSIB,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

PSIBA_quants <- apply(
  PSIBA,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)

PSIBa_quants <- apply(
  PSIBa,
  c(2,3),
  quantile,
  probs = c(0.025,0.5,0.975)
)


# plot out results

#par(mfrow = c(4, 3))
x <- x*attr(pc,"scaled:scale")+attr(pc,"scaled:center")
#x

###Plot SIF Average
SIF3<-rowMeans(sif_deer_quants[2,,])
Lline<-rowMeans(sif_deer_quants[1,,])
Uline<-rowMeans(sif_deer_quants[3,,])
Lline
Uline
SIF3All <- cbind(Lline, SIF3, Uline, x)



plot(SIF3 ~ x[,2], type = "l",
     ylim = range(sif_deer_quants), main= "Swift Fox/Coyote Interaction",
     xlab = "Proportion CRP", ylab = "SIF")
lines(Lline ~ x[,2], lty = 2)
lines(Uline ~ x[,2], lty = 2)

#for ggplot
SIF3All <- as.data.frame(SIF3All)

#define font for plot
black.bold.text <- element_text(face = "bold", color = "black", size="20")

#plot predictions
k <- ggplot(SIF3All, aes(x[,2], SIF3))+geom_line(size=2)
k
k2 <- k+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      geom_ribbon(aes(ymin=Lline, ymax=Uline), linetype=2, alpha=0.1) 
k2
#make graph nice
k2 = k + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x=element_text(family="sans", face="bold", colour="black", size="15", margin=margin(5,40,10,0))) + 
  theme(axis.text.y=element_text(family="sans", face="bold", colour="black", size="15", margin=margin(10,5,0,10))) + 
  theme(axis.title = black.bold.text) + coord_cartesian(ylim = c(0, 1.75), expand = FALSE) + theme(plot.margin = margin(30,17,15,15))+
  geom_ribbon(aes(ymin=Lline, ymax=Uline), linetype=2, alpha=0.1)+xlab("Proportion CRP")+ylab("SIF") + 
  scale_x_continuous(breaks=c(.1,.2,.3,.4,.5,.6))+geom_hline(yintercept =1,linetype= 2, size=1)
#plot graph
#k
k2



###Plot SIF by Year
mean(sif_deer_quants[2,,])
mean(sif_deer_quants[1,,])
mean(sif_deer_quants[3,,])

plot(sif_deer_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(sif_deer_quants), main= "year 1",
     xlab = "Proportion CRP", ylab = "SIF")
lines(sif_deer_quants[1,,1] ~ x[,2], lty = 2)
lines(sif_deer_quants[3,,1] ~ x[,2], lty = 2)

plot(sif_deer_quants[2,,2] ~ x[,2], type = "l",
     ylim = range(sif_deer_quants), main= "year 2",
     xlab = "Proportion CRP", ylab = "SIF")
lines(sif_deer_quants[1,,2] ~ x[,2], lty = 2)
lines(sif_deer_quants[3,,2] ~ x[,2], lty = 2)

plot(sif_deer_quants[2,,3] ~ x[,2], type = "l",
     ylim = range(sif_deer_quants), main= "year 3",
     xlab = "Proportion CRP", ylab = "SIF")
lines(sif_deer_quants[1,,3] ~ x[,2], lty = 2)
lines(sif_deer_quants[3,,3] ~ x[,2], lty = 2)


#Plot PSI
###For average across all years
PSIB3<-rowMeans(PSIB_quants[2,,])
Lline<-rowMeans(PSIB_quants[1,,])
Uline<-rowMeans(PSIB_quants[3,,])
PSIAB3<-rowMeans(PSIAB_quants[2,,])
Lline2<-rowMeans(PSIAB_quants[1,,])
Uline2<-rowMeans(PSIAB_quants[3,,])

plot(PSIB3 ~ x[,2], type = "l",
     ylim = range(PSIAB_quants,PSIB_quants), main= "3-Year Mean",
     xlab = "Proportion CRP", ylab = "PSIB")
lines(Lline ~ x[,2], lty = 2)
lines(Uline ~ x[,2], lty = 2)

lines(PSIAB3 ~ x[,2], lty = 1)
lines(Lline2 ~ x[,2], lty = 2)
lines(Uline2 ~ x[,2], lty = 2)






#SF persistence w/o coyote
plot(pmup[,,2],cdp[,,1])
#SF persistence w coyote
cdp[,,1]

plot(PSIA_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(PSIA_quants), main= "PSIA",
     xlab = "Proportion CRP", ylab = "PSIA")
lines(PSIA_quants[1,,1] ~ x[,2], lty = 2)
lines(PSIA_quants[3,,1] ~ x[,2], lty = 2)

plot(PSIAB_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(PSIAB_quants), main= "PSIAB",
     xlab = "Proportion CRP", ylab = "PSIAB")
lines(PSIAB_quants[1,,1] ~ x[,2], lty = 2)
lines(PSIAB_quants[3,,1] ~ x[,2], lty = 2)

plot(PSIB_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(PSIB_quants), main= "PSIB",
     xlab = "Proportion CRP", ylab = "PSIB")
lines(PSIB_quants[1,,1] ~ x[,2], lty = 2)
lines(PSIB_quants[3,,1] ~ x[,2], lty = 2)

plot(PSIBA_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(PSIBA_quants), main= "PSIBA",
     xlab = "Proportion CRP", ylab = "PSIBA")
lines(PSIBA_quants[1,,1] ~ x[,2], lty = 2)
lines(PSIBA_quants[3,,1] ~ x[,2], lty = 2)

plot(PSIBa_quants[2,,1] ~ x[,2], type = "l",
     ylim = range(PSIBa_quants), main= "PSIBa",
     xlab = "Proportion CRP", ylab = "PSIBa")
lines(PSIBa_quants[1,,1] ~ x[,2], lty = 2)
lines(PSIBa_quants[3,,1] ~ x[,2], lty = 2)

