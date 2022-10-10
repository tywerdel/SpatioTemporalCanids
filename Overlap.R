## Clear memory and variables in R
rm(list=ls())

## Set working directory
setwd("C:/Users/tywer/OneDrive - Kansas State University/SwiftFox/Overlap")


## load overlap package
library(overlap)
library(activity)
library(ggplot2)
library(plotrix) 
library(vioplot)
library(forcats)
## Import data
active = read.csv("C:/Users/tywer/OneDrive - Kansas State University/SwiftFox/Overlap/CoyFoxActivity.csv")
coyactive = read.csv("C:/Users/tywer/OneDrive - Kansas State University/SwiftFox/Overlap/CoyoteActivity.csv")
sfactive = read.csv("C:/Users/tywer/OneDrive - Kansas State University/SwiftFox/Overlap/SFActivity.csv")

head(active)
summary(active$Species)
table(active$Species)
range(active$Time)

#convert to hours
timeRad <-active$Time * 2 * pi
#timeRad

#fit kernel density to all coyotes

cactAll <- timeRad[active$Species == 'Coyote']
densityPlot(cactAll, rug = TRUE)

#Repeat for swift fox
sfactALL <- timeRad[active$Species == 'Swift Fox']
densityPlot(sfactALL, rug = TRUE)

#set min lengths
min(length(cactAll), length(sfactALL))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestALL <- overlapEst(cactAll, sfactALL, type="Dhat4")
coysfestALL

overlapPlot(cactAll, sfactALL, main="All Activity", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

#bootstrapping
coyfoxbootAll <- bootstrap(cactAll, sfactALL, 1000, type="Dhat4")

boxplot(coyfoxbootAll)
BSmean <- mean(coyfoxbootAll)
BSmean
#BSmin <- min(coyfoxboot)
#BSmax <- max(coyfoxboot)



# If you want to create your own confidence intervals
BSmean <- mean(coyfoxbootAll)
CIcoyfox<-bootCI(coysfestALL, coyfoxbootAll)
BSmean
CIcoyfox
UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

All <- cbind(BSmean,UCI,LCI)
All
UCI
LCI
BSmean


plotCI(x = BSmean,               # plotrix plot with confidence intervals,
       li = LCI,
       ui = UCI)

ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

#bp <- boxplot(BSmean, plot = FALSE)

#bp$stats <- matrix(c(
#  BSmin,
#  UCI,
#  BSmean,
#  LCI,
#BSmax
#), nrow = 5, byrow = TRUE)

#bxp(bp)

#coefficient of overlap backtransforming
#CIlogitcoyfox<-bootCIlogit(coysfest, coyfoxboot)



#fit kernel density where both present

cactBoth <- timeRad[active$Species == 'Coyote' & active$Both == "1"]
densityPlot(cactBoth, rug = TRUE)

#Repeat for swift fox
sfactBoth <- timeRad[active$Species == 'Swift Fox'& active$Both == '1']
densityPlot(sfactBoth, rug = TRUE)

#set min lengths
min(length(cactBoth), length(sfactBoth))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestBoth <- overlapEst(cactBoth, sfactBoth, type="Dhat4")
coysfestBoth

overlapPlot(cactBoth, sfactBoth, main="Both Present", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

coyfoxbootBoth <- bootstrap(cactBoth, sfactBoth, 1000, type="Dhat4")

#boxplot(coyfoxbootAll, coyfoxbootBoth)
BSmean <- mean(coyfoxbootBoth)
CIcoyfox<-bootCI(coysfestBoth, coyfoxbootBoth)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

Both <- cbind(BSmean,UCI,LCI)
Both



plotCI(x = BSmean,               # plotrix plot with confidence intervals,
       li = LCI,
       ui = UCI)

#fit kernel density where only 1 present

cactOne <- timeRad[active$Species == 'Coyote' & active$OnlyCoy == "1"]
densityPlot(cactOne, rug = TRUE)

#Repeat for swift fox
sfactOne <- timeRad[active$Species == 'Swift Fox'& active$OnlyFox == '1']
densityPlot(sfactOne, rug = TRUE)

#set min lengths
min(length(cactOne), length(sfactOne))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestOne <- overlapEst(cactOne, sfactOne, type="Dhat1")
coysfestOne

overlapPlot(cactOne, sfactOne, main="1 Present", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

coyfoxbootOne <- bootstrap(cactOne, sfactOne, 1000, type="Dhat4")

BSmean <- mean(coyfoxbootOne)
CIcoyfox<-bootCI(coysfestOne, coyfoxbootOne)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

One <- cbind(BSmean,UCI,LCI)
One
str(cactOne)
str(sfactOne)
#boxplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootOne)


#fit kernel density where coyote is present and not present

cactEach <- timeRad[active$Species == 'Swift Fox' & active$OnlyFox == "1"]
densityPlot(cactEach, rug = TRUE)

#Repeat for swift fox
sfactEach <- timeRad[active$Species == 'Swift Fox'& active$Both == '1']
densityPlot(sfactEach, rug = TRUE)

#set min lengths
min(length(cactEach), length(sfactEach))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestEach <- overlapEst(cactEach, sfactEach, type="Dhat4")
coysfestEach

overlapPlot(cactEach, sfactEach, main="Swift Fox", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote Absent", "Coyote Present"), lty=c(1,2), col=c(1,4), bty='n')

coyfoxbootEach <- bootstrap(cactEach, sfactEach, 1000, type="Dhat4")

BSmean <- mean(coyfoxbootEach)
CIcoyfox<-bootCI(coysfestEach, coyfoxbootEach)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

Each <- cbind(BSmean,UCI,LCI)
Each



#boxplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootOne, coyfoxbootEach)


#fit kernel density where both present with high CRP

cactHCRP <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$CRPPrp>=.1]
densityPlot(cactHCRP, rug = TRUE)

#Repeat for swift fox
sfactHCRP <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$CRPPrp>=.1]
densityPlot(sfactHCRP, rug = TRUE)

#set min lengths
min(length(cactHCRP), length(sfactHCRP))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestHCRP <- overlapEst(cactHCRP, sfactHCRP, type="Dhat4")
coysfestHCRP

overlapPlot(cactHCRP, sfactHCRP, main="Both Present - High CRP (>10%)", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')


coyfoxbootHCRP <- bootstrap(cactHCRP, sfactHCRP, 10000, type="Dhat1")

BSmean <- mean(coyfoxbootHCRP)
CIcoyfox<-bootCI(coysfestHCRP, coyfoxbootHCRP)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

HCRP <- cbind(BSmean,UCI,LCI)
HCRP

#boxplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootOne, coyfoxbootEach, coyfoxbootHCRP)

#fit kernel density where both present with low CRP

cactLCRP <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$CRPPrp<.1]
densityPlot(cactLCRP, rug = TRUE)

#Repeat for swift fox
sfactLCRP <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$CRPPrp<.1]
densityPlot(sfactLCRP, rug = TRUE)

#set min lengths
min(length(cactLCRP), length(sfactLCRP))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestLCRP <- overlapEst(cactLCRP, sfactLCRP, type="Dhat4")
coysfestLCRP

overlapPlot(cactLCRP, sfactLCRP, main="Both Present - Low CRP (<10%)", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

coyfoxbootLCRP <- bootstrap(cactLCRP, sfactLCRP, 10000, type="Dhat4")

BSmean <- mean(coyfoxbootLCRP)
CIcoyfox<-bootCI(coysfestLCRP, coyfoxbootLCRP)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

LCRP <- cbind(BSmean,UCI,LCI)
LCRP

#fit kernel density where both present with high CRP

cactHCRP <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$CRPPrp>=.1]
densityPlot(cactHCRP, rug = TRUE)

#Repeat for swift fox
sfactHCRP <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$CRPPrp>=.1]
densityPlot(sfactHCRP, rug = TRUE)

#set min lengths
min(length(cactHCRP), length(sfactHCRP))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestHCRP <- overlapEst(cactHCRP, sfactHCRP, type="Dhat4")
coysfestHCRP

overlapPlot(cactHCRP, sfactHCRP, main="Both Present - High CRP (>10%)", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')


coyfoxbootHCRP <- bootstrap(cactHCRP, sfactHCRP, 10000, type="Dhat1")

BSmean <- mean(coyfoxbootHCRP)
CIcoyfox<-bootCI(coysfestHCRP, coyfoxbootHCRP)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

HCRP <- cbind(BSmean,UCI,LCI)
HCRP

#boxplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootOne, coyfoxbootEach, coyfoxbootHCRP)

#fit kernel density where both present with low CRP

cactLCRP <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$CRPPrp<.1]
densityPlot(cactLCRP, rug = TRUE)

#Repeat for swift fox
sfactLCRP <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$CRPPrp<.1]
densityPlot(sfactLCRP, rug = TRUE)

#set min lengths
min(length(cactLCRP), length(sfactLCRP))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestLCRP <- overlapEst(cactLCRP, sfactLCRP, type="Dhat4")
coysfestLCRP

overlapPlot(cactLCRP, sfactLCRP, main="Both Present - Low CRP (<10%)", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

coyfoxbootLCRP <- bootstrap(cactLCRP, sfactLCRP, 10000, type="Dhat4")
coyfoxbootLCRP
BSmean <- mean(coyfoxbootLCRP)
CIcoyfox<-bootCI(coysfestLCRP, coyfoxbootLCRP)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

LCRP <- cbind(BSmean,UCI,LCI)
LCRP



par(mfrow=c(1,1))

for.plot.5 <- rbind(All, Both, LCRP, HCRP)#, LAG, HAG, LSG, HSG, LTE, HTE)
for.plot.5
rownames(for.plot.5) <- c('All', 'Both', 'LCRP', 'HCRP')#, 'LAG', 'HAG', 'LSG', 'HSG', 'LTE', 'HTE')
for.plot.5
for.plot.5 <- as.data.frame(for.plot.5)
plotCI(x = for.plot.5[,1],               # plotrix plot with confidence intervals,
       li = for.plot.5[,3],
       ui = for.plot.5[,2])

k<-ggplot(for.plot.5, aes(BSmean, BSmean )) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = LCI, ymax = UCI))
k
######This is what Adam Wants, Violin Plotss
vioplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootLCRP, coyfoxbootHCRP, 
        coyfoxboothag, coyfoxbootlag, coyfoxboothsg, coyfoxbootlsg, coyfoxboothte, coyfoxbootlte)

boxplot(coyfoxbootAll, coyfoxbootBoth, coyfoxbootLCRP, coyfoxbootHCRP)#, 
        #coyfoxboothag, coyfoxbootlag, coyfoxboothsg, coyfoxbootlsg, coyfoxboothte, coyfoxbootlte)
#define font for plot
black.bold.text <- element_text(face = "bold", color = "black", size="20")


#p <- ggplot(data=LoamyTable, aes(x = new.loamy1, y = predict2)) + geom_line(size=1.9, colour = "black") +
 # geom_ribbon(aes(ymin = lower1, ymax = upper1), alpha = 0.2) + xlab("Proportion of Loamy Tableland Soil") + ylab("") 

#df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2) 
df <- data.frame(grp =c('All', 'Both', 'LCRP', 'HCRP'), for.plot.5)
#k <- ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))
df         

#plot predictions
k<-ggplot(df, aes(x=fct_inorder(grp), BSmean)) +        # ggplot2 plot with confidence intervals
  geom_point(size=6) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI, width=.2)) + xlab("") + ylab("Overlap")
k 

#make graph sexy
k2 = k + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text.x=element_text(family="sans", face="bold", colour="black", size="15", margin=margin(5,40,10,0))) + 
  theme(axis.text.y=element_text(family="sans", face="bold", colour="black", size="15", margin=margin(10,5,0,10))) + 
  theme(axis.title = black.bold.text) + coord_cartesian(ylim = c(0.4, 1), expand = FALSE) + theme(plot.margin = margin(30,17,15,15))
#plot graph
#k
k2







#confidence intervals
bootCI(coysfestLCRP, coyfoxbootLCRP)
bootCI(coysfestHCRP, coyfoxbootHCRP)
mean(coyfoxbootHCRP)
mean(coyfoxbootLCRP)

boxplot(coyfoxbootHCRP, coyfoxbootLCRP,main="", names = c("CRP >10%","CRP <10%"),
        xlab="",
        ylab="D4 Overlap",
        sub="")

#compareCkern in package activity

compareCkern(coyfoxbootHCRP, coyfoxbootLCRP)
?`actmod-class`

AllactHCRP <- timeRad[active$Both == "1" & active$CRPPrp>=.1]
densityPlot(AllactHCRP, rug = TRUE)

AllactLCRP <- timeRad[active$Both == "1" & active$CRPPrp<.1]
densityPlot(AllactLCRP, rug = TRUE)

AllactHAG <- timeRad[active$Both == "1" & active$RowcropPrp>=.37]
densityPlot(AllactHAG, rug = TRUE)

AllactLAG <- timeRad[active$Both == "1" & active$RowcropPrp<.37]
densityPlot(AllactLAG, rug = TRUE)

AllactHTE <- timeRad[active$Both == "1" & active$TotalEdge>=312182.8]
densityPlot(AllactHTE, rug = TRUE)

AllactLTE <- timeRad[active$Both == "1" & active$TotalEdge<312182.8]
densityPlot(AllactLTE, rug = TRUE)

AllactHSGP <- timeRad[active$Both == "1" & active$SGPPrp>=.18]
densityPlot(AllactHSGP, rug = TRUE)

AllactLSGP <- timeRad[active$Both == "1" & active$SGPPrp<.18]
densityPlot(AllactLSGP, rug = TRUE)


#Repeat for swift fox
AllactBoth <- timeRad[active$Both == '1']
densityPlot(AllactBoth, rug = TRUE)

AllactAll <- timeRad[active$CoyPresent == '1' & active$FoxPresent == '1']
densityPlot(AllactAll, rug = TRUE)


fit1 <- fitact(
  AllactHCRP,
  wt = NULL,
  reps = 999,
  bw = NULL,
  adj = 1,
  sample = c("none", "data", "model"),
  bounds = NULL,
  show = TRUE)

fit2 <- fitact(
  AllactAll,
  wt = NULL,
  reps = 999,
  bw = NULL,
  adj = 1,
  sample = c("none", "data", "model"),
  bounds = NULL,
  show = TRUE)

CompareAll <- compareCkern(fit1 = fit1, fit2 = fit2)
CompareAll

#Value What does the output of compareCkern Mean
#A named 4-element vector: obs = observed overlap index; null = mean null overlap index; seNull =
#  standard error of the null distribution; pNull = probability observed index arose by chance.


#bootstrapping
coyfoxboot <- bootstrap(cact, sfact, 10000, type="Dhat4")
(BSmean <- mean(coyfoxboot))

#confidence intervals
bootCI(coysfest, coyfoxboot)

#coefficient of overlap backtransforming
bootCIlogit(coysfest, coyfoxboot)



###########
#############
#############

#For other covariates
#fit kernel density where both present with high Ag
range(active$RowcropPrp)
range(active$SGPPrp)
range(active$TotalEdge)
range(active$SHDI)
range(active$CRPPrp)

mean(active$RowcropPrp)
mean(active$SGPPrp)
mean(active$TotalEdge)
mean(active$CRPPrp)


cacthag <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$RowcropPrp>.37]
densityPlot(cacthag, rug = TRUE)

#Repeat for swift fox
sfacthag <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$RowcropPrp>.37]
densityPlot(sfacthag, rug = TRUE)

#set min lengths
min(length(cacthag), length(sfhactag))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfesthag <- overlapEst(cacthag, sfacthag, type="Dhat4")
coysfesthag

overlapPlot(cacthag, sfacthag, main="Both Present - High Ag (>37%)", linecol = c('black', 'black'),olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

#fit kernel density where both present with low Ag

cactlag <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$RowcropPrp<=.37]
densityPlot(cactlag, rug = TRUE)

#Repeat for swift fox
sfactlag <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$RowcropPrp<=.37]
densityPlot(sfactlag, rug = TRUE)

coyfoxboothag <- bootstrap(cacthag, sfacthag, 10000, type="Dhat4")
coyfoxboothag
BSmean <- mean(coyfoxboothag)
CIcoyfox<-bootCI(coysfesthag, coyfoxboothag)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

HAG <- cbind(BSmean,UCI,LCI)

#set min lengths
min(length(cactlag), length(sfactlag))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestlag <- overlapEst(cactlag, sfactlag, type="Dhat1")
coysfestlag

overlapPlot(cactlag, sfactlag, main="Both Present - Low Ag (<37%)",olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

coyfoxbootlag <- bootstrap(cactlag, sfactlag, 10000, type="Dhat4")
coyfoxbootlag
BSmean <- mean(coyfoxbootlag)
CIcoyfox<-bootCI(coysfestlag, coyfoxbootlag)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

LAG <- cbind(BSmean,UCI,LCI)

#fit kernel density where both present with high SGP

cacthsg <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$SGPPrp>=.18]
densityPlot(cacthsg, rug = TRUE)

#Repeat for swift fox
sfacthsg <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$SGPPrp>=.18]
densityPlot(sfacthsg, rug = TRUE)

#set min lengths
min(length(cacthsg), length(sfacthsg))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfesthsg <- overlapEst(cacthsg, sfacthsg, type="Dhat4")
coysfesthsg

overlapPlot(cacthsg, sfacthsg, main="Both Present - High SGP (>18%)",olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

coyfoxboothsg <- bootstrap(cacthsg, sfacthsg, 10000, type="Dhat4")
coyfoxboothsg
BSmean <- mean(coyfoxboothsg)
CIcoyfox<-bootCI(coysfesthsg, coyfoxboothsg)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

HSG <- cbind(BSmean,UCI,LCI)



#fit kernel density where both present with low SGP

cactlsg <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$SGPPrp<.18]
densityPlot(cactlsg, rug = TRUE)

#Repeat for swift fox
sfactlsg <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$SGPPrp<.18]
densityPlot(sfactlsg, rug = TRUE)

#set min lengths
min(length(cactlsg), length(sfactlsg))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestlsg <- overlapEst(cactlsg, sfactlsg, type="Dhat4")
coysfestlsg

overlapPlot(cactlsg, sfactlsg, main="Both Present - Low SGP (<18%)",olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

coyfoxbootlsg <- bootstrap(cactlsg, sfactlsg, 10000, type="Dhat4")
coyfoxbootlsg
BSmean <- mean(coyfoxbootlsg)
CIcoyfox<-bootCI(coysfestlsg, coyfoxbootlsg)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

LSG <- cbind(BSmean,UCI,LCI)


#fit kernel density where both present with high Total Edge

cacthte <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$TotalEdge>=312183]
densityPlot(cacthte, rug = TRUE)

#Repeat for swift fox
sfacthte <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$TotalEdge>=312183]
densityPlot(sfacthte, rug = TRUE)

#set min lengths
min(length(cacthte), length(sfacthte))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfesthte <- overlapEst(cacthte, sfacthte, type="Dhat4")
coysfesthte

overlapPlot(cacthte, sfacthte, main="Both Present - High TE (>312183)",olapcol = "grey",  cex.main=1.25, cex.lab=1.5, cex.axis=1.5)
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,1), bty='n')

coyfoxboothte <- bootstrap(cacthte, sfacthte, 10000, type="Dhat4")
coyfoxboothte
BSmean <- mean(coyfoxboothte)
CIcoyfox<-bootCI(coysfesthte, coyfoxboothte)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

HTE <- cbind(BSmean,UCI,LCI)


#fit kernel density where both present with low TE

cactlte <- timeRad[active$Species == 'Coyote' & active$Both == "1" & active$TotalEdge<312183]
densityPlot(cactlte, rug = TRUE)

#Repeat for swift fox
sfactlte <- timeRad[active$Species == 'Swift Fox'& active$Both == '1'& active$TotalEdge<312183]
densityPlot(sfactlte, rug = TRUE)

#set min lengths
min(length(cactlte), length(sfactlte))

#plot overlap (Dhat 1 if smaller sample is has less than 75 observations, dhat 4 if more)
coysfestlte <- overlapEst(cactlte, sfactlte, type="Dhat4")
coysfestlte

overlapPlot(cactlte, sfactlte, main="Both Present - Low TE (<312702)")
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

coysfesthte <- overlapEst(cacthte, sfacthte, type="Dhat4")
coysfesthte

overlapPlot(cacthte, sfacthte, main="Both Present - High TE (>312702)")
legend('topright', c("Coyote", "Swift Fox"), lty=c(1,2), col=c(1,4), bty='n')

coyfoxbootlte <- bootstrap(cactlte, sfactlte, 10000, type="Dhat4")
coyfoxbootlte
BSmean <- mean(coyfoxbootlte)
CIcoyfox<-bootCI(coysfestlte, coyfoxbootlte)

UCI <- CIcoyfox[1,2]
LCI <- CIcoyfox[1,1]

LTE <- cbind(BSmean,UCI,LCI)


#bootstrapping
coyfoxboot <- bootstrap(cact, sfact, 10000, type="Dhat4")
(BSmean <- mean(coyfoxboot))

#confidence intervals
bootCI(coysfest, coyfoxboot)

#coefficient of overlap backtransforming
bootCIlogit(coysfest, coyfoxboot)

# I need to look at how landscape affects these overlaps

























