library(R2jags)
setwd ("F:/research stuff/FS_PostDoc/outside_consult/LoggingData_Julia") #If running on desktop.
load("Data_compiled.RData")

# Covariate inputs used in model fitting (recalculated here in case needed) #
Logging <- (Plot.data$Logging - mean(Plot.data$Logging))/sd(Plot.data$Logging)

nplot <- length(plot)
nyear <- length(year)
nspec <- length(spp)
Y <- Y.arry

Z.init <- apply(Y,c(1,2,3),function(x) sum(x>=1))
spp <- toupper(spp)

library(R.utils)
mod <- loadObject("Mod_Pers_Log")

#Functions
logit <- function(x) log(x/(1-x))
expit <- function(x) exp(x)/(1+exp(x))

##_______ Tabulate species slope parameter estimates and 90% credible intervals ____________________________##
rows<-spp
cols<-c("p","p.lo","p.hi","beta0","beta0.lo","beta0.hi","beta1","beta1.lo","beta1.hi",
        "beta.Logging","beta.Logging.lo","beta.Logging.hi")
out<-matrix(NA,nrow=length(rows),ncol=length(cols))
dimnames(out)<-list(rows,cols)

out[,"beta0"] <- apply(mod$BUGSoutput$sims.list$beta0,2,median)  
out[,"beta0.lo"] <- apply(mod$BUGSoutput$sims.list$beta0,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta0.hi"] <- apply(mod$BUGSoutput$sims.list$beta0,2,function(x) quantile(x,prob=0.95,type=8))  

out[,"beta1"] <- apply(mod$BUGSoutput$sims.list$beta1,2,median)  
out[,"beta1.lo"] <- apply(mod$BUGSoutput$sims.list$beta1,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta1.hi"] <- apply(mod$BUGSoutput$sims.list$beta1,2,function(x) quantile(x,prob=0.95,type=8))  

out[,"p"] <- apply(mod$BUGSoutput$sims.list$pspp,2,median)  
out[,"p.lo"] <- apply(mod$BUGSoutput$sims.list$pspp,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"p.hi"] <- apply(mod$BUGSoutput$sims.list$pspp,2,function(x) quantile(x,prob=0.95,type=8))  

out[,"beta.Logging"] <- apply(mod$BUGSoutput$sims.list$beta.Logging,2,median)  
out[,"beta.Logging.lo"] <- apply(mod$BUGSoutput$sims.list$beta.Logging,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta.Logging.hi"] <- apply(mod$BUGSoutput$sims.list$beta.Logging,2,function(x) quantile(x,prob=0.95,type=8))  

write.csv(out,"Beta_Ests_Pers_Log.csv",row.names=T)
