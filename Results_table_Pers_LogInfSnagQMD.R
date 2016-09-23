library(R2jags)
setwd ("F:/research stuff/FS_PostDoc/outside_consult/LoggingData_Julia") #If running on desktop.
load("Data_compiled.RData")

# Covariate inputs used in model fitting (recalculated here in case needed) #
Logging <- (Plot.data$Logging - mean(Plot.data$Logging))/sd(Plot.data$Logging)
QMD <- (Plot.data$QMD - mean(Plot.data$QMD))/sd(Plot.data$QMD)

Infest <- as.matrix(cbind(Plot.data[,"EarlInf_2014"],Plot.data[,c("EarlInf_2014","EarlInf_2015","EarlInf_2016")]))
Infest <- (Infest - mean(Infest))/sd(Infest)

nplot <- length(plot)
nyear <- length(year)
nspec <- length(spp)
Y <- Y.arry

Z.init <- apply(Y,c(1,2,3),function(x) sum(x>=1))
spp <- toupper(spp)

library(R.utils)
mod <- loadObject("Mod_Pers_LogInfestSnagQMD")

#Functions
logit <- function(x) log(x/(1-x))
expit <- function(x) exp(x)/(1+exp(x))

##_______ Tabulate species slope parameter estimates and 90% credible intervals ____________________________##
rows<-spp
cols<-c("p","p.lo","p.hi","beta0","beta0.lo","beta0.hi","beta1","beta1.lo","beta1.hi","beta.Logging","beta.Logging.lo",
        "beta.Logging.hi","beta.Infest","beta.Infest.lo","beta.Infest.hi","beta.snag","beta.snag.lo","beta.snag.hi",
        "beta.QMD","beta.QMD.lo","beta.QMD.hi")
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

out[,"beta.Infest"] <- apply(mod$BUGSoutput$sims.list$beta.Infest,2,median)  
out[,"beta.Infest.lo"] <- apply(mod$BUGSoutput$sims.list$beta.Infest,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta.Infest.hi"] <- apply(mod$BUGSoutput$sims.list$beta.Infest,2,function(x) quantile(x,prob=0.95,type=8))  

out[,"beta.snag"] <- apply(mod$BUGSoutput$sims.list$beta.snag,2,median)  
out[,"beta.snag.lo"] <- apply(mod$BUGSoutput$sims.list$beta.snag,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta.snag.hi"] <- apply(mod$BUGSoutput$sims.list$beta.snag,2,function(x) quantile(x,prob=0.95,type=8))  

out[,"beta.QMD"] <- apply(mod$BUGSoutput$sims.list$beta.QMD,2,median)  
out[,"beta.QMD.lo"] <- apply(mod$BUGSoutput$sims.list$beta.QMD,2,function(x) quantile(x,prob=0.05,type=8))  
out[,"beta.QMD.hi"] <- apply(mod$BUGSoutput$sims.list$beta.QMD,2,function(x) quantile(x,prob=0.95,type=8))  

write.csv(out,"Beta_Ests_Pers_LogInfestSnagQMD.csv",row.names=T) #Not needed for now.

betas <- out

## Species that had statistically supported relationships with one or more covariates ##
out <- out[which(out[,"beta.Logging.lo"]>0|out[,"beta.Logging.hi"]<0|out[,"beta.Infest.lo"]>0|out[,"beta.Infest.hi"]<0|
                   out[,"beta.QMD.lo"]>0|out[,"beta.QMD.hi"]<0|out[,"beta.snag.lo"]>0|out[,"beta.snag.hi"]<0),]
betas.supported <- out
write.csv(out,"Beta_supported_Pers_LogInfestSnagQMD.csv",row.names=T) #Not needed for now.
rm(out)

##_______ Tabulate occupancy estimates and 90% credible intervals for covariate levels ____________________________##

# Levels for finite-sample estimates:
  # Logging:
    # mean x (Unlogged = 0, Logged = 0.17)
    # range z (Unlogged = min(Logged), Logged > -0.4)
  # Infest:
    # mean x (None = 0, Low = 2.83, Mod/High = 14.9)
    # range z (None = min(Infest), Low = -0.4 - 0.5, Mod/High > 0.5)
  # Snag
    # mean x (Low = 1.36, Low = 3.69, Mod/High = 7.31)
    # range z (Low < -0.65, Mod = -0.3 - 0.1, High > 0.1)
  # QMD (331-[z = -1.33])
    # mean x (Low = 514, Low = 818, Mod/High = 1331)
    # range z (Low < -0.6, Mod = -0.6 - 0.14, High > 0.14)

# Logging #
  # Variables for script generating predicted probabilities #
X.data <- Plot.data$Logging
X <- seq(min(X.data),max(X.data),length.out=20)
LogX.plot <- X # Store values along gradient for plotting predicted occupancy probabilities
Z <- (X - mean(X.data))/sd(X.data)
X.mult <- 100 # Multiplier for column names in table of predicted values
B <- "beta.Logging" # Name of Beta coefficient in BUGS model output
  #_________________________________________________________#

rows<-spp
cols<-c("psiEst.Unlog.md","psiEst.Unlog.lo","psiEst.Unlog.hi",
        "psiEst.Log.md","psiEst.Log.lo","psiEst.Log.hi")
cols <- c(cols,paste("psiPred.Log",round(X*X.mult),".md",sep=""),
          paste("psiPred.Log",round(X*X.mult),".lo",sep=""),
          paste("psiPred.Log",round(X*X.mult),".hi",sep=""))
psi.log<-matrix(NA,nrow=length(rows),ncol=length(cols))
dimnames(psi.log)<-list(rows,cols)

psi.log[,"psiEst.Unlog.md"] <- apply(mod$BUGSoutput$sims.list$psi.Unlogged,2,median)
psi.log[,"psiEst.Unlog.lo"] <- apply(mod$BUGSoutput$sims.list$psi.Unlogged,2,function(x) quantile(x,prob=0.05,type=8))
psi.log[,"psiEst.Unlog.hi"] <- apply(mod$BUGSoutput$sims.list$psi.Unlogged,2,function(x) quantile(x,prob=0.95,type=8))

psi.log[,"psiEst.Log.md"] <- apply(mod$BUGSoutput$sims.list$psi.Logged,2,median)
psi.log[,"psiEst.Log.lo"] <- apply(mod$BUGSoutput$sims.list$psi.Logged,2,function(x) quantile(x,prob=0.05,type=8))
psi.log[,"psiEst.Log.hi"] <- apply(mod$BUGSoutput$sims.list$psi.Logged,2,function(x) quantile(x,prob=0.95,type=8))

for(i in 1:length(X)) {
  psi <- expit(mod$BUGSoutput$sims.list$beta0 +
                 mod$BUGSoutput$sims.list$beta1*mod$BUGSoutput$sims.list$psi.hat +
                 mod$BUGSoutput$sims.list[[B]]*Z[i])
  psi.log[,paste("psiPred.Log",round(X[i]*X.mult),".md",sep="")] <- apply(psi,2,median)
  psi.log[,paste("psiPred.Log",round(X[i]*X.mult),".lo",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.05,type=8))
  psi.log[,paste("psiPred.Log",round(X[i]*X.mult),".hi",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.95,type=8))

}

# Infestation #
  # Variables for script generating predicted probabilities #
X.data <- as.numeric(as.matrix(cbind(Plot.data[,"EarlInf_2014"],
                                     Plot.data[,c("EarlInf_2014","EarlInf_2015","EarlInf_2016")])))
X <- seq(min(X.data),max(X.data),length.out=20)
InfX.plot <- X # Store values along gradient for plotting predicted occupancy probabilities
Z <- (X - mean(X.data))/sd(X.data)
X.mult <- 1 # Multiplier for column names in table of predicted values
B <- "beta.Infest" # Name of Beta coefficient in BUGS model output
  #_________________________________________________________#

rows<-spp
cols<-c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
        "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
        "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")
cols <- c(cols,paste("psiPred.Inf",round(X*X.mult),".md",sep=""),
          paste("psiPred.Inf",round(X*X.mult),".lo",sep=""),
          paste("psiPred.Inf",round(X*X.mult),".hi",sep=""))
psi.Inf<-matrix(NA,nrow=length(rows),ncol=length(cols))
dimnames(psi.Inf)<-list(rows,cols)

psi.Inf[,"psiEst.Inf0.md"] <- apply(mod$BUGSoutput$sims.list$psi.Inf0,2,median)
psi.Inf[,"psiEst.Inf0.lo"] <- apply(mod$BUGSoutput$sims.list$psi.Inf0,2,function(x) quantile(x,prob=0.05,type=8))
psi.Inf[,"psiEst.Inf0.hi"] <- apply(mod$BUGSoutput$sims.list$psi.Inf0,2,function(x) quantile(x,prob=0.95,type=8))

psi.Inf[,"psiEst.Inflo.md"] <- apply(mod$BUGSoutput$sims.list$psi.Inflo,2,median)
psi.Inf[,"psiEst.Inflo.lo"] <- apply(mod$BUGSoutput$sims.list$psi.Inflo,2,function(x) quantile(x,prob=0.05,type=8))
psi.Inf[,"psiEst.Inflo.hi"] <- apply(mod$BUGSoutput$sims.list$psi.Inflo,2,function(x) quantile(x,prob=0.95,type=8))

psi.Inf[,"psiEst.Infhi.md"] <- apply(mod$BUGSoutput$sims.list$psi.Infhi,2,median)
psi.Inf[,"psiEst.Infhi.lo"] <- apply(mod$BUGSoutput$sims.list$psi.Infhi,2,function(x) quantile(x,prob=0.05,type=8))
psi.Inf[,"psiEst.Infhi.hi"] <- apply(mod$BUGSoutput$sims.list$psi.Infhi,2,function(x) quantile(x,prob=0.95,type=8))

for(i in 1:length(X)) {
  psi <- expit(mod$BUGSoutput$sims.list$beta0 +
                 mod$BUGSoutput$sims.list$beta1*mod$BUGSoutput$sims.list$psi.hat +
                 mod$BUGSoutput$sims.list[[B]]*Z[i])
  psi.Inf[,paste("psiPred.Inf",round(X[i]*X.mult),".md",sep="")] <- apply(psi,2,median)
  psi.Inf[,paste("psiPred.Inf",round(X[i]*X.mult),".lo",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.05,type=8))
  psi.Inf[,paste("psiPred.Inf",round(X[i]*X.mult),".hi",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.95,type=8))

}

# Snags #
  # Variables for script generating predicted probabilities #
X.data <- Plot.data$snag
X <- seq(min(X.data),max(X.data))
SnagX.plot <- X # Store values along gradient for plotting predicted occupancy probabilities
Z <- (X - mean(X.data))/sd(X.data)
X.mult <- 1 # Multiplier for column names in table of predicted values
B <- "beta.snag" # Name of Beta coefficient in BUGS model output
  #_________________________________________________________#

rows<-spp
cols<-c("psiEst.snaglo.md","psiEst.snaglo.lo","psiEst.snaglo.hi",
        "psiEst.snagmd.md","psiEst.snagmd.lo","psiEst.snagmd.hi",
        "psiEst.snaghi.md","psiEst.snaghi.lo","psiEst.snaghi.hi")
cols <- c(cols,paste("psiPred.Snag",round(X*X.mult),".md",sep=""),
          paste("psiPred.Snag",round(X*X.mult),".lo",sep=""),
          paste("psiPred.Snag",round(X*X.mult),".hi",sep=""))
psi.snag<-matrix(NA,nrow=length(rows),ncol=length(cols))
dimnames(psi.snag)<-list(rows,cols)

psi.snag[,"psiEst.snaglo.md"] <- apply(mod$BUGSoutput$sims.list$psi.snaglo,2,median)
psi.snag[,"psiEst.snaglo.lo"] <- apply(mod$BUGSoutput$sims.list$psi.snaglo,2,function(x) quantile(x,prob=0.05,type=8))
psi.snag[,"psiEst.snaglo.hi"] <- apply(mod$BUGSoutput$sims.list$psi.snaglo,2,function(x) quantile(x,prob=0.95,type=8))

psi.snag[,"psiEst.snagmd.md"] <- apply(mod$BUGSoutput$sims.list$psi.snagmd,2,median)
psi.snag[,"psiEst.snagmd.lo"] <- apply(mod$BUGSoutput$sims.list$psi.snagmd,2,function(x) quantile(x,prob=0.05,type=8))
psi.snag[,"psiEst.snagmd.hi"] <- apply(mod$BUGSoutput$sims.list$psi.snagmd,2,function(x) quantile(x,prob=0.95,type=8))

psi.snag[,"psiEst.snaghi.md"] <- apply(mod$BUGSoutput$sims.list$psi.snaghi,2,median)
psi.snag[,"psiEst.snaghi.lo"] <- apply(mod$BUGSoutput$sims.list$psi.snaghi,2,function(x) quantile(x,prob=0.05,type=8))
psi.snag[,"psiEst.snaghi.hi"] <- apply(mod$BUGSoutput$sims.list$psi.snaghi,2,function(x) quantile(x,prob=0.95,type=8))

for(i in 1:length(X)) {
  psi <- expit(mod$BUGSoutput$sims.list$beta0 +
                 mod$BUGSoutput$sims.list$beta1*mod$BUGSoutput$sims.list$psi.hat +
                 mod$BUGSoutput$sims.list[[B]]*Z[i])
  psi.snag[,paste("psiPred.Snag",round(X[i]*X.mult),".md",sep="")] <- apply(psi,2,median)
  psi.snag[,paste("psiPred.Snag",round(X[i]*X.mult),".lo",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.05,type=8))
  psi.snag[,paste("psiPred.Snag",round(X[i]*X.mult),".hi",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.95,type=8))

}

# QMD #
  # Variables for script generating predicted probabilities #
X.data <- Plot.data$QMD
X <- seq(min(X.data),max(X.data),length.out=20)
QMDX.plot <- X # Store values along gradient for plotting predicted occupancy probabilities
Z <- (X - mean(X.data))/sd(X.data)
X.mult <- 1 # Multiplier for column names in table of predicted values
B <- "beta.QMD" # Name of Beta coefficient in BUGS model output
  #_________________________________________________________#

rows<-spp
cols<-c("psiEst.QMDlo.md","psiEst.QMDlo.lo","psiEst.QMDlo.hi",
        "psiEst.QMDmd.md","psiEst.QMDmd.lo","psiEst.QMDmd.hi",
        "psiEst.QMDhi.md","psiEst.QMDhi.lo","psiEst.QMDhi.hi")
cols <- c(cols,paste("psiPred.QMD",round(X*X.mult),".md",sep=""),
          paste("psiPred.QMD",round(X*X.mult),".lo",sep=""),
          paste("psiPred.QMD",round(X*X.mult),".hi",sep=""))
psi.QMD<-matrix(NA,nrow=length(rows),ncol=length(cols))
dimnames(psi.QMD)<-list(rows,cols)

psi.QMD[,"psiEst.QMDlo.md"] <- apply(mod$BUGSoutput$sims.list$psi.QMDlo,2,median)
psi.QMD[,"psiEst.QMDlo.lo"] <- apply(mod$BUGSoutput$sims.list$psi.QMDlo,2,function(x) quantile(x,prob=0.05,type=8))
psi.QMD[,"psiEst.QMDlo.hi"] <- apply(mod$BUGSoutput$sims.list$psi.QMDlo,2,function(x) quantile(x,prob=0.95,type=8))

psi.QMD[,"psiEst.QMDmd.md"] <- apply(mod$BUGSoutput$sims.list$psi.QMDmd,2,median)
psi.QMD[,"psiEst.QMDmd.lo"] <- apply(mod$BUGSoutput$sims.list$psi.QMDmd,2,function(x) quantile(x,prob=0.05,type=8))
psi.QMD[,"psiEst.QMDmd.hi"] <- apply(mod$BUGSoutput$sims.list$psi.QMDmd,2,function(x) quantile(x,prob=0.95,type=8))

psi.QMD[,"psiEst.QMDhi.md"] <- apply(mod$BUGSoutput$sims.list$psi.QMDhi,2,median)
psi.QMD[,"psiEst.QMDhi.lo"] <- apply(mod$BUGSoutput$sims.list$psi.QMDhi,2,function(x) quantile(x,prob=0.05,type=8))
psi.QMD[,"psiEst.QMDhi.hi"] <- apply(mod$BUGSoutput$sims.list$psi.QMDhi,2,function(x) quantile(x,prob=0.95,type=8))

for(i in 1:length(X)) {
  psi <- expit(mod$BUGSoutput$sims.list$beta0 +
                 mod$BUGSoutput$sims.list$beta1*mod$BUGSoutput$sims.list$psi.hat +
                 mod$BUGSoutput$sims.list[[B]]*Z[i])
  psi.QMD[,paste("psiPred.QMD",round(X[i]*X.mult),".md",sep="")] <- apply(psi,2,median)
  psi.QMD[,paste("psiPred.QMD",round(X[i]*X.mult),".lo",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.05,type=8))
  psi.QMD[,paste("psiPred.QMD",round(X[i]*X.mult),".hi",sep="")] <-
    apply(psi,2,function(x) quantile(x,prob=0.95,type=8))

}

##_______ Tabulate species richness estimates and 90% credible intervals for plots ____________________________##
cols <- c("Plot","Year","md","lo","hi")
SR.est <- data.frame(matrix(0,nrow=nplot*nyear,ncol=length(cols),dimnames=list(NULL,cols)))

SR.est$Plot <- ""
SR.est$Plot <- rep(plot,nyear)
SR.est$Year <- c(rep(2013,nplot),rep(2014,nplot),rep(2015,nplot),rep(2016,nplot))
SR.est$md <- as.numeric(apply(mod$BUGSoutput$sims.list$SR,c(2,3),median))
SR.est$lo <- as.numeric(apply(mod$BUGSoutput$sims.list$SR,c(2,3),function(x) quantile(x,prob=0.05,type=8)))
SR.est$hi <- as.numeric(apply(mod$BUGSoutput$sims.list$SR,c(2,3),function(x) quantile(x,prob=0.95,type=8)))

##_______ Store rho and 90% credible intervals ____________________________##
rho <- c(median(mod$BUGSoutput$sims.list$rho),
         quantile(mod$BUGSoutput$sims.list$rho,prob=0.05,type=8),
         quantile(mod$BUGSoutput$sims.list$rho,prob=0.95,type=8))
names(rho) <- c("median","q05","q95")

##_______ Store p-value for GOF test ____________________________##
GOFp <- sum(mod$BUGSoutput$sims.list$test)/mod$BUGSoutput$n.sims

## Cleanup ##
rm(psi,cols,mod,rows,B,X,X.data,X.mult,i,Z)

save.image("Plotting_Pers_LogInfSnagQMD.RData")
