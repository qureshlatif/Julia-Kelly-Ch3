library(R2jags) #or...
library(abind)
#setwd("C:/Users/qlatif/Desktop/LoggingData_Julia") # Change to environment
setwd("F:/research stuff/FS_PostDoc/outside_consult/LoggingData_Julia")
load("Data_compiled.RData")

# Covariate inputs #
Logging <- (Plot.data$Logging - mean(Plot.data$Logging))/sd(Plot.data$Logging)
snag <- (Plot.data$snag - mean(Plot.data$snag))/sd(Plot.data$snag)
shrubs <- (Plot.data$shrubs_per - mean(Plot.data$shrubs_per))/sd(Plot.data$shrubs_per)
QMD <- (Plot.data$QMD - mean(Plot.data$QMD))/sd(Plot.data$QMD)

Infest <- as.matrix(cbind(Plot.data[,"EarlInf_2014"],Plot.data[,c("EarlInf_2014","EarlInf_2015","EarlInf_2016")]))
Infest <- (Infest - mean(Infest))/sd(Infest) # Note this is a matrix because values change across years.

nplot <- length(plot)
nyear <- length(year)
nspec <- length(spp)
Y <- Y.arry

Z.init <- apply(Y,c(1,2,3),function(x) sum(x>=1))

# Bin assignments for finite-sample estimates #
Unlogged <- as.numeric(Logging==min(Logging))
Logged <- as.numeric(Logging>min(Logging))

Infest0 <- (Infest==min(Infest))*1
Infest.low <- (Infest>min(Infest)&Infest<0.7)*1
Infest.high <- (Infest>0.7)*1

snag.low <- as.numeric(snag<(-0.65))
snag.mod <- as.numeric(snag>(-0.65)&snag<0.1)
snag.high <- as.numeric(snag>0.1)

QMD.low <- as.numeric(QMD<(-0.6))
QMD.mod <- as.numeric(QMD>(-0.6)&QMD<0.14)
QMD.high <- as.numeric(QMD>0.14)

shrub.low <- as.numeric(shrubs<(-0.5))
shrub.mod <- as.numeric(shrubs>(-0.5)&QMD<0.5)
shrub.high <- as.numeric(shrubs>0.5)

# Assemble the data names list for JAGS.
data <- list("Y","nplot","nyear","n.visits","nspec","Logging","snag","shrubs","QMD","Infest","Unlogged","Logged",
             "Infest0","Infest.low","Infest.high","snag.low","snag.mod","snag.high","QMD.low","QMD.mod",
             "QMD.high","shrub.low","shrub.mod","shrub.high")

# Assemble the initial values for JAGS.  Here z is the actual detection 

inits <- function(){list(Z=Z.init,rho=runif(1,-1,1))}

# Assemble the parameters vector for WinBUGS (What we want to track).
#For model parameters
parameters <- c("pspp","beta0","beta1","beta.Logging","beta.snag","beta.shrubs","beta.QMD","beta.Infest",
                "rho","SR","test","psi.hat","psi.Unlogged","psi.Logged","psi.Inf0","psi.Inflo",
                "psi.Infhi","psi.snaglo","psi.snagmd","psi.snaghi","psi.QMDlo","psi.QMDmd","psi.QMDhi",
                "psi.shrblo","psi.shrbmd","psi.shrbhi")

sink("model.txt")
cat("

# The model.
model{

for(i in 1:nspec){
  # Species-specific parameters for baseline occupancy (intercept) and related parameters for each location.
  beta0[i] ~ dnorm(mu.beta0,tau.beta0)T(-10,10) # mean probability of occupancy for sites previously unoccupied (= colonization or gamma)

  #Species-specific parameters for detection
  mu.eta[i] <- mu.alpha0 + (rho*sd.alpha0/sd.beta0)*(beta0[i] - mu.beta0)      #sd.alpha0 = sd(detection), sd.beta0 = sd(occupancy)
  eta[i] ~ dnorm(mu.eta[i], var.eta)T(-10,10) #logit(p) - incorporates correlation between occupancy and detection across species
  logit(pspp[i]) <- eta[i]        #species-specific detection probability

  #Species-specific parameters for occupancy covariates in the northwest
  beta.Logging[i] ~ dnorm(mu.beta.Logging, tau.beta.Logging)T(-10,10)
  beta.Infest[i] ~ dnorm(mu.beta.Infest, tau.beta.Infest)T(-10,10)
  beta.snag[i] ~ dnorm(mu.beta.snag, tau.beta.snag)T(-10,10)
  beta.QMD[i] ~ dnorm(mu.beta.QMD, tau.beta.QMD)T(-10,10)
  beta.shrubs[i] ~ dnorm(mu.beta.shrubs, tau.beta.shrubs)T(-10,10)

  beta1[i] ~ dnorm(mu.beta1, tau.beta1)T(-10,10) # Persistence offset (added to beta0 if occupied in previous year) 
  # colonization probability (gamma in literature) = expit(beta0)
  # persistence probability (phi in literature) = expit(beta0 + beta1)
  
  #Model
  psi0[i] ~ dunif(0,1) # Probability of occupancy in year prior to sampling   
  for(j in 1:nplot){  # Loop for year 1 - uses z0 to calculate persistence offset 
    z0[j,i] ~ dbern(psi0[i]) # Occupancy state in year prior to sampling
    ###Model for occupancy probability in year 1 ###
    logit(psi[j,i,1]) <- beta0[i] + beta1[i]*z0[j,i] + beta.Logging[i]*Logging[j] + beta.snag[i]*snag[j] +
        beta.shrubs[i]*shrubs[j] + beta.QMD[i]*QMD[j] + beta.Infest[i]*Infest[j,1]
    ###Model for detection probability ###
    Z[j,i,1] ~ dbern(psi[j,i,1]) # Occupancy state
    pZ[j,i,1] <- pspp[i]*Z[j,i,1]  # Unconditional probability of detection
    Y[j,i,1] ~ dbin(pZ[j,i,1],n.visits[1]) # Data model
    #____________Bayesian GOF_________________________________
    ynew[j,i,1] ~ dbin(pZ[j,i,1],n.visits[1])  #simulated new data y under model
    
    LLsim[j,i,1] <- (ynew[j,i,1]*log(pspp[i])+
      (n.visits[1]-ynew[j,i,1])*log(1-pspp[i]))*Z[j,i,1]  #log-likelihood simulated data
    LLdata[j,i,1]<- (Y[j,i,1]*log(pspp[i])+
      (n.visits[1]-Y[j,i,1])*log(1-pspp[i]))*Z[j,i,1]    #log-likelihood observed data
    #_________________________________________________________
    for(t in 2:nyear){  # Loop through remaining years 2 and up - uses Z[,,t-1] to calculate persistence offset
      ###Model for occupancy probability ###
      logit(psi[j,i,t]) <- beta0[i] + beta1[i]*Z[j,i,(t-1)] + beta.Logging[i]*Logging[j] + beta.snag[i]*snag[j] +
        beta.shrubs[i]*shrubs[j] + beta.QMD[i]*QMD[j] + beta.Infest[i]*Infest[j,t]
      ###Model for detection probability ###
      Z[j,i,t] ~ dbern(psi[j,i,t]) # Occupancy state
      pZ[j,i,t] <- pspp[i]*Z[j,i,t] # Unconditional probability of detection
      Y[j,i,t] ~ dbin(pZ[j,i,t],n.visits[t]) # Data model
      #____________Bayesian GOF_________________________________
      ynew[j,i,t] ~ dbin(pZ[j,i,t],n.visits[t])  #simulated data y under model
    
      LLsim[j,i,t] <- (ynew[j,i,t]*log(pspp[i])+
        (n.visits[t]-ynew[j,i,t])*log(1-pspp[i]))*Z[j,i,t]  #log-likelihood simulated data
      LLdata[j,i,t]<- (Y[j,i,t]*log(pspp[i])+
        (n.visits[t]-Y[j,i,t])*log(1-pspp[i]))*Z[j,i,t]    #log-likelihood observed data
      #_________________________________________________________
      }
      #___________Zs for finite-sample estimates________________
      for(t in 1:nyear) {
      Z.unlogged[j,i,t] <- Z[j,i,t]*Unlogged[j]
      Z.logged[j,i,t] <- Z[j,i,t]*Logged[j]
      
      Z.Inf0[j,i,t] <- Z[j,i,t]*Infest0[j,t]
      Z.Inflo[j,i,t] <- Z[j,i,t]*Infest.low[j,t]
      Z.Infhi[j,i,t] <- Z[j,i,t]*Infest.high[j,t]
      
      Z.snaglo[j,i,t] <- Z[j,i,t]*snag.low[j]
      Z.snagmd[j,i,t] <- Z[j,i,t]*snag.mod[j]
      Z.snaghi[j,i,t] <- Z[j,i,t]*snag.high[j]

      Z.QMDlo[j,i,t] <- Z[j,i,t]*QMD.low[j]
      Z.QMDmd[j,i,t] <- Z[j,i,t]*QMD.mod[j]
      Z.QMDhi[j,i,t] <- Z[j,i,t]*QMD.high[j]

      Z.shrblo[j,i,t] <- Z[j,i,t]*shrub.low[j]
      Z.shrbmd[j,i,t] <- Z[j,i,t]*shrub.mod[j]
      Z.shrbhi[j,i,t] <- Z[j,i,t]*shrub.high[j]
      #_________________________________________________________
    }
  }
  # Derive finite-sample occupancy estimates #
  psi.hat[i] <- sum(Z[,i,])/(nplot*nyear)
  
  psi.Unlogged[i] <- sum(Z.unlogged[,i,])/(sum(Unlogged[])*nyear)
  psi.Logged[i] <- sum(Z.logged[,i,])/(sum(Logged[])*nyear)

  psi.Inf0[i] <- sum(Z.Inf0[,i,])/sum(Infest0[,])
  psi.Inflo[i] <- sum(Z.Inflo[,i,])/sum(Infest.low[,])
  psi.Infhi[i] <- sum(Z.Infhi[,i,])/sum(Infest.high[,])

  psi.snaglo[i] <- sum(Z.snaglo[,i,])/(sum(snag.low[])*nyear)
  psi.snagmd[i] <- sum(Z.snagmd[,i,])/(sum(snag.mod[])*nyear)
  psi.snaghi[i] <- sum(Z.snaghi[,i,])/(sum(snag.high[])*nyear)

  psi.QMDlo[i] <- sum(Z.QMDlo[,i,])/(sum(QMD.low[])*nyear)
  psi.QMDmd[i] <- sum(Z.QMDmd[,i,])/(sum(QMD.mod[])*nyear)
  psi.QMDhi[i] <- sum(Z.QMDhi[,i,])/(sum(QMD.high[])*nyear)

  psi.shrblo[i] <- sum(Z.shrblo[,i,])/(sum(shrub.low[])*nyear)
  psi.shrbmd[i] <- sum(Z.shrbmd[,i,])/(sum(shrub.mod[])*nyear)
  psi.shrbhi[i] <- sum(Z.shrbhi[,i,])/(sum(shrub.high[])*nyear)
  }

for(j in 1:nplot){  # Derive species richness estimates
  for(t in 1:nyear){
    SR[j,t] <- sum(Z[j,,t])
  }
}

### Hyper-parameters for detection model ###

mu.alpha0 ~ dnorm(0,0.1)
tau.alpha0 <- 1/(sd.alpha0*sd.alpha0)
sd.alpha0 ~ dunif(0,10)

### Hyper-parameters for occupancy model ###

mu.beta0 ~ dnorm(0,0.1)         #Hyper-parameter for intercept
sd.beta0 ~ dunif(0,10)
tau.beta0 <- 1/(sd.beta0*sd.beta0)

mu.beta1 ~ dnorm(0,0.1)         #Hyper-parameter for persistence offset
sd.beta1 ~ dunif(0,10)
tau.beta1 <- 1/(sd.beta1*sd.beta1)


mu.beta.Logging ~ dnorm(0,0.1)
tau.beta.Logging <- 1/(sd.beta.Logging*sd.beta.Logging)
sd.beta.Logging ~ dunif(0,10)

mu.beta.Infest ~ dnorm(0,0.1)
tau.beta.Infest <- 1/(sd.beta.Infest*sd.beta.Infest)
sd.beta.Infest ~ dunif(0,10)

mu.beta.snag ~ dnorm(0,0.1)
tau.beta.snag <- 1/(sd.beta.snag*sd.beta.snag)
sd.beta.snag ~ dunif(0,10)

mu.beta.QMD ~ dnorm(0,0.1)
tau.beta.QMD <- 1/(sd.beta.QMD*sd.beta.QMD)
sd.beta.QMD ~ dunif(0,10)

mu.beta.shrubs ~ dnorm(0,0.1)
tau.beta.shrubs <- 1/(sd.beta.shrubs*sd.beta.shrubs)
sd.beta.shrubs ~ dunif(0,10)

rho ~ dunif(-1,1)  			#correlation parameter relating occupancy with detection
var.eta <- tau.alpha0/(1.-pow(rho,2))  #variance in detection

#________Bayesian GOF___________
#deviance
dev_sim <- (-2)*sum(LLsim[,,])
dev_data <- (-2)*sum(LLdata[,,])

#test statistics should be ~0.5 if model fits
test<-step(dev_data-dev_sim)
#________________________________

}
",fill=TRUE)
sink()


# MCMC values.
nc <- 4
nb <- 5000
ni <- 55000
nt <- 1

# Send it all to JAGS and hope for the best!

# To help track time.
starttime <- Sys.time()

bugout <- jags(data, inits, parameters, model.file="model.txt", n.chains=nc, n.iter=ni,
n.burnin=nb, n.thin=nt)
#ni <- 50000
#bugout <- update(bugout,n.iter=ni)

endtime <- Sys.time()
runtime <- endtime - starttime
runtime

#Check n.effectives and R.hats for parameters
length(which(bugout$BUGSoutput$summary[,"n.eff"]<100))/length(bugout$BUGSoutput$summary[,"n.eff"])
min(bugout$BUGSoutput$summary[,"n.eff"])
sort(bugout$BUGSoutput$summary[,"n.eff"])[1:50]
max(bugout$BUGSoutput$summary[,"Rhat"])
sort(bugout$BUGSoutput$summary[,"Rhat"],decreasing=T)[1:50]

#use bugout object to manipulate results in R environment

library(R.utils)
saveObject(bugout,"Mod_Pers_LogInfSnagQMDShrub")

