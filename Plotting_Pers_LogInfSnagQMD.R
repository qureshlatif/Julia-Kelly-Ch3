require(R2jags)
require(ggplot2)
require(cowplot)
require(grid)
setwd ("F:/research stuff/FS_PostDoc/outside_consult/LoggingData_Julia") #If running on desktop.
load("Plotting_Pers_LogInfSnagQMD.RData")

##_____________ Logging & Infection parameter estimates for all species ________________________
Spp <- spp
dat <- data.frame(betas[,c("beta.Logging","beta.Logging.lo","beta.Logging.hi",
                           "beta.Infest","beta.Infest.lo","beta.Infest.hi")],stringsAsFactors=F)
dat$Spp <- Spp
row.names(dat) <- NULL
dat <- dat[order(dat$beta.Infest),]
dat$x <- seq(1:nrow(dat))

pLog <- ggplot(data = dat,aes(x=x,y=beta.Logging)) +
  geom_errorbar(aes(ymin=beta.Logging.lo,ymax=beta.Logging.hi),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_x_continuous(breaks=seq(1,44),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["Logging"])) + xlab("Species") +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=47,y=-2.1),label="A",size=8)

pInf <- ggplot(data = dat,aes(x=x,y=beta.Infest)) +
  geom_errorbar(aes(ymin=beta.Infest.lo,ymax=beta.Infest.hi),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_x_continuous(breaks=seq(1,44),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["Infestation"])) + xlab(NULL) +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=47,y=-2.1),label="B",size=8)

p <- ggdraw() +  
  draw_plot(pLog, x = 0, y = 0, width = .52, height = 1) + 
  draw_plot(pInf, x = 0.52, y = 0, width = .48, height = 1)
#p

save_plot("Betas_all_spp.jpeg", p, ncol = 3, nrow = 3, dpi=600) 

##_____________ Logging, Infection, Snag, & QMD parameter estimates for supported species ____________________
Spp <- spp
dat <- data.frame(betas.supported[,c("beta.Logging","beta.Logging.lo","beta.Logging.hi",
                           "beta.Infest","beta.Infest.lo","beta.Infest.hi",
                           "beta.snag","beta.snag.lo","beta.snag.hi",
                           "beta.QMD","beta.QMD.lo","beta.QMD.hi")],stringsAsFactors=F)
dat$Spp <- row.names(dat)
row.names(dat) <- NULL
dat <- dat[order(dat$beta.Infest),]
dat$x <- seq(1:nrow(dat))

dat$support.Inf <- dat$support.sng <- dat$support.QMD <- "none"
dat$support.Inf[which(dat$beta.Infest.lo>0)] <- "pos"
dat$support.Inf[which(dat$beta.Infest.hi<0)] <- "neg"
dat$support.sng[which(dat$beta.snag.lo>0)] <- "pos"
dat$support.QMD[which(dat$beta.QMD.hi<0)] <- "neg"

pLog <- ggplot(data = dat,aes(x=x,y=beta.Logging)) +
  geom_errorbar(aes(ymin=beta.Logging.lo,ymax=beta.Logging.hi),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_x_continuous(breaks=seq(1,11),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["Logging"])) + xlab("Species") +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=11.5,y=-2.1),label="A",size=8)

pInf <- ggplot(data = dat,aes(x=x,y=beta.Infest)) +
  geom_errorbar(aes(ymin=beta.Infest.lo,ymax=beta.Infest.hi,colour=support.Inf),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_colour_manual(values = c('#0072B2','black','#D55E00')) +  
  scale_x_continuous(breaks=seq(1,11),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["Infestation"])) + xlab(NULL) +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=11.5,y=-2.1),label="B",size=8) +
  guides(colour=F)

pSnag <- ggplot(data = dat,aes(x=x,y=beta.snag)) +
  geom_errorbar(aes(ymin=beta.snag.lo,ymax=beta.snag.hi,colour=support.sng),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_colour_manual(values = c('black','#D55E00')) +  
  scale_x_continuous(breaks=seq(1,11),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["Snag"])) + xlab("Species") +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=11.5,y=-2.1),label="C",size=8) +
  guides(colour=F)

pQMD <- ggplot(data = dat,aes(x=x,y=beta.QMD)) +
  geom_errorbar(aes(ymin=beta.QMD.lo,ymax=beta.QMD.hi,colour=support.QMD),size=1,width=0) +
  geom_point(size=2.5) + 
  geom_hline(yintercept=0) +
  coord_flip() +
  scale_colour_manual(values = c('#0072B2','black')) +  
  scale_x_continuous(breaks=seq(1,11),labels=dat$Spp,expand=c(0,1)) +
  scale_y_continuous(lim=c(-2.15,2.16)) +
  ylab(expression(hat(beta)["QMD"])) + xlab(NULL) +
  theme(axis.title.y=element_text(size=30)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  geom_text(aes(x=11.5,y=-2.1),label="D",size=8) +
  guides(colour=F)

p <- ggdraw() +  
  draw_plot(pLog, x = 0, y = 0.5, width = .52, height = 0.5) + 
  draw_plot(pInf, x = 0.52, y = 0.5, width = .48, height = 0.5) +
  draw_plot(pSnag, x = 0, y = 0, width = .52, height = 0.5) +
  draw_plot(pQMD, x = 0.52, y = 0, width = .48, height = 0.5)

save_plot("Betas_supported_spp.jpeg", p, ncol = 3, nrow = 3, dpi=600) 

##_____________ Species richness X Logging & Infection plots ____________________
SR.est$Logging <- rep(Plot.data$Logging,4)
SR.est$Infest <- c(Plot.data$EarlInf_2014,Plot.data$EarlInf_2014,Plot.data$EarlInf_2015,Plot.data$EarlInf_2016)

pLog <- ggplot(data = SR.est,aes(x=Logging,y=md)) + 
  geom_point(alpha=0.4) + 
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.01,alpha=0.4) +
  scale_y_continuous(breaks=c(15,20,25)) +
  labs(x="Proportion cut stumps",y=NULL) +
  theme(axis.title.x=element_text(size=35)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=35)) +
  theme(axis.text.y=element_text(size=35)) +
  geom_text(aes(x=0,y=30),label="A",size=15)

pInf <- ggplot(data = SR.est,aes(x=Infest,y=md)) + 
  geom_point(alpha=0.4) + 
  geom_errorbar(aes(ymin=lo,ymax=hi),width=0.3,alpha=0.4) +
  scale_y_continuous(breaks=c(15,20,25)) +
  labs(x="Number of early infested trees",y=NULL) +
  theme(axis.title.x=element_text(size=35)) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=35)) +
  theme(axis.text.y=element_text(size=35)) +
  geom_text(aes(x=0,y=30),label="B",size=15)

p <- ggdraw() +  
  draw_plot(pLog, x = 0.05, y = 0.5, width = .95, height = 0.5) + 
  draw_plot(pInf, x = 0.05, y = 0, width = .95, height = 0.5) +
  draw_plot_label(label="Species Richness",size=35,x=0,y=0.2,angle=90)

save_plot("SpecRich.jpeg", p, ncol = 2.5, nrow = 3, dpi=600) 

##_____________ Species occupancy estimates and predictions ____________________

## 9 species related with infestation
# Compile column indices for predicted occupancy #
prd.md <- which(substr(dimnames(psi.Inf)[[2]],1,7)=="psiPred"&substr(dimnames(psi.Inf)[[2]],
                                                                     (nchar(dimnames(psi.Inf)[[2]])-1),
                                                                     nchar(dimnames(psi.Inf)[[2]]))=="md")
prd.lo <- which(substr(dimnames(psi.Inf)[[2]],1,7)=="psiPred"&substr(dimnames(psi.Inf)[[2]],
                                                                     (nchar(dimnames(psi.Inf)[[2]])-1),
                                                                     nchar(dimnames(psi.Inf)[[2]]))=="lo")
prd.hi <- which(substr(dimnames(psi.Inf)[[2]],1,7)=="psiPred"&substr(dimnames(psi.Inf)[[2]],
                                                                     (nchar(dimnames(psi.Inf)[[2]])-1),
                                                                     nchar(dimnames(psi.Inf)[[2]]))=="hi")
prd.cols <- as.numeric(rbind(prd.md,prd.lo,prd.hi)) #column indices
rm(prd.md,prd.lo,prd.hi)

# ATTW #
dat.est <- psi.Inf["ATTW",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["ATTW",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pATTW <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
#  geom_errorbarh(data=dat.est,aes(y=psi,xmin=bin.lo,xmax=bin.hi),size=1,height=0,color="dark gray",linetype="longdash") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
#  geom_vline(aes(xintercept=0.5),size=0.5) + # Add vertical lines bin boundaries for finte-sample estimate
#  geom_vline(aes(xintercept=6.5),size=0.5) +
  ylab(NULL) + xlab(NULL) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
#  ylab("Occupancy") + xlab("Infestation") +
#  theme(axis.title.x=element_text(size=30)) +
#  theme(axis.title.y=element_text(size=30)) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "ATTW",size=8)
#save_plot("ATTWv1.jpeg", pATTW, ncol = 2.5, nrow = 3, dpi=600) 
#save_plot("ATTWv2.jpeg", pATTW, ncol = 2.5, nrow = 3, dpi=600) 

# BTLH #
dat.est <- psi.Inf["BTLH",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["BTLH",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pBTLH <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "BTLH",size=8)

# CLNU #
dat.est <- psi.Inf["CLNU",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["CLNU",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pCLNU <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "CLNU",size=8)

# HAWO #
dat.est <- psi.Inf["HAWO",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["HAWO",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pHAWO <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "HAWO",size=8)

# HOWR #
dat.est <- psi.Inf["HOWR",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["HOWR",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pHOWR <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "HOWR",size=8)

# NOFL #
dat.est <- psi.Inf["NOFL",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["NOFL",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pNOFL <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "NOFL",size=8)

# RBNU #
dat.est <- psi.Inf["RBNU",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["RBNU",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pRBNU <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "RBNU",size=8)

# WETA #
dat.est <- psi.Inf["WETA",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["WETA",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pWETA <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "WETA",size=8)

# WEWP #
dat.est <- psi.Inf["WEWP",c("psiEst.Inf0.md","psiEst.Inf0.lo","psiEst.Inf0.hi",
                            "psiEst.Inflo.md","psiEst.Inflo.lo","psiEst.Inflo.hi",
                            "psiEst.Infhi.md","psiEst.Infhi.lo","psiEst.Infhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(0,4.33,12.7)
dat.est$bin.lo <- c(0,1,7)
dat.est$bin.hi <- c(0,5,23)

dat <- as.numeric(psi.Inf["WEWP",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- InfX.plot

pWEWP <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  ylab(NULL) + xlab(NULL) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=3,y=1.05),label = "WEWP",size=8)

p <- ggdraw() +  
  draw_plot(pATTW, x = 0.05, y = 0.684, width = .317, height = .317) + 
  draw_plot(pBTLH, x = 0.367, y = 0.684, width = .317, height = .317) + 
  draw_plot(pCLNU, x = 0.684, y = 0.684, width = .317, height = .317) + 
  draw_plot(pHAWO, x = 0.05, y = 0.367, width = .317, height = .317) + 
  draw_plot(pNOFL, x = 0.367, y = 0.367, width = .317, height = .317) + 
  draw_plot(pRBNU, x = 0.684, y = 0.367, width = .317, height = .317) + 
  draw_plot(pHOWR, x = 0.05, y = 0.05, width = .317, height = .317) + 
  draw_plot(pWETA, x = 0.367, y = 0.05, width = .317, height = .317) + 
  draw_plot(pWEWP, x = 0.684, y = 0.05, width = .317, height = .317) + 
  draw_plot_label(label=c("Infestation (Number of infested spruce)","Point occupancy"),
                  size=c(30,30),x=c(0,0),y=c(0.05,0.3),angle=c(0,90))

save_plot("SppPlots_Inf.jpeg", p, ncol = 3, nrow = 3, dpi=600) 

## 1 species related with snags, and 4 with QMD
# Compile column indices for predicted occupancy with snags #
prd.md <- which(substr(dimnames(psi.snag)[[2]],1,7)=="psiPred"&substr(dimnames(psi.snag)[[2]],
                                                                     (nchar(dimnames(psi.snag)[[2]])-1),
                                                                     nchar(dimnames(psi.snag)[[2]]))=="md")
prd.lo <- which(substr(dimnames(psi.snag)[[2]],1,7)=="psiPred"&substr(dimnames(psi.snag)[[2]],
                                                                     (nchar(dimnames(psi.snag)[[2]])-1),
                                                                     nchar(dimnames(psi.snag)[[2]]))=="lo")
prd.hi <- which(substr(dimnames(psi.snag)[[2]],1,7)=="psiPred"&substr(dimnames(psi.snag)[[2]],
                                                                     (nchar(dimnames(psi.snag)[[2]])-1),
                                                                     nchar(dimnames(psi.snag)[[2]]))=="hi")
prd.cols <- as.numeric(rbind(prd.md,prd.lo,prd.hi)) #column indices
rm(prd.md,prd.lo,prd.hi)

# HOWR #
dat.est <- psi.snag["HOWR",c("psiEst.snaglo.md","psiEst.snaglo.lo","psiEst.snaglo.hi",
                            "psiEst.snagmd.md","psiEst.snagmd.lo","psiEst.snagmd.hi",
                            "psiEst.snaghi.md","psiEst.snaghi.lo","psiEst.snaghi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(1.36,3.69,7.31)
dat.est$bin.lo <- c(0,3,7)
dat.est$bin.hi <- c(2,6,11)

dat <- as.numeric(psi.snag["HOWR",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- SnagX.plot

pHOWR <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=0.5) +
  ylab(NULL) + xlab("Snags (count)") +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=2,y=1.05),label = "HOWR",size=8)

# Compile column indices for predicted occupancy with snags #
prd.md <- which(substr(dimnames(psi.QMD)[[2]],1,7)=="psiPred"&substr(dimnames(psi.QMD)[[2]],
                                                                     (nchar(dimnames(psi.QMD)[[2]])-1),
                                                                     nchar(dimnames(psi.QMD)[[2]]))=="md")
prd.lo <- which(substr(dimnames(psi.QMD)[[2]],1,7)=="psiPred"&substr(dimnames(psi.QMD)[[2]],
                                                                     (nchar(dimnames(psi.QMD)[[2]])-1),
                                                                     nchar(dimnames(psi.QMD)[[2]]))=="lo")
prd.hi <- which(substr(dimnames(psi.QMD)[[2]],1,7)=="psiPred"&substr(dimnames(psi.QMD)[[2]],
                                                                     (nchar(dimnames(psi.QMD)[[2]])-1),
                                                                     nchar(dimnames(psi.QMD)[[2]]))=="hi")
prd.cols <- as.numeric(rbind(prd.md,prd.lo,prd.hi)) #column indices
rm(prd.md,prd.lo,prd.hi)

# AMRO #
dat.est <- psi.QMD["AMRO",c("psiEst.QMDlo.md","psiEst.QMDlo.lo","psiEst.QMDlo.hi",
                            "psiEst.QMDmd.md","psiEst.QMDmd.lo","psiEst.QMDmd.hi",
                            "psiEst.QMDhi.md","psiEst.QMDhi.lo","psiEst.QMDhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(514,818,1331)
dat.est$bin.lo <- c(331,635,960)
dat.est$bin.hi <- c(630,939,2441)

dat <- as.numeric(psi.QMD["AMRO",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- QMDX.plot

pAMRO <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=50) +
  labs(y=NULL,x=expression("QMD ("~cm^2~")")) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=500,y=1.05),label = "AMRO",size=8)

# WETA #
dat.est <- psi.QMD["WETA",c("psiEst.QMDlo.md","psiEst.QMDlo.lo","psiEst.QMDlo.hi",
                            "psiEst.QMDmd.md","psiEst.QMDmd.lo","psiEst.QMDmd.hi",
                            "psiEst.QMDhi.md","psiEst.QMDhi.lo","psiEst.QMDhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(514,818,1331)
dat.est$bin.lo <- c(331,635,960)
dat.est$bin.hi <- c(630,939,2441)

dat <- as.numeric(psi.QMD["WETA",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- QMDX.plot

pWETA <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=50) +
  labs(y=NULL,x=expression("QMD ("~cm^2~")")) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=500,y=1.05),label = "WETA",size=8)

# WISA #
dat.est <- psi.QMD["WISA",c("psiEst.QMDlo.md","psiEst.QMDlo.lo","psiEst.QMDlo.hi",
                            "psiEst.QMDmd.md","psiEst.QMDmd.lo","psiEst.QMDmd.hi",
                            "psiEst.QMDhi.md","psiEst.QMDhi.lo","psiEst.QMDhi.hi")]
dat.est <- data.frame(rbind(as.numeric(dat.est[1:3]),as.numeric(dat.est[4:6]),as.numeric(dat.est[7:9])))
names(dat.est) <- c("psi","psi.lo","psi.hi")
dat.est$X <- c(514,818,1331)
dat.est$bin.lo <- c(331,635,960)
dat.est$bin.hi <- c(630,939,2441)

dat <- as.numeric(psi.QMD["WISA",prd.cols])
dat.prd <- dat[1:3]
for(i in seq(4,length(dat),by=3)) dat.prd <- rbind(dat.prd,dat[i:(i+2)])
dat.prd <- data.frame(dat.prd,row.names=NULL)
names(dat.prd) <- c("psi","psi.lo","psi.hi")
dat.prd$X <- QMDX.plot

pWISA <- ggplot(data = dat.prd,aes(x=X,y=psi)) +
  geom_line(size=1,linetype="solid") +
  geom_line(aes(y=psi.lo),size=1,linetype="dashed") +
  geom_line(aes(y=psi.hi),size=1,linetype="dashed") +
  geom_point(data=dat.est,aes(x=X,y=psi),size=5) + 
  geom_errorbar(data=dat.est,aes(x=X,ymin=psi.lo,ymax=psi.hi),size=1,width=50) +
  labs(y=NULL,x=expression("QMD ("~cm^2~")")) +
  scale_y_continuous(lim=c(0,1.05),breaks=c(0,0.25,0.5,0.75,1)) +
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.text.x=element_text(size=20)) +
  theme(axis.text.y=element_text(size=25)) +
  guides(shape=FALSE,linetype=FALSE) +
  geom_text(aes(x=500,y=1.05),label = "WISA",size=8)

p <- ggdraw() +  
  draw_plot(pHOWR, x = 0.05, y = 0.5, width = .475, height = .5) + 
  draw_plot(pAMRO, x = 0.525, y = 0.5, width = .475, height = .5) + 
  draw_plot(pWETA, x = 0.05, y = 0, width = .475, height = .5) + 
  draw_plot(pWISA, x = 0.525, y = 0, width = .475, height = .5) + 
  draw_plot_label(label="Point occupancy",size=30,x=0,y=0.3,angle=90)

save_plot("SppPlots_SnagQMD.jpeg", p, ncol = 3, nrow = 3, dpi=600) 
