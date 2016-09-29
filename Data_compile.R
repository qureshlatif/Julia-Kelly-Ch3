#setwd("C:/Users/qlatif/Desktop/LoggingData_Julia") # Change to environment
setwd("F:/research stuff/FS_PostDoc/outside_consult/LoggingData_Julia") # Change to environment

## Load and clean data ##
Detection.data <- read.csv("BirdsLog.csv",header=T,stringsAsFactors=F)
Detection.data <- Detection.data[-which(Detection.data$point==88),]

  # Cleanup plot IDs 
Detection.data <- Detection.data[-which(Detection.data$plot=="11a"),]
Detection.data <- Detection.data[-which(Detection.data$plot=="11b"),]
Detection.data <- Detection.data[-which(Detection.data$plot=="4a"),]
Detection.data <- Detection.data[-which(Detection.data$plot=="4b"),]
Detection.data <- Detection.data[-which(Detection.data$plot=="5a"),]
Detection.data <- Detection.data[-which(Detection.data$plot=="5b"),]
Detection.data$plot[which(Detection.data$plot=="11a 1")] <- "11a1"
Detection.data$plot[which(Detection.data$plot=="11a 7")] <- "11a7"
Detection.data$plot[which(Detection.data$plot=="11a 8")] <- "11a8"
Detection.data$plot[which(Detection.data$plot=="11a 9")] <- "11a9"
sort(unique(Detection.data$plot))

  # Cleanup species codes and remove non-species records
Detection.data$species[which(Detection.data$species=="hwo")] <- "hawo"
Detection.data$species[which(Detection.data$species=="rcki ")] <- "rcki"
Detection.data$species[which(Detection.data$species=="rcka")] <- "rcki"
Detection.data$species[which(Detection.data$species=="nobi ")] <- "nobi"
Detection.data$species[which(Detection.data$species=="bthu")] <- "bthl"
Detection.data$species[which(Detection.data$species=="cora ")] <- "cora"
Detection.data$species[which(Detection.data$species=="rnsp")] <- "rnsa"
Detection.data$species[which(Detection.data$species=="pi")] <- "pisi"
Detection.data$species[which(Detection.data$species=="bthl")] <- "btlh"
Detection.data$species[which(Detection.data$species=="grja")] <- "graj"
Detection.data$species[which(Detection.data$species=="wesp")] <- "wcsp"
Detection.data$species[which(Detection.data$species=="auwa")] <- "yrwa"
Detection.data$species[which(Detection.data$species=="rbsa")] <- "rnsa"
Detection.data$species[which(Detection.data$species=="webl")] <- "mobl"
Detection.data <- Detection.data[-which(Detection.data$species=="usap"),]
Detection.data <- Detection.data[-which(Detection.data$species=="dugr"),]
Detection.data <- Detection.data[-which(Detection.data$species=="witu"),]
Detection.data <- Detection.data[-which(Detection.data$species=="coha"),]
Detection.data <- Detection.data[-which(Detection.data$species=="unac"),]
Detection.data <- Detection.data[-which(Detection.data$species=="pika"),]
Detection.data <- Detection.data[-which(Detection.data$species=="resq"),]
Detection.data <- Detection.data[-which(Detection.data$species=="plvi"),]
Detection.data <- Detection.data[-which(Detection.data$species=="unbi"),]
Detection.data <- Detection.data[-which(Detection.data$species=="unem"),]
Detection.data <- Detection.data[-which(Detection.data$species=="unfa"),]
Detection.data <- Detection.data[-which(Detection.data$species=="unwo"),]
Detection.data <- Detection.data[-which(Detection.data$species=="ssha"),]
Detection.data <- Detection.data[-which(Detection.data$species=="cora"),]
sort(unique(Detection.data$species))

  # Cleanup codes for how birds were detected (not really used)
Detection.data <- Detection.data[-which(Detection.data$how=="f"),]
sort(unique(Detection.data$how))

  # Check years
sort(unique(Detection.data$year))

  # Check visit numbers
sort(unique(Detection.data$survey_round))

  # Truncate by distance (100m cutoff)
Detection.data <- Detection.data[-which(Detection.data$distance>100),]

## Compile detection data array ##
plot <- sort(unique(Detection.data$plot))
spp <- sort(unique(Detection.data$species))
spp <- spp[-which(spp=="nobi")]
year <- sort(unique(Detection.data$year))
n.visits <- c(1,3,3,3) # 1 visit in 2013, then 3 visits in each other year
Y.arry <- array(0,c(length(plot),length(spp),3,length(year)))
for(j in 1:dim(Y.arry)[1]) for(k in 1:3) for(t in 1:length(year)) {
  obs <- Detection.data[which(Detection.data$plot==plot[j]&Detection.data$survey_round==k&
                                Detection.data$year==year[t]),]
  for(i in 1:dim(Y.arry)[2]) if(any(obs$species==spp[i])) Y.arry[j,i,k,t] <- 1
}
Y.arry <- apply(Y.arry,c(1,2,4),sum) # Collapses across visits (y = number of visits [0-3] species was detected).

apply(Y.arry,2,sum) #Total number of detections by species
apply(Y.arry,2,function(x) sum(x>0)) #Total number of point X year occasions each species was detected (reported this in Ecosphere table [Latif et al. 2016])

## Compile covariate matrix ##
Plot.data <- read.csv("PlotLevel_Data.csv",header=T,stringsAsFactors=F)
Plot.data <- Plot.data[-which(substr(Plot.data$plot,1,2)=="1a"|substr(Plot.data$plot,1,2)=="2a"),]
sum(!is.element(Plot.data$plot,plot)) # Should be 0.
sort(unique(Plot.data$plot),na.last=T)
Plot.data <- Plot.data[order(Plot.data$plot),] #sort plot IDs to match order in detection data.

Plot.data$treat_cont[which(Plot.data$treat_cont=="t ")] <- "t"
unique(Plot.data$treat_cont)

sort(unique(Plot.data$overstory_density))
sort(unique(Plot.data$shrubs_per))
sort(unique(Plot.data$litter_per))
sort(unique(Plot.data$forbes_per))
sort(unique(Plot.data$grasses_per))
unique(Plot.data$shrubs_per+Plot.data$litter_per+Plot.data$forbes_per+Plot.data$grasses_per) #Should be 100 for all.

Plot.data <- Plot.data[,c("plot","treat_cont","slope","overstory_density","shrubs_per","litter_per",
                          "forbes_per","grasses_per")]
cor(Plot.data[,c("slope","overstory_density","shrubs_per","litter_per","forbes_per","grasses_per")])

# Calculate plot-level values for tree data #
Tree.data <- read.csv("TreesLog.csv",header=T,stringsAsFactors=F)
sum(!is.element(Tree.data$plot,Plot.data$plot))
sum(!is.element(Plot.data$plot,Tree.data$plot))

Tree.data$core_outsideplot[which(Tree.data$core_outsideplot=="yes")] <- "y"
sort(unique(Tree.data$core_outsideplot))

  # Check tree species 
Tree.data$species[which(Tree.data$species=="aba")] <- "abla"
Tree.data$species[which(Tree.data$species=="abla ")] <- "abla"
Tree.data$species[which(Tree.data$species=="albla")] <- "abla"
Tree.data$species[which(Tree.data$species=="albla")] <- "abla"
Tree.data$species[which(Tree.data$species=="piea")] <- "pien"
Tree.data$species[which(Tree.data$species=="pien ")] <- "pien"
Tree.data$species[which(Tree.data$species=="ppien")] <- "pien"
sort(unique(Tree.data$species))

  # Check dbh columns
sort(unique(Tree.data$snag_dbh),na.last=T)
sort(unique(Tree.data$stump_dbh),na.last=T)
sort(unique(Tree.data$tree_dbh),na.last=T)

  # Check that there is only one dbh value per tree and look for missing DBH
sum(!is.na(Tree.data$snag_dbh))+sum(!is.na(Tree.data$stump_dbh))+sum(!is.na(Tree.data$tree_dbh)) # 9 trees missing DBH
sum((!is.na(Tree.data$snag_dbh))+(!is.na(Tree.data$stump_dbh))+(!is.na(Tree.data$tree_dbh))) # Should be same as prev row
  # Only one value per tree found. 9 missing values found.

  # Combine dbh columns and create indicator of tree type
Tree.data$type <- ""
Tree.data$type[which(!is.na(Tree.data$snag_dbh))] <- "snag"
Tree.data$type[which(!is.na(Tree.data$tree_dbh))] <- "tree"
Tree.data$type[which(!is.na(Tree.data$stump_dbh))] <- "stump"
Tree.data$type[which(Tree.data$type=="")] <- "tree"
Tree.data$dbh <- 0
for(i in 1:nrow(Tree.data)) Tree.data$dbh[i] <-
  sum(Tree.data$snag_dbh[i],Tree.data$tree_dbh[i],Tree.data$stump_dbh[i],na.rm=T)

  # Fix additional dbh errors
Tree.data$dbh[which(Tree.data$plot=="5b2"&Tree.data$dbh==3)] <- 13
Tree.data$dbh[which(Tree.data$plot=="5b4"&Tree.data$dbh==3)] <- 34
Tree.data$dbh[which(Tree.data$plot=="5b5"&Tree.data$dbh==1)] <- 11
Tree.data$dbh[which(Tree.data$plot=="5b9"&Tree.data$dbh==3)] <- 31
Tree.data$dbh[which(Tree.data$dbh==0)] <- NA
sort(unique(Tree.data$dbh),na.last=T)

  # Check status
Tree.data$status[which(Tree.data$status=="Br")] <- "br"
Tree.data$status[which(Tree.data$status=="g")] <- "gr"
Tree.data$status[which(Tree.data$status=="grr")] <- "gr"
Tree.data$status[which(Tree.data$status=="i")] <- "li"
Tree.data$status[which(Tree.data$status=="l")] <- "li"
Tree.data$status[which(Tree.data$status=="lo")] <- "li"
Tree.data$status[which(Tree.data$status=="te")] <- "tw"
Tree.data$status[which(is.element(Tree.data$status,c("br","nd","tw","sn","de")))] <- "snag"
Tree.data$status[which(is.element(Tree.data$status,c("gr","ye")))] <- "early inf"
Tree.data$status[which(Tree.data$status=="li")] <- "healthy"
sort(unique(Tree.data$status),na.last=T)

  # Fill in 3 missing dbh values
mn <- mean(Tree.data$dbh[which(Tree.data$plot=="5b5"&Tree.data$species=="pien")],na.rm=T)
Tree.data$dbh[which(Tree.data$plot=="5b5"&is.na(Tree.data$dbh))] <- mn

mn <- mean(Tree.data$dbh[which(Tree.data$plot=="11a2"&Tree.data$species=="pien")],na.rm=T)
Tree.data$dbh[which(Tree.data$plot=="11a2"&is.na(Tree.data$dbh))] <- mn

mn <- mean(Tree.data$dbh[which(Tree.data$plot=="4a6"&Tree.data$species=="abla")],na.rm=T)
Tree.data$dbh[which(Tree.data$plot=="4a6"&is.na(Tree.data$dbh))] <- mn

rm(mn)

  # Compile tree summary values into Plot-level table #
Plot.data$QMD <- Plot.data$spruceBA <- Plot.data$snag <-
  Plot.data$EarlInf_2016 <- Plot.data$EarlInf_2015 <- Plot.data$EarlInf_2014 <- Plot.data$Logging <- 0
for(i in 1:nrow(Plot.data)) {
  obs2014 <- Tree.data[which(Tree.data$plot==Plot.data$plot[i]&Tree.data$year==2014&Tree.data$core_outsideplot!="y"),]
  obs2015 <- Tree.data[which(Tree.data$plot==Plot.data$plot[i]&Tree.data$year==2015&Tree.data$core_outsideplot!="y"),]
  obs2016 <- Tree.data[which(Tree.data$plot==Plot.data$plot[i]&Tree.data$year==2016&Tree.data$core_outsideplot!="y"),]
  Plot.data$QMD[i] <- mean(obs2014$dbh[obs2014$type!="stump"]^2,na.rm=T)
  spruce2014 <- obs2014[which(obs2014$species=="pien"&obs2014$type!="stump"),]
  ifelse(nrow(spruce2014)>0,Plot.data$spruceBA[i] <- sum(0.005454154*(spruce2014$dbh^2)),Plot.data$spruceBA[i] <- 0)
  Plot.data$snag[i] <- sum(obs2014$status=="snag"&obs2014$dbh>=23)
  Plot.data$EarlInf_2014[i] <- sum(obs2014$status=="early inf")
  Plot.data$EarlInf_2015[i] <- sum(obs2015$status=="early inf")
  Plot.data$EarlInf_2016[i] <- sum(obs2016$status=="early inf")
  Plot.data$Logging[i] <- sum(obs2014$type=="stump")/nrow(obs2014[-which(obs2014$type=="stump")])
}

#cor(Plot.data[,c("slope","Logging","EarlInf_2014","EarlInf_2015","snag","overstory_density","shrubs_per","litter_per",
#                 "forbes_per","grasses_per","QMD","spruceBA")]) # Inspect correlation plot if desired.

# Forest structure PCA (not used but retained in workspace) #
vars <- c("overstory_density","shrubs_per","litter_per","forbes_per","grasses_per","QMD")
nvars <- length(vars)
pca.mod <- prcomp(Plot.data[,vars],scale=T)
summary(pca.mod)$importance["Cumulative Proportion",] # Cumulative variation explained
    # Loadings
pca.load <- matrix(NA,nrow=nvars,ncol=nvars,dimnames=list(vars,paste("PC",1:nvars,sep="")))
for(i in 1:nvars) for(j in 1:nvars) pca.load[i,j] <- cor(Plot.data[,vars[i]],predict(pca.mod)[,j])

Plot.data <- cbind(Plot.data,predict(pca.mod)[,1:3])
names(Plot.data)[16:18] <- c("PC1_forblit","PC2_canopy","PC3_shrbsLrgTrs")

# Variance inflation factors #
vars <- c("Logging","EarlInf_2015","snag","PC1_forblit","PC2_canopy","PC3_shrbsLrgTrs","spruceBA")
VIFs <- matrix(NA,nrow=length(vars),ncol=1,dimnames=list(vars,"VIF"))
for(i in 1:length(vars)) {
  m <- lm(as.formula(paste(vars[i],"~",paste(vars[-i],collapse="+",sep=""),sep="")),data=Plot.data)
  VIFs[i] <- 1/(1 - summary(m)$r.squared)
}
VIFs

# Cleanup
rm(i,j,k,m,nvars,t,vars,spruce2014,obs2014,obs2015,obs2016,obs)

save.image("Data_compiled.RData")
