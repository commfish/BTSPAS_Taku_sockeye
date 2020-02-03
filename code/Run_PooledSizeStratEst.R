# STEP-BY-STEP SCRIPT TO CALCULATE SIZE-STRATIFIED POOLED ESTIMATE
# FOR POST-SEASON REVIEW



##############################################
# Step 1: GET FUNCTIONS

# Note: For now, not using BTSPAS directly in this script, but using
# the local modified version SimplePetersenMod() which is built on top of 
# the SimplePetersen() function from the package.
# BUT: Should expand this to run the BTSPAS estimate as well and compare


library(tidyverse)
source("CODE/FUNCTIONS_SimplePetersen&DropoutAdjustment.R")

force.local <- FALSE # if true, use local version of Carl's functions even if internet connection is available

# get Carl's custom functions 
# check if the URL exists (i.e. have internet connection and correct URL)
library(RCurl)
url.check <- url.exists("https://raw.githubusercontent.com/cschwarz-stat-sfu-ca/taku/master/FUNCTIONS_BTSPAS_Wrappers.R")

# load functions from url or local source
if(url.check & !force.local){
	library(devtools)
      devtools::source_url("https://raw.githubusercontent.com/cschwarz-stat-sfu-ca/taku/master/FUNCTIONS_BTSPAS_Wrappers.R")
	}
	
if(!url.check | force.local){ source("../CODE/Local Copies of Carl Functions/FUNCTIONS_BTSPAS_Wrappers.R") }


# install BTSPAS package

# get the BTSPAS package
library(devtools)
if(url.check){ 

	devtools::install_github("cschwarz-stat-sfu-ca/BTSPAS", dependencies = TRUE,
                        build_vignettes = FALSE, force=TRUE)
	}


# load BTSPAS and dependent packages
library(BTSPAS)
library(ggplot2)




# load packages to extract from excel
#install.packages("readxl")
#install.packages("tibble")
library(readxl)
library(tibble)
library(plyr)



# test the simple petersen function and its modified form

test.out <- SimplePetersenMod(n1 = 2454, m2 = 266, u2 = 4985.2) 
test.out
dropout.adj(Abd=test.out[,"est"],SE_Abd =test.out[,"se"] ,tags_dropped =24, tags_total = 118)

vec.test <- SimplePetersenMod(c(200,400,NA), c(10,20,40), c(300,600,900))





##############################################
# Step 2: PREPARE THE INPUTS

# FOR NOW: ASSUMING THAT ALL RECORDS ARE VALID
# (i.e. not checking that recovery date is after release date etc.)


yr.do <- 2019  # this gets used in the outputs, but doesn't affect the calcs

# get tag release and asl data

# why this file? just includes date and SW, which are also in the next file?
#cyi.tag.dates <- read_excel("PostSeason_SizeStratEst/INPUT/2019 postseason tags out.xlsx",
#                        sheet=1,col_names=TRUE)

# read in asl data
cyi.asl.raw <-  read_excel("PostSeason_SizeStratEst/INPUT/2019 fish wheel asl from Alex.xlsx",
                       sheet=1,col_names=TRUE) 
  
# filter out any records without size measurement
# filter out any records without tag ID
# filter out FW3 - no FW 3 in 2019

cyi.asl.filtered <- cyi.asl.raw # %>%  
                    #filter(!is.na(LENGTH_MILLIMETERS)) %>%
                    #filter(!is.na(SPAGHETTI_TAG_NO)) # %>%
                    #filter(FW %in% c(1,2))

num.filtered <- dim(cyi.asl.raw)[1] - dim(cyi.asl.filtered)[1]
print(paste(num.filtered,"out of",dim(cyi.asl.raw)[1],"filtered out (FW3, no size data)"))


# read in catch and comm asl data
# NOTE: date format discrepancy will need to be handled, but for now just pooling it anyway,
# so not bothering with it.

catch.data <- read.csv("PostSeason_SizeStratEst/INPUT/catch_data (sockeye).csv",
                         stringsAsFactors = FALSE) 

catch.rec <- read.csv("PostSeason_SizeStratEst/INPUT/recovery_data (sockeye).csv",stringsAsFactors=FALSE)


catch.asl <- read_excel("PostSeason_SizeStratEst/INPUT/2019 Canadian Taku Sockeye Commercial Data.xlsx",
                        sheet=1,col_names=TRUE,na=c(""," ","NR")) 

sizeconv.pars <- read.csv("DATA/BaseData/Lookup_LengthConversion.csv",stringsAsFactors = FALSE)

# convert size measurements
length.caf <- catch.asl$"Length - CAF"
length.mef <- sizeconv.pars$Int + sizeconv.pars$Slope * length.caf





########################################
# STEP 3: DO THE CALCULATTIONS

# storage object
pooled.test.df <- data.frame(Year = 1800,plevel=0,MEFsplit =0, EstType = "Tmp", n1 = 0, 
                             m2 = 0,  u2 = 0 ,  est = 0,  se = 0, est.adj = 0,  se.adj = 0,     stringsAsFactors=FALSE)


# 3a: Pooled Estimate ------------------------------------------

cyi.asl.filtered$SPAGHETTI_TAG_NO[duplicated(cyi.asl.filtered$SPAGHETTI_TAG_NO)]

length(unique(cyi.asl.filtered$SPAGHETTI_TAG_NO))
sum(!is.na(cyi.asl.filtered$SPAGHETTI_TAG_NO))

pooled.inputs <-  list(n1= length(unique(cyi.asl.filtered$SPAGHETTI_TAG_NO)) , # number of unique tag ID in release file 
                       m2 = length(unique(catch.rec$TagID)), # number of unique tag ID in catch
                       u2 = sum(catch.data$CdnCommCt) - length(unique(catch.rec$TagID))   # total CdnCommCt - tag recoveries
                    )

pooled.inputs

pooled.est<- SimplePetersenMod(n1 = pooled.inputs$n1, m2 = pooled.inputs$m2 , u2 = pooled.inputs$u2) 
pooled.est

pooled.est.adj <- dropout.adj(Abd=pooled.est[,"est"],
                              SE_Abd =pooled.est[,"se"] ,
                              tags_dropped =13, 
                              tags_total = 51)

pooled.est.adj

pooled.test.df <- rbind(pooled.test.df,
                        c(yr.do, "None", "None", "All",pooled.est,round(pooled.est.adj[1]),
                            round(pooled.est.adj[2])) ) 




# 3b: Size-Stratified Estimate ------------------------------------------

# calculate break points for size stratified estimate
plevels <- seq(0.05,0.95,by=0.05)
plevels.labels <- paste0("p",plevels*100)
size.plevels <- quantile(length.mef,probs=plevels,na.rm=TRUE) 


for(i in 1:length(plevels)){
  
split.use <-   size.plevels[i]
  
# large estimate

 large.rel <-  cyi.asl.filtered %>% filter(LENGTH_MILLIMETERS >= split.use) # number of unique tag ID in release file 
 large.rec <-  length(unique(catch.rec$TagID[catch.rec$TagID %in% large.rel$SPAGHETTI_TAG_NO])) # number of unique tag ID in catch
 large.um <-   (pooled.inputs$u2 * (1- plevels[i])) - large.rec # (total CdnCommCt * Plevel) - tag recoveries
  
  large.inputs <-  list(n1= length(unique(large.rel$SPAGHETTI_TAG_NO)) , 
                         m2 = large.rec, 
                         u2 = large.um)

  large.est<- SimplePetersenMod(n1 = large.inputs$n1, m2 = large.inputs$m2 , u2 = large.inputs$u2) 
  large.est.adj <- dropout.adj(Abd=large.est[,"est"],
                                SE_Abd =large.est[,"se"] ,
                                tags_dropped =13, 
                                tags_total = 51)

  pooled.test.df <- rbind(pooled.test.df,
                          c(yr.do, plevels[i]*100, round(split.use), "Large",large.est,round(large.est.adj[1]),
                            round(large.est.adj[2])) ) 
  
  
  
  
# small estimate
  
  small.rel <-  cyi.asl.filtered %>% filter(LENGTH_MILLIMETERS < split.use) # number of unique tag ID in release file 
  small.rec <-  length(unique(catch.rec$TagID[catch.rec$TagID %in% small.rel$SPAGHETTI_TAG_NO])) # number of unique tag ID in catch
  small.um <-   (pooled.inputs$u2 * plevels[i]) - small.rec # (total CdnCommCt * Plevel) - tag recoveries
  
  small.inputs <-  list(n1= length(unique(small.rel$SPAGHETTI_TAG_NO)) , 
                        m2 = small.rec, 
                        u2 = small.um)
  
  small.est<- SimplePetersenMod(n1 = small.inputs$n1, m2 = small.inputs$m2 , u2 = small.inputs$u2) 
  small.est.adj <- dropout.adj(Abd=small.est[,"est"],
                               SE_Abd =small.est[,"se"] ,
                               tags_dropped =13, 
                               tags_total = 51)
  
  pooled.test.df <- rbind(pooled.test.df,
                          c(yr.do, plevels[i]*100, round(split.use), "Small",small.est,round(small.est.adj[1]),
                            round(small.est.adj[2])) ) 
  
  
  
  # calculate sum of large and small
   pooled.test.df <- as.data.frame(rbind(pooled.test.df,
                          c(yr.do, plevels[i]*100, round(split.use), 
                            "SumSize",
                            n1 = NA, m2 = NA, u2 = NA,
                            est = round(large.est[,"est"] + small.est[,"est"]),
                            se = round(sqrt((large.est[,"se"] + small.est[,"se"])^2)),
                            est.adj = round(large.est.adj[1] + small.est.adj[1]),
                            se.adj = round(sqrt((large.est.adj[2] + small.est.adj[2])^2))                            
                          )) )

  
} # end looping through plevels

pooled.test.df <- pooled.test.df[-1,] # remove initial placeholder row


# why is this now all character?
# fiddled a bit, but quickes way is to read in from csv...
write.csv(pooled.test.df,"PostSeason_SizeStratEst/OUTPUT/SizeStratified_Estimates.csv",row.names = FALSE)

pooled.test.df <- read.csv("PostSeason_SizeStratEst/OUTPUT/SizeStratified_Estimates.csv",
                           stringsAsFactors = FALSE)
str(pooled.test.df )



# add +- 2SE 
pooled.test.df <- pooled.test.df %>%
                  mutate(lower.adj =  est.adj - 2*se.adj,  # why does 2* not work?
                         upper.adj =  est.adj + 2*se.adj   ) 
# overwrite with the updated version
write.csv(pooled.test.df,"PostSeason_SizeStratEst/OUTPUT/SizeStratified_Estimates.csv",row.names = FALSE)


# basic diagnostic plot



png(filename = "PostSeason_SizeStratEst/OUTPUT/SizeStrat_SummaryPlot.png",
    width = 480*4, height = 480*3.7, units = "px", pointsize = 14*2.8, bg = "white",  res = NA)

est.forplot <- pooled.test.df %>% filter(EstType =="SumSize")


plot(1:5,1:5, type="n",xlim = c(0,100),ylim = c(0,max(est.forplot$upper.adj)),
     xlab = "p-level", ylab = "Estimate (Adjusted for Dropout)",bty="n",axes=FALSE)


# non-stratified
all.est <- pooled.test.df %>% filter(EstType =="All")
points(0,all.est$est.adj,col="red",pch=15)
segments(0,all.est$lower.adj,0,all.est$upper.adj,col="red")
abline(h=all.est$est.adj,col="red",lty=1)

lines(est.forplot$plevel,est.forplot$est.adj,col="darkblue")
segments(as.numeric(est.forplot$plevel),est.forplot$lower.adj,
         as.numeric(est.forplot$plevel),est.forplot$upper.adj,col = "darkblue")
points(est.forplot$plevel,est.forplot$est.adj,col="darkblue",pch=21,bg="white")


axis(1)
axis(2,pretty(c(0,max(est.forplot$upper.adj))))


legend("bottomleft",legend=c("Pooled","Size Strat"),pch=c(15,21),col=c("red","darkblue"),bg="white",bty="n")

dev.off()






