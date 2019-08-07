# Weekly Settings
fw.stat.weeks <- 23:32

# stat weeks with releases and recoveries to  be included
Year<-2019 # input year
year.subfolder <- "2019_inseason" #subfolder for forecast through week

# use this to specify a subfolder for this week (in the "data" and "output" folders)
sw.subfolder <- "SW32" 
sw.randomseed <- 2332

data.directory <-file.path('data','2019_inseason',sw.subfolder)
if(!dir.exists(file.path("output",year.subfolder))){dir.create(file.path("output",year.subfolder))}
if(!dir.exists(file.path("output",year.subfolder, sw.subfolder))){dir.create(file.path("output",year.subfolder, sw.subfolder))}

# load libraries
devtools::install_github("cschwarz-stat-sfu-ca/BTSPAS", dependencies = TRUE, build_vignettes = FALSE) #only load once then comment out

# check if the URL exists (i.e. have internet connection and correct URL)
library(RCurl)
url.check <- url.exists("https://raw.githubusercontent.com/cschwarz-stat-sfu-ca/taku/master/FUNCTIONS_BTSPAS_Wrappers.R")

# load functions from url or local source
if(url.check){
  library(devtools)
  devtools::source_url("https://raw.githubusercontent.com/cschwarz-stat-sfu-ca/taku/master/FUNCTIONS_BTSPAS_Wrappers.R")
}

if(!url.check){ source("../CODE/Local Copies of Carl Functions/FUNCTIONS_BTSPAS_Wrappers.R") }
library(BTSPAS) 
library(ggplot2)
library(lubridate)
library(fs)
library(devtools)
library(rjags)
library(cellranger)
library(readxl)



# load data and ensure variable names match
read.csv(file.path(data.directory,'release_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> release
release$ReleaseDate <- lubridate::mdy(release$ReleaseDate)


dim(release)
release <- release[ !is.na(release$ReleaseDate),]
dim(release)
head(release)

read.csv(file.path(data.directory,'recovery_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> recap
recap$RecoveryDate <- lubridate::mdy(recap$RecoveryDate)  # *** CJS *** careful of data formats
recap$RecoveryType <- "Commercial"
head(recap)

read.csv(file.path(data.directory,'catch_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> catch
head(catch)

catch$Date <- lubridate::mdy(catch$Date)

catch <- plyr::rename(catch, c("Date"="RecoveryDate",
                               "StatWeek"="RecoveryStatWeek",
                               "CdnCommCt"="CatchWithTags"))
catch$RecoveryType="Commercial"
head(catch)

xtabs(~ReleaseStatWeek,  data=release,exclude=NULL, na.action=na.pass)
xtabs(~RecoveryStatWeek, data=recap,  exclude=NULL, na.action=na.pass)
xtabs(CatchWithTags~RecoveryStatWeek, data=catch,  exclude=NULL, na.action=na.pass)

# Figure out the half week strata
# Divide Sunday -> Wednesday as the first half= Commercial Opening and Thursday -> Saturday as the second half
strata <- rbind( 
  plyr::rename(release, c("ReleaseStatWeek"="StatWeek",
                          "ReleaseDate"="Date"))[,c("Year","Date","StatWeek")],
  plyr::rename(recap,   c("RecoveryStatWeek"="StatWeek",
                          "RecoveryDate"="Date"))[,c("Year","Date","StatWeek")],
  plyr::rename(catch,   c("RecoveryStatWeek"="StatWeek",
                          "RecoveryDate"="Date"))[,c("Year","Date","StatWeek")])
strata <- unique(strata[,c("Year","Date","StatWeek")])

strata <- strata[ order(strata$Date),]
# Get the day of the week,
strata$dow <- lubridate::wday(strata$Date,week_start=7)

# Days 1-4 (Sunday -> Wed) = first half of week = Commercial Opening
# Days 5-7 (Thurs  -> Sat) = last half of week
strata$HalfStatWeek <- strata$StatWeek + .1 +
  .1*strata$dow %in% c(5,6,7)
head(strata)

# merge back the strata halfweek back to release, recovery, and commercial catch
dim(release)
release <- merge(release, 
                 plyr::rename(strata, c("Date"="ReleaseDate","HalfStatWeek"="ReleaseHalfStatWeek"))
                 [,c("ReleaseDate","ReleaseHalfStatWeek")],
                 all.x=TRUE)
dim(release)

dim(recap)
recap <- merge(recap, 
               plyr::rename(strata,  c("Date"="RecoveryDate", "HalfStatWeek"="RecoveryHalfStatWeek"))
               [,c("RecoveryDate","RecoveryHalfStatWeek")],
               all.x=TRUE)
dim(recap)

# but a recap that occurs in the second half of the week is assumed to have occurred
# in the commerical opening. Change the data as needed for the half week analysis; likely to be year specific
xtabs(~RecoveryHalfStatWeek, data=recap)

select <- (recap$RecoveryHalfStatWeek %% 1) > .15  # recoveries in second half of week
sum(select)
xtabs(~RecoveryHalfStatWeek, data=recap[select,])
recap$RecoveryHalfStatWeek[select] <- recap$RecoveryHalfStatWeek[select] - .1 # shift back to the opening

xtabs(~RecoveryHalfStatWeek, data=recap)


dim(catch)
catch <- merge(catch, 
               plyr::rename(strata, c("Date"="RecoveryDate","HalfStatWeek"="RecoveryHalfStatWeek"))
               [,c("RecoveryDate","RecoveryHalfStatWeek")],
               all.x=TRUE)
dim(catch)

# merge the recaptures with the releases; check that recapture tag numbers match
setdiff(recap$TagID, release$TagID)

dim(release)
relrecap <- merge(release, recap, all.x=TRUE)
dim(relrecap)

# Generate the inseason estimate
# Give the list the StatWeeks that should be included in the estimate
xtabs(~ReleaseStatWeek,  data=relrecap)
xtabs(~RecoveryStatWeek, data=relrecap)
xtabs(CatchWithTags~RecoveryStatWeek, data=catch) 


# Full Week BTSPAS analysis
# Define the stratum variable as 1 = first stat week, 2=second stat week etc
# THIS IS WHERE YOU SELECT THE STAT WEEKS FROM YOUR DATA SET TO MAKE THE ESTIMATES
# REFER TO THE XTABS() JUST ABOVE FOR SOME HELP IN MAKING THE DECISION
fw.stratum.index <- data.frame(stratum.index=1:length(fw.stat.weeks),
                               stratum.label=as.character(fw.stat.weeks),
                               stringsAsFactors=FALSE)
fw.stratum.index


# get the data necessary for the call to BTSPAS
fw.data <- BTSPAS_input(relrecap, catch, "ReleaseStatWeek", "RecoveryStatWeek",
                        fw.stratum.index, catch.var="CatchWithTags")

# fit the BTSPAS model
fw.prefix <- paste("Taku-FullWeek-Inseason-W",round(min(fw.stat.weeks)),
                   "-W",round(max(fw.stat.weeks)),"-",sep="")

fit.BTSPAS(fw.data,prefix=fw.prefix, add.ones.at.start=TRUE, InitialSeed=sw.randomseed)

# fit the BTSPAS model with fall back (say n=50, x=11)
fw.prefix.dropout <- paste("Taku-FullWeek-Inseason-W",round(min(fw.stat.weeks)),
                           "-W",round(max(fw.stat.weeks)),"-fallback-",sep="")

fit.BTSPAS.dropout(fw.data,prefix=fw.prefix.dropout, n=50, dropout=11, add.ones.at.start=TRUE, InitialSeed=sw.randomseed)

# Half Week BTSPAS analysis
# Define the stratum variable as 1 = first stat week, 2=second stat week etc
hw.stat.weeks <- sort(as.vector(outer(fw.stat.weeks, c(.1,.2), "+")))  # releases and recoveries 
hw.stat.weeks

hw.stratum.index <- data.frame(stratum.index=1:length(hw.stat.weeks),
                               stratum.label=as.character(hw.stat.weeks),
                               stringsAsFactors=FALSE)
hw.stratum.index

# get the data necessary for the call to BTSPAS
hw.data <- BTSPAS_input(relrecap, catch, "ReleaseHalfStatWeek", "RecoveryHalfStatWeek",
                        hw.stratum.index, catch.var="CatchWithTags")

# fit the BTSPAS model
hw.prefix <- gsub("FullWeek","HalfWeek",fw.prefix)
fit.BTSPAS(hw.data,prefix=hw.prefix, add.ones.at.start=TRUE, InitialSeed=sw.randomseed)

# fit the BTSPAS model with fall back (say n=50, x=11)
hw.prefix.dropout <- gsub("FullWeek","HalfWeek",fw.prefix.dropout)
fit.BTSPAS.dropout(hw.data,prefix=hw.prefix.dropout, n=50, dropout=11, add.ones.at.start=TRUE, InitialSeed=sw.randomseed)

# Make a table of the estimates from the various sets of weeks etc
# Extract the results from the various fits
file.names <-dir()

# Extract the directories with the fits
file.names.fits<- file.names[grepl(paste("^Taku-"), file.names)]
file.names.fits

# make a pdf file of the fitted curves
prefix <- paste("Taku-Inseason-W",round(min(fw.stat.weeks)),
                "-W",round(max(fw.stat.weeks)),"-",sep="")

pdf(file.path("output", year.subfolder, sw.subfolder, "/",paste(prefix,"-Inseason_fits.pdf",sep="")))

plyr::l_ply(file.names.fits, function(x){
  cat("Extracting final plot from ", x, "\n")
  load(file.path(x, "taku-fit-tspndenp-saved.Rdata"))
  tryCatch(plot(taku.fit.tspndenp$plots$fit.plot),
           error=function(cond){
             message("Unable to draw plot - convergence error?- skipped")
             return(NULL)
           })
})
dev.off()

# Extract all of the estimates of the total run size
run.size <- plyr::ldply(file.names.fits, function(x){
  cat("Extracting total run size from ", x, "\n")
  load(file.path(x, "taku-fit-tspndenp-saved.Rdata"))
  Ntot <- taku.fit.tspndenp$summary["Ntot",]
  if(is.null(Ntot))Ntot <- rep(NA,9) # if model didn't converge properly
  #browser()
  Ntot <- as.data.frame(t(Ntot))
  Ntot[,1:7] <- round(Ntot[,1:7])
  Ntot$file=x
  Ntot
})
run.size

write.csv(run.size, file.path("output", year.subfolder, sw.subfolder, "/", paste(prefix,"-Inseason_runsize.csv",sep="")), row.names=TRUE)


# Extract the Petersen estimators
# Extract all of the estimates of the total run size
run.pet.size <- plyr::ldply(file.names.fits, function(x){
  cat("Extracting Petersen from ",x,"\n")
  load(file.path(x, "taku-fit-tspndenp-saved.Rdata"))
  Year <- as.numeric(substring(x, 2+regexpr('--',x,fixed=TRUE)))
  #browser()
  Ntot.pp <- taku.fit.tspndenp$PP$using.all.data
  if(is.null(Ntot.pp$N.se))Ntot.pp <- data.frame(N.est=NA, N.se=NA)
  # see if this included fall back
  Ntot.pp.fallback <- taku.fit.tspndenp$PP$using.all.data.fallback
  if(is.null(Ntot.pp.fallback$N.se))Ntot.pp.fallback <- data.frame(N.est=NA, N.se=NA)
  
  c(Ntot.pp.est=round(Ntot.pp$N.est), Ntot.pp.se=round(Ntot.pp$N.se), 
    Ntot.pp.fallback.est=round(Ntot.pp.fallback$N.est), Ntot.pp.fallback.se=round(Ntot.pp.fallback$N.se),
    file=x)
})
run.pet.size

write.csv(run.pet.size,file.path("output",year.subfolder, sw.subfolder, "/", paste(prefix,"-Inseason_PP_runsize.csv",sep="")), row.names=TRUE)


#move files to correct directory
taku.prefix <- paste(fw.prefix,"-",Year, sep="")
files_old <- paste0(getwd(), "/", taku.prefix)

files_new <- paste0(getwd(), "/output/",year.subfolder, "/", sw.subfolder, "/", taku.prefix)

file_move(files_old, files_new)

taku.prefix <- paste(fw.prefix.dropout,"-",Year, sep="")
files_old <- paste0(getwd(), "/", taku.prefix)

files_new <- paste0(getwd(), "/output/",year.subfolder, "/", sw.subfolder, "/", taku.prefix)

file_move(files_old, files_new)

taku.prefix <- paste(hw.prefix,"-",Year, sep="")
files_old <- paste0(getwd(), "/", taku.prefix)

files_new <- paste0(getwd(), "/output/",year.subfolder, "/", sw.subfolder, "/", taku.prefix)

file_move(files_old, files_new)

taku.prefix <- paste(hw.prefix.dropout,"-",Year, sep="")
files_old <- paste0(getwd(), "/", taku.prefix)

files_new <- paste0(getwd(), "/output/",year.subfolder,"/", sw.subfolder, "/", taku.prefix)

file_move(files_old, files_new)

