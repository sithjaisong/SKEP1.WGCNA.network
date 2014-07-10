###############################################################################
#'title         : Load survey data on SKEP Phase I
#'date          : July, 2014
#'purpose       : load data from the shared Google Drive which is the exel 
#'                format
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : import excel file from the shared files and delete the 
#'                samples with NA and set the class of varibles of data set in 
#'                the right class of varible
#'output        : data frame and RData 
###############################################################################

#####-- Load Package --#####
library(XLConnect)

# set option
options(stringsAsFactors = FALSE) # data frame created after executing that line will not auto-convert to factors

######-- Step 1: Loading Data --#####
# load data of Survey data of SKEP Phase 1 

file <- list.files(path = paste("/Users/iSith/Google Drive/SKEP1project/data.file/"), pattern = ".xlsx$", full.names = TRUE) # list all file with xlsx format.

file # show all exel file in this folder

### Load the data work sheet exel file
workbook <- loadWorkbook(file[3]) # select the file name : SKEP1Survey.xlsx

survey <- readWorksheet(workbook, sheet = 2) # # select the data in sheet no 2

# Check the names of the variables 

names(survey) <- tolower(names(survey)) # lower case names of variables

ds <- subset(survey, select = -c(ced, nplsqm, wcp)) # delete column named ced because there is no data in those column

ds <- ds[complete.cases(ds),] # delete the rows that contain NA 

str(ds) # check the data

# set the the class of variable 

ds$fno <- as.character(ds$fno)
ds$identifier <- as.character(ds$identifier)
ds$country <- as.factor(ds$country)
ds$year <- as.factor(ds$year)
ds$season <- as.factor(ds$season)
ds$latitude <- as.character(ds$latitude)
ds$longitude <- as.character(ds$longitude)
ds$latituderev <- as.character(ds$latituderev)
ds$longituderev <- as.character(ds$longituderev)
ds$village <- as.character(ds$village)
ds$fa <- as.numeric(ds$fa)
ds$fn <- as.character(ds$fn)
ds$lfm <- as.factor(ds$lfm)
ds$pc <- as.factor(ds$pc)
ds$fp <- as.numeric(ds$fp)
ds$cem <- as.factor(ds$cem)
ds$ast <- as.numeric(ds$ast)
ds$nplsqm <- as.numeric(ds$nplsqm)
ds$ced <- as.numeric(ds$ced)
ds$cedjul <- as.numeric(ds$cedjul)
ds$hd <- as.Date(ds$hd)
ds$hdjul <- as.numeric(ds$hdjul)
ds$ccd <- as.numeric(ds$ccd)
ds$cvr <- as.factor(ds$cvr)
ds$vartype <- as.factor(ds$vartype)
ds$varcoded <- as.factor(ds$varcoded)
ds$fym <- as.factor(ds$fym)
ds$fym.coded <- as.factor(ds$fym.coded)
ds$n <- as.numeric(ds$n)
ds$p <- as.numeric(ds$p)
ds$k <- as.numeric(ds$k)
ds$mf <- as.numeric(ds$mf)
ds$wcp <- as.factor(ds$wcp)
ds$iu <- as.numeric(ds$iu)
ds$hu <- as.numeric(ds$hu)
ds$fu <- as.numeric(ds$fu)
ds$cs <- as.factor(ds$cs)
ds$cs.coded <- as.factor(ds$cs.coded)
ds$ldg <- as.numeric(ds$ldg)
ds$yield <- as.numeric(ds$yield)
ds$dvs1 <- as.factor(ds$dvs1)
ds$dvs2 <- as.factor(ds$dvs2)
ds$ws1 <- as.factor(ds$ws1)
ds$ws2 <- as.factor(ds$ws2)
ds$dswe1 <- as.factor(ds$dswe1)
ds$dswe2 <- as.factor(ds$dswe2)
ds$dscum <- as.factor(ds$wecum)
ds$wecum <- as.factor(ds$wecum)
ds$ntmax <- as.factor(ds$ntmax)
ds$np <- as.numeric(ds$np)
ds$nltmax <- as.numeric(ds$nltmax)
ds$nlhmax <- as.numeric(ds$nlhmax)
ds$wa <- as.numeric(ds$wa)
ds$wb <- as.numeric(ds$wb)
ds$dh <- as.numeric(ds$dh)
ds$wh <- as.numeric(ds$wh)
ds$gm <- as.numeric(ds$gm)
ds$rt <- as.numeric(ds$rt)
ds$wm <- as.numeric(ds$wm)
ds$lf <- as.numeric(ds$lf)
ds$de <- as.numeric(ds$de)
ds$bph <- as.numeric(ds$bph)
ds$wbp <- as.numeric(ds$wbp)
ds$aw <- as.numeric(ds$aw)
ds$rb <- as.numeric(ds$rb)
ds$rbb <- as.numeric(ds$rbb)
ds$glh <- as.numeric(ds$glh)
ds$blb <- as.numeric(ds$blb)
ds$lb <- as.numeric(ds$lb)
ds$bs <- as.numeric(ds$bs)
ds$bls <- as.numeric(ds$bls)
ds$nbs <- as.numeric(ds$nbs)
ds$rs <- as.numeric(ds$rs)
ds$ls <- as.numeric(ds$ls)
ds$shb <- as.numeric(ds$shb)
ds$shr <- as.numeric(ds$shr)
ds$sr <- as.numeric(ds$sr)
ds$fsm <- as.numeric(ds$fsm)
ds$nb <- as.numeric(ds$nb)
ds$dp <- as.numeric(ds$dp)
ds$rtd <- as.numeric(ds$rtd)
ds$rsd <- as.numeric(ds$rsd)
ds$gsd <- as.numeric(ds$gsd)
ds$hb <- as.numeric(ds$hb)
ds$bu <- as.numeric(ds$bu)
ds$mu <- as.numeric(ds$mu)
ds$pghu <- as.numeric(ds$pghu)
ds$stb <- as.numeric(ds$stb)
ds$def <- as.numeric(ds$def)

## save the processed data to the survey.data.RData
save(ds, file = "survey.data.RData")

#eos