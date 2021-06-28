rm(list=ls())
options(scipen=999)
# set work directory
setwd(dir = "~/Dropbox/India 2019/")

# # #
# # # Some useful functions:
# # #

library(missRanger)
library(foreach)
library(R2jags)
library(arm)
library(ranger)
library(data.table)
library(parallel)
library(grDevices)
library(plyr)
library(xtable)
library(foreign)
library(dplyr)
library(xtable)
library(anesrake)
library(lwgeom)
library(sf)
is.min = function(x){x==min(x)}
is.neg = function(x){x<0}

# Sample function is useful but buggy - if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}
# 

ultraparameters = list(source=c("AMechTurk","SubjectPool","Both")[3],
                       turnout_2014 = c(TRUE,FALSE)[1],
                       sample_prop_census = 0.005,
                       num.trees_survey_imp = 1000,
                       num.trees_sf_imp = 1000
 )
# save(ultraparameters,file = "Generated Quantities/ultraparameters.RData",compress = TRUE)
load(file = "Generated Quantities/ultraparameters.RData")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Building the Stratification Frame
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # #
# # # 1) Load large representative survey of the Indian population; this will be the backbone of the stratification frame;
# # # the IHDS is wide but short: extensive details on categories of interest, but only 135,984 observations.
# # #

IHDS_clean = fread("Generated Quantities/IHDS_clean.csv",na.strings = c("",NA))

# # #
# # # 2) Load Census data and sample a few millions elements to ensure we have most state-level categories of interest in frame;
# # # this is a response to the fact that some very small categories (some whole small states) are not present in the IHDS,
# # # whilst also increasing the precision of estimates available in both IHDS and Census. 
# # # Note that we use the widest possible Census which allows for common variables across IHDS and Census for consistency; 
# # # sadly these Census counts do not include caste, becase the caste-Census categorizes case differently from the IHDS. 
# # #

# load census
CENSUS_counts_clean = read.csv("Generated Quantities/CENSUS_counts_clean.csv",na.strings = c("",NA))

colnames(CENSUS_counts_clean)
# sample from census 
# sample_CENSUS = data.table(
#   CENSUS_counts_clean[
#     sample(1:dim( CENSUS_counts_clean)[1],
#            size = ultraparameters$sample_prop_census*sum(CENSUS_counts_clean$totcount), # 1% is 7 million people; go for 1/2 percent 
#            replace = TRUE,
#           prob =  CENSUS_counts_clean$totcount/sum( CENSUS_counts_clean$totcount)),])
# # remove totcount - this sample is at the individual level, don't need totcount of the category
# sample_CENSUS = sample_CENSUS[,-which(names(sample_CENSUS)=="totcount"),with = FALSE]

# save(sample_CENSUS,file = 'Generated Quantities/sample_CENSUS.RData',compress= TRUE)
load(file = 'Generated Quantities/sample_CENSUS.RData')
# # #
# # # 3) Load National Election Study to augment individual-level data to include Political variables.
# # #

# load political frame
NES14_clean = read.csv("Generated Quantities/NES14_clean.csv",na.strings = c("",NA))
# clean states - we pretend telangana hasn't yet split from anhra pradesh to make compatibility across datasets easier
NES14_clean$states = as.factor(as.character(unlist(NES14_clean$states )))
levels(NES14_clean $states)[which(levels(NES14_clean $states)=="(36) Telangana 36")]="(28) Andhra Pradesh 28"

# # #
# # # 5-TEMP) load HIST early, will re-load later. need it to re-weight NES14
# # #

# lpload state-level and national-level historical data on election results and turnout 
HIST = read.csv("Generated Quantities/HIST.csv",na.strings = c("",NA))
# load zone-level historical data on election results and turnout 
HIST_Zones = read.csv('Generated Quantities/HIST_Zones.csv',na.strings = c("",NA))
# merge the two for a complete picture
HIST = merge(HIST,HIST_Zones,by = c("Year","Zones"),all=TRUE)
# recode the zones so they match the zone coding in the targets above
HIST$Zones = as.factor(as.character(unlist(HIST$Zones)))
levels(HIST$Zones)[which(levels(HIST$Zones)=="Eastern")] = "(04) Eastern 04"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North")] = "(01) North 01"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North-Central")] = "(02) North-Central 02"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North-Eastern")] = "(03) North-Eastern 03"
levels(HIST$Zones)[which(levels(HIST$Zones)=="Southern")] = "(06) Southern 06"
levels(HIST$Zones)[which(levels(HIST$Zones)=="Western")] = "(05) Western 05"

# focus on 2014 (goes back 2 lags);
# it's called "AV_ST" but it's the correct of district-level results weighted by voting population, not the average ;just a strange naming convention.
HIST_ST <- unique(HIST[HIST$Year==2014,  c( "State","Zones",
                                            "AV_ST_Vote_Share_Percentage.NDA",       
                                            "AV_ST_Vote_Share_Percentage.UPA",        
                                            "AV_ST_Vote_Share_Percentage.OTHER",      
                                            "AV_ST_Turnout_Percentage",              
                                            "AV_LAG_ST_Vote_Share_Percentage.NDA",    
                                            "AV_LAG_ST_Vote_Share_Percentage.UPA",    
                                            "AV_LAG_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG_ST_Turnout_Percentage",           
                                            "AV_LAG2_ST_Vote_Share_Percentage.NDA",   
                                            "AV_LAG2_ST_Vote_Share_Percentage.UPA",  
                                            "AV_LAG2_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG2_ST_Turnout_Percentage",          
                                            "AV_LAG3_ST_Vote_Share_Percentage.NDA",  
                                            "AV_LAG3_ST_Vote_Share_Percentage.UPA",   
                                            "AV_LAG3_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG3_ST_Turnout_Percentage",
                                            
                                            "Z_Vote_Share_Percentage.NDA",       
                                            "Z_Vote_Share_Percentage.UPA",        
                                            "Z_Vote_Share_Percentage.OTHER",      
                                            "Z_Turnout_Percentage",              
                                            "LAG_Z_Vote_Share_Percentage.NDA",    
                                            "LAG_Z_Vote_Share_Percentage.UPA",    
                                            "LAG_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG_Z_Turnout_Percentage",           
                                            "LAG2_Z_Vote_Share_Percentage.NDA",   
                                            "LAG2_Z_Vote_Share_Percentage.UPA",  
                                            "LAG2_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG2_Z_Turnout_Percentage",          
                                            "LAG3_Z_Vote_Share_Percentage.NDA",  
                                            "LAG3_Z_Vote_Share_Percentage.UPA",   
                                            "LAG3_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG3_Z_Turnout_Percentage"
)])
# any missing should be turned to 0, as it means the party wasn't running there;
HIST_ST = as.data.table(apply( HIST_ST,2,function(x){ifelse(is.na(x),0,x)}))
# add "_2014" at the end of these variabes, it's useful to recognize them 
names(HIST_ST)[-which(names(HIST_ST)=="State"|names(HIST_ST)=="Zones")] = paste(names(HIST_ST)[-which(names(HIST_ST)=="State"|names(HIST_ST)=="Zones")],"_2014" ,sep="")
# and lower the names 
names(HIST_ST) = tolower(names(HIST_ST))
# and change the 'state' name to 'states, to match the targets
names(HIST_ST)[grep("state",names(HIST_ST))] = "states"
# finally, ensure all the numeric variables are indeed numeric, and the state and zones are factors
HIST_ST = data.table(HIST_ST[, lapply(.SD,function(x){as.factor(x)}),.SDcols  = c("states","zones")],HIST_ST[,lapply(.SD,function(x){as.numeric(as.character(unlist(x)))}),.SDcols = names(HIST_ST)[-which(names(HIST_ST)=="states"|names(HIST_ST)=="zones")]])

#library(caret)
HIST_ST = cbind(HIST_ST[,c("states","zones"),with = FALSE],
                HIST_ST[,-c("states","zones"),with = FALSE]#[,-findCorrelation(cor(HIST_ST[,-c("states","zones")]), cutoff = 0.95, verbose = TRUE,exact = TRUE),with = FALSE]
)
# # # assign a variable to re-weight - remove this after 
NES14_clean$TEMP_alliance_vote =as.factor(ifelse(NES14_clean$turnout==0,'stayed home',as.character(unlist(NES14_clean$alliance_vote))))
# # # complete NES just for the raking - will use incomplete in multiple imp.
# NES14_clean_misstemp = missRanger::missRanger(data = NES14_clean,pmm.k = 30,maxiter = 30,verbose = TRUE,returnOOB = TRUE)
#  save(NES14_clean_misstemp,file = 'Generated Quantities/NES14_clean_misstemp.RData',compress= TRUE)
load(file = 'Generated Quantities/NES14_clean_misstemp.RData')
# # # 

NES14_clean_list = list()
for(ST in levels(NES14_clean$states)){
# state-level data 
if(sum(NES14_clean$states==as.character(unlist(ST)))>0){
NES14_clean_state = NES14_clean_misstemp [which(NES14_clean_misstemp $states==as.character(unlist(ST))),]
NES14_clean_state$TEMP_alliance_vote = as.factor(as.character(unlist(NES14_clean_state$TEMP_alliance_vote)))
# get electorate size - multiply by turnout 
Electorate = sum(CENSUS_counts_clean$totcount[CENSUS_counts_clean$states==ST])
Turnout = Electorate*HIST_ST$av_st_turnout_percentage_2014[HIST_ST$states==ST]/100
Home = Electorate*(100-HIST_ST$av_st_turnout_percentage_2014[HIST_ST$states==ST])/100
NDA_Counts = Turnout*HIST_ST$av_st_vote_share_percentage.nda_2014[HIST_ST$states==ST]/100
UPA_Counts = Turnout*HIST_ST$av_st_vote_share_percentage.upa_2014[HIST_ST$states==ST]/100
OTHER_Counts = Turnout*HIST_ST$av_st_vote_share_percentage.other_2014[HIST_ST$states==ST]/100
# setup weights
target_weights = list(
  TEMP_alliance_vote =         
    weights::wpct(x = c("NDA","UPA","OTHER","stayed home"),
                  weight = c(NDA_Counts/Electorate,
                             UPA_Counts/Electorate,
                             OTHER_Counts/Electorate,
                             Home/Electorate)
                  )
  )

target_weights$TEMP_alliance_vote = 
  target_weights$TEMP_alliance_vote[which(names(target_weights$TEMP_alliance_vote) %in% levels(NES14_clean_state$TEMP_alliance_vote))]
# assign weights to match past-vote 
raking <- anesrake(inputter = target_weights,
                   dataframe = NES14_clean_state,
                   caseid = 1:dim(NES14_clean_state)[1],
                   maxit = 10000,
                   cap =  10,   # Maximum allowed weight per iteration - how much can we up-vote a given guy?
                   choosemethod = "total",# How are parameters compared for selection?#
                   type = "pctlim",# What selection criterion is used?
                   verbose = TRUE,                 
                   pctlim = 0 #percent  # Threshold for selection
)
# append to list 
NES14_clean_list <- append(NES14_clean_list,list(cbind(NES14_clean_state,weight = raking$weightvec)))
} }
# save(NES14_clean_list,file = 'Generated Quantities/NES14_clean_list.RData')
load(file = 'Generated Quantities/NES14_clean_list.RData')
# make into dataset again 
NES14_clean_temp = do.call('rbind',NES14_clean_list)

# take bootstrap sample with new weights
bootstrap = sample(x = 1:dim(NES14_clean_temp)[1],size = dim(NES14_clean_temp)[1],replace = TRUE,prob = NES14_clean_temp$weight)
# save(bootstrap,file = 'Generated Quantities/bootstrap_NES.RData',compress = TRUE)
load(file = 'Generated Quantities/bootstrap_NES.RData')
NES14_clean_bootsrap = NES14_clean[bootstrap,]
NES14_clean = as.data.table(NES14_clean_bootsrap)
# rmeove temporary variable 
NES14_clean = NES14_clean[,!"TEMP_alliance_vote"]
## unify turnout and vote choice in 2014 
#NES14_clean$alliance_vote = ifelse(NES14_clean$turnout==0,"stayed home",NES14_clean$alliance_vote)
## remove turnout after consolidation
#NES14_clean = NES14_clean[,-which(names(NES14_clean)=="turnout"),with = FALSE]

# # #
# # # 4) Bind the Individual-Level survey, the Census sample and the National Election Study.
# # #

# bind these together
targets <- bind_rows(IHDS_clean,sample_CENSUS,NES14_clean)


print(
xtable(x = bind_rows(
  cbind(Source = "IHDS",
        Source_ID = c(1:dim(IHDS_clean)[1])[c(1:5,(dim(IHDS_clean)[1] - 4):dim(IHDS_clean)[1])],
        IHDS_clean[c(1:5,(dim(IHDS_clean)[1] - 4):dim(IHDS_clean)[1]),]),
  cbind(Source = "Census",
        Source_ID = c(1:dim(sample_CENSUS)[1])[c(1:5,(dim(sample_CENSUS)[1] - 4):dim(sample_CENSUS)[1])],
        sample_CENSUS[c(1:5,(dim(sample_CENSUS)[1] - 4):dim(sample_CENSUS)[1]),]),  
  cbind(Source = "NES",
        Source_ID = c(1:dim(NES14_clean)[1])[c(1:5,(dim(NES14_clean)[1] - 4):dim(NES14_clean)[1])],
        NES14_clean[c(1:5,(dim(NES14_clean)[1] - 4):dim(NES14_clean)[1]),])
  )),
include.rownames=FALSE)

# redefine each column as factor to incorporate new levels
targets <- targets [,lapply(.SD,function(x){as.factor(as.character(unlist(x)))}),.SDcols = names(targets)]

# # #
# # # 4-b) make state/zone-level covariates from census
# # #
library(mltools)
demos_CENSUS = c("rurality","age_cat","education_level","gender")
demos_IHDS = c("family_role","marital_status","religion","literacy","income_level","jati")
# # # CENSUS
# states
temp = 
  as.data.table(
    cbind(
      CENSUS_counts_clean[,c("states","zones")],
      model.matrix(~rurality -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~age_cat -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~education_level -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~gender -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")]
    ) )
temp_census = 
  temp[,lapply(.SD,function(x){sum(x)}),
       by = c("states","zones"),
       .SDcols = colnames(temp[,!c("zones","states")])]
temp_census = cbind(temp_census[,c("states","zones")],
                    temp_census[,!c("states","zones")]/rowSums(temp_census[,c("rurality(0) rural 0","rurality(1) urban 1")])
)
# zones
temp = 
  as.data.table(
    cbind(
      model.matrix(~rurality -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~age_cat -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~education_level -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")],
      model.matrix(~gender -1,data = CENSUS_counts_clean)*CENSUS_counts_clean[,c("totcount")]
    ) )
temp $zones = CENSUS_counts_clean[,c("zones")]

temp_census_zones = 
  temp[,lapply(.SD,function(x){sum(x)}),
       by = c("zones"),
       .SDcols = colnames(temp[,!c("zones")])]

temp_census_zones = cbind(temp_census_zones[,c("zones")],
                          temp_census_zones[,!c("zones")]/rowSums(temp_census_zones[,c("rurality(0) rural 0","rurality(1) urban 1")])
)
# # # IHDS
# states
temp = IHDS_clean[complete.cases(IHDS_clean),]
temp = 
  as.data.table(
    cbind(temp[,c("states","zones")],
          model.matrix(~family_role -1,data = temp),
          model.matrix(~marital_status -1,data = temp),
          model.matrix(~religion -1,data =temp),
          model.matrix(~literacy -1,data = temp),
          model.matrix(~income_level -1,data = temp),
          model.matrix(~jati -1,data = temp)
    )
  )
temp = temp[,lapply(.SD,function(x){sum(x)}),by = c("states","zones"),.SDcols = names(temp[,!c("states","zones")])]
temp_ihds = cbind(temp[,c("states","zones")],
                  temp[,!c("states","zones")]/rowSums(temp[,c("literacy(0) No 0","literacy(1) Yes 1")])
)
# zones
temp = IHDS_clean[complete.cases(IHDS_clean),]
temp = 
  as.data.table(
    cbind(temp[,c("zones")],
          model.matrix(~family_role -1,data = temp),
          model.matrix(~marital_status -1,data = temp),
          model.matrix(~religion -1,data =temp),
          model.matrix(~literacy -1,data = temp),
          model.matrix(~income_level -1,data = temp),
          model.matrix(~jati -1,data = temp)
    )
  )
temp = temp[,lapply(.SD,function(x){sum(x)}),by = c("zones"),.SDcols = names(temp[,!c("zones")])]
temp_ihds_zones = cbind(temp[,c("zones")],
                        temp[,!c("zones")]/rowSums(temp[,c("literacy(0) No 0","literacy(1) Yes 1")])
)

temp_demos_states = merge(temp_ihds,temp_census,by = c("states","zones"),all=TRUE)
colnames(temp_demos_states)[-which(colnames(temp_demos_states) %in% c("states","zones"))]=
  paste("states_",colnames(temp_demos_states)[-which(colnames(temp_demos_states) %in% c("states","zones"))],sep="")
temp_demos_zones = merge(temp_ihds_zones,temp_census_zones,by = c("zones"),all=TRUE)
colnames(temp_demos_zones)[-which(colnames(temp_demos_zones) %in% c("states","zones"))]=
  paste("zones_",colnames(temp_demos_zones)[-which(colnames(temp_demos_zones) %in% c("states","zones"))],sep="")


# couple of missing IHDS states, impute them with zones 
temp_demos = merge(temp_demos_zones,temp_demos_states,by = c("zones"),all = TRUE)
temp_demos$zones = as.factor(temp_demos$zones)
colnames(temp_demos) = make.names(colnames(temp_demos))
# # #
# # # 5) Load and clean area-level political variables - these can contribute to better estimates for the `missing' variables of the targets.
# # #

# lpload state-level and national-level historical data on election results and turnout 
HIST = read.csv("Generated Quantities/HIST.csv",na.strings = c("",NA))
# load zone-level historical data on election results and turnout 
HIST_Zones = read.csv('Generated Quantities/HIST_Zones.csv',na.strings = c("",NA))
# merge the two for a complete picture
HIST = merge(HIST,HIST_Zones,by = c("Year","Zones"),all=TRUE)
# recode the zones so they match the zone coding in the targets above
HIST$Zones = as.factor(as.character(unlist(HIST$Zones)))
levels(HIST$Zones)[which(levels(HIST$Zones)=="Eastern")] = "(04) Eastern 04"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North")] = "(01) North 01"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North-Central")] = "(02) North-Central 02"
levels(HIST$Zones)[which(levels(HIST$Zones)=="North-Eastern")] = "(03) North-Eastern 03"
levels(HIST$Zones)[which(levels(HIST$Zones)=="Southern")] = "(06) Southern 06"
levels(HIST$Zones)[which(levels(HIST$Zones)=="Western")] = "(05) Western 05"

# focus on 2014 (goes back 2 lags);
# it's called "AV_ST" but it's the correct of district-level results weighted by voting population, not the average ;just a strange naming convention.
HIST_ST <- unique(HIST[HIST$Year==2014,  c( "State","Zones",
                                            "AV_ST_Vote_Share_Percentage.NDA",       
                                            "AV_ST_Vote_Share_Percentage.UPA",        
                                            "AV_ST_Vote_Share_Percentage.OTHER",      
                                            "AV_ST_Turnout_Percentage",              
                                            "AV_LAG_ST_Vote_Share_Percentage.NDA",    
                                            "AV_LAG_ST_Vote_Share_Percentage.UPA",    
                                            "AV_LAG_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG_ST_Turnout_Percentage",           
                                            "AV_LAG2_ST_Vote_Share_Percentage.NDA",   
                                            "AV_LAG2_ST_Vote_Share_Percentage.UPA",  
                                            "AV_LAG2_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG2_ST_Turnout_Percentage",          
                                            "AV_LAG3_ST_Vote_Share_Percentage.NDA",  
                                            "AV_LAG3_ST_Vote_Share_Percentage.UPA",   
                                            "AV_LAG3_ST_Vote_Share_Percentage.OTHER", 
                                            "AV_LAG3_ST_Turnout_Percentage",
                                            
                                            "Z_Vote_Share_Percentage.NDA",       
                                            "Z_Vote_Share_Percentage.UPA",        
                                            "Z_Vote_Share_Percentage.OTHER",      
                                            "Z_Turnout_Percentage",              
                                            "LAG_Z_Vote_Share_Percentage.NDA",    
                                            "LAG_Z_Vote_Share_Percentage.UPA",    
                                            "LAG_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG_Z_Turnout_Percentage",           
                                            "LAG2_Z_Vote_Share_Percentage.NDA",   
                                            "LAG2_Z_Vote_Share_Percentage.UPA",  
                                            "LAG2_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG2_Z_Turnout_Percentage",          
                                            "LAG3_Z_Vote_Share_Percentage.NDA",  
                                            "LAG3_Z_Vote_Share_Percentage.UPA",   
                                            "LAG3_Z_Vote_Share_Percentage.OTHER", 
                                            "LAG3_Z_Turnout_Percentage"
)])
# any missing should be turned to 0, as it means the party wasn't running there;
HIST_ST = as.data.table(apply( HIST_ST,2,function(x){ifelse(is.na(x),0,x)}))
# add "_2014" at the end of these variabes, it's useful to recognize them 
names(HIST_ST)[-which(names(HIST_ST)=="State"|names(HIST_ST)=="Zones")] = paste(names(HIST_ST)[-which(names(HIST_ST)=="State"|names(HIST_ST)=="Zones")],"_2014" ,sep="")
# and lower the names 
names(HIST_ST) = tolower(names(HIST_ST))
# and change the 'state' name to 'states, to match the targets
names(HIST_ST)[grep("state",names(HIST_ST))] = "states"
# finally, ensure all the numeric variables are indeed numeric, and the state and zones are factors
HIST_ST = data.table(HIST_ST[, lapply(.SD,function(x){as.factor(x)}),.SDcols  = c("states","zones")],HIST_ST[,lapply(.SD,function(x){as.numeric(as.character(unlist(x)))}),.SDcols = names(HIST_ST)[-which(names(HIST_ST)=="states"|names(HIST_ST)=="zones")]])
HIST_ST = cbind(HIST_ST[,c("states","zones"),with = FALSE],
                HIST_ST[,-c("states","zones"),with = FALSE]
)
# # #
# # # 6) load distance matrix 
# # #

shape <- read_sf(dsn = "India_SHP/", layer = "INDIA",options = "ENCODING=UTF-8")

shape$ST_NAME[which(shape$ST_NAME=="ANDAMAN AND NICOBAR ISLANDS")] = "(35) Anadman/Nicobar 35"
shape$ST_NAME[which(shape$ST_NAME=="Andhra Pradesh")] = "(28) Andhra Pradesh 28"
shape$ST_NAME[which(shape$ST_NAME=="Arunachal Pradesh")] = "(12) Arunachal Pradesh 12"
shape$ST_NAME[which(shape$ST_NAME=="Assam")] = "(18) Assam 18"
shape$ST_NAME[which(shape$ST_NAME=="Bihar")] = "(10) Bihar 10"
shape$ST_NAME[which(shape$ST_NAME=="CHANDIGARH")] = "(04) Chandigarh 04"
shape$ST_NAME[which(shape$ST_NAME=="Chhattisgarh")] = "(22) Chhattisgarh 22"
shape$ST_NAME[which(shape$ST_NAME=="DADRA AND NAGAR HAVELI")] = "(26) Dadra+Nagar Haveli 26"
shape$ST_NAME[which(shape$ST_NAME=="DAMAN AND DIU")] = "(25) Daman & Diu 25"
shape$ST_NAME[which(shape$ST_NAME=="Goa")] = "(30) Goa 30"
shape$ST_NAME[which(shape$ST_NAME=="Gujarat")] = "(24) Gujarat 24"
shape$ST_NAME[which(shape$ST_NAME=="Haryana")] = "(06) Haryana 06"
shape$ST_NAME[which(shape$ST_NAME=="Himachal Pradesh")] = "(02) Himachal Pradesh 02"
shape$ST_NAME[which(shape$ST_NAME=="Jammu And Kashmir")] = "(01) Jammu & Kashmir 01"
shape$ST_NAME[which(shape$ST_NAME=="Jharkhand")] = "(20) Jharkhand 20"
shape$ST_NAME[which(shape$ST_NAME=="Karnataka")] = "(29) Karnataka 29"
shape$ST_NAME[which(shape$ST_NAME=="Kerala")] = "(32) Kerala 32"
shape$ST_NAME[which(shape$ST_NAME=="LAKSHADWEEP")] = "(31) Lakshadweep 31"
shape$ST_NAME[which(shape$ST_NAME=="Madhya Pradesh")] = "(23) Madhya Pradesh 23"
shape$ST_NAME[which(shape$ST_NAME=="Maharashtra")] = "(27) Maharashtra 27"
shape$ST_NAME[which(shape$ST_NAME=="Manipur")] = "(14) Manipur 14"
shape$ST_NAME[which(shape$ST_NAME=="Meghalaya")] = "(17) Meghalaya 17"
shape$ST_NAME[which(shape$ST_NAME=="Mizoram")] = "(15) Mizoram 15"
shape$ST_NAME[which(shape$ST_NAME=="Pondicherry")] = "(34) Pondicherry 34"
shape$ST_NAME[which(shape$ST_NAME=="Nagaland")] = "(13) Nagaland 13"
shape$ST_NAME[which(shape$ST_NAME=="Nct Of Delhi")] = "(07) Delhi 07"
shape$ST_NAME[which(shape$ST_NAME=="Orissa")] = "(21) Orissa 21"
shape$ST_NAME[which(shape$ST_NAME=="Punjab")] = "(03) Punjab 03"
shape$ST_NAME[which(shape$ST_NAME=="Rajasthan")] = "(08) Rajasthan 08"
shape$ST_NAME[which(shape$ST_NAME=="Sikkim")] = "(11) Sikkim 11"
shape$ST_NAME[which(shape$ST_NAME=="Tamil Nadu")] = "(33) Tamil Nadu 33"
shape$ST_NAME[which(shape$ST_NAME=="Tripura")] = "(16) Tripura 16"
shape$ST_NAME[which(shape$ST_NAME=="Uttar Pradesh")] = "(09) Uttar Pradesh 09"
shape$ST_NAME[which(shape$ST_NAME=="Uttarakhand")] = "(05) Uttarakhand 05"
shape$ST_NAME[which(shape$ST_NAME=="West Bengal")] = "(19) West Bengal 19"

shape_lat_long_centroids = st_centroid(shape)
shape_lat_long_centroids = shape_lat_long_centroids[match(levels(HIST_ST$states),shape_lat_long_centroids$ST_NAME),]

clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}


D = st_distance(shape_lat_long_centroids)
D = clean_units(D)/1000

W = 1/D
diag(W) = 1

# # #
# # # 7) Append area-level covariates to targets
# # #

D  = as.data.table(D)
colnames(D) = make.names(paste("D_from_",shape_lat_long_centroids$ST_NAME,sep=""))
D = cbind(states=shape_lat_long_centroids$ST_NAME,D)

area.covs = merge(HIST_ST ,D,by = "states",all=TRUE)
area.covs = merge(temp_demos,area.covs,by = c("states","zones"),all=TRUE)
area.covs$states = as.factor(area.covs$states)

area.covs[!complete.cases(area.covs),]

# create full area-level covariate matrix 
# area.covs = missRanger(area.covs,maxiter = 30,verbose = TRUE,returnOOB = TRUE)
# save( area.covs,file = 'Generated Quantities/area.covs.RData')
load(file = 'Generated Quantities/area.covs.RData')


targets  = merge(targets,area.covs ,by = c("states","zones"),all.x = TRUE)

# re-factor to update levels 
targets$states = as.factor(as.character(unlist(targets$states)))
# remove 0 to 17 year olds - they're in there as a category in the census
if(length(which(targets$age_cat=="[0-17]"))!=0){
  targets = targets[-which(targets$age_cat=="[0-17]"),]
  # re-factor to update levels 
  targets$age_cat = as.factor(as.character(unlist(targets$age_cat)))
}
# update the name of 'alliance_vote' to be 2014 - so that it can be distinguished from 2019 alliance vote later
names(targets)[which(names(targets)=="alliance_vote")]="PC_vote_choice_14"
names(targets)[names(targets)=="turnout"] = "turnout_2014"

# # #
# # # 8) Use a multiple-imputation strategy to `complete` the individual-level variables. 
# # #

# make names to ensure the `ranger' random-forest function can eat it 
names(targets) <- make.names(names(targets),unique = TRUE)
# take sample for speed during testing (hash samling-id out if this is not a test)
# sample_id <- sample(1:dim(targets)[1],size = 50000)
targets_sample <- targets# [sample_id,]

# perform multiple imputation allowing for substantial sampling of neighbours in predictive means matching, to allow for uncertainty. 
#  use subsamples to allow for memory challenges
# # # Generate 5 folds for the total model
# # # n.folds = 10
# # # Folds <- list()
# # # already_sampled<- c()
# # # sample_from = 1:dim(targets_sample )[1]
# # # for(i in 1:n.folds){
# # #  if(i>1){sample_from = sample_from[-which(sample_from %in% already_sampled)]}
# # #   if(i==n.folds){fold.size = dim(targets_sample)[1]-length(already_sampled)}else{fold.size = dim(targets_sample)[1]/n.folds}
# # #   Folds[[i]] <- sample(sample_from ,size =  fold.size,replace = FALSE)
# # #   already_sampled <- c(already_sampled,Folds[[i]] )
# # # }

# # # targets_imp.list = data.table()
# # # for(i in 1:n.folds){
#   targets_imp.temp <- missRanger(data = targets_sample,# # # [Folds[[i]],],
#                                  pmm.k = 30,
#                                  maxiter = 30,
#                                  verbose = TRUE,
#                                  returnOOB = TRUE,
#                                  num.trees = ultraparameters$num.trees_sf_imp)
  # # # targets_imp.list <- bind_rows(targets_imp.list,targets_imp.temp)
  # # # }
#   targets_imp = targets_imp.temp # targets_imp.list
  # # # targets_imp = targets_imp[order(unlist(Folds)),]

# create consolidated variable - useful for raking
# targets_imp$alliance_vote_2014 = ifelse(targets_imp$turnout_2014==0,"stayed home",as.character(unlist(targets_imp$PC_vote_choice_14)))
# targets_imp$alliance_vote_2014 = as.factor(targets_imp$alliance_vote_2014)
# save the imputed targets
# save(targets_imp,file = "Generated Quantities/targets_imp.RData",compress=TRUE)
load(file = "Generated Quantities/targets_imp.RData")


# plot 2014 vote distribution imputation
temp_past_vote_frame = as.matrix(
table(targets_imp$states,targets_imp$alliance_vote_2014)[,!levels(targets_imp$alliance_vote_2014) %in% "stayed home"]/
  rowSums(table(targets_imp$states,targets_imp$alliance_vote_2014)[,!levels(targets_imp$alliance_vote_2014) %in% "stayed home"])
)

temp_past_vote_frame = cbind(temp_past_vote_frame,0)
temp_past_vote_frame [match(names(table(NES14_clean$states)),rownames(temp_past_vote_frame)),dim(temp_past_vote_frame)[2]] = 
  as.numeric(as.character(unlist(table(NES14_clean$states))))


temp_past_vote_frame_T = 
  table(targets_imp$states,targets_imp$alliance_vote_2014)[,levels(targets_imp$alliance_vote_2014) %in% "stayed home"]/
  rowSums(table(targets_imp$states,targets_imp$alliance_vote_2014))

size_NES = as.numeric(as.character(unlist(  temp_past_vote_frame[,dim(temp_past_vote_frame)[2]] ) ) )/
  max(temp_past_vote_frame[,dim(temp_past_vote_frame)[2]])


pdf(file = 'Plots/preIPF_strat_density_bystate.pdf',width = 12.5,height = 3.5)
par(mfrow = c(1,4))
for(i in 1:(dim(temp_past_vote_frame)[2])){
  y = as.numeric(as.character(unlist(
      HIST_ST[match(rownames(temp_past_vote_frame),HIST_ST$states),c("av_st_vote_share_percentage.nda_2014",
                                                                     "av_st_vote_share_percentage.upa_2014",
                                                                     "av_st_vote_share_percentage.other_2014",
                                                                     "av_st_turnout_percentage_2014")[i],
              with=FALSE]/100
    ) ) )
  x = cbind(temp_past_vote_frame[,-dim(temp_past_vote_frame)[2]],(1-temp_past_vote_frame_T))[,i]
  
plot(y = y, x = x,
  xlim = c(0,1),ylim = c(0,1),
  xlab = 'estimated',ylab = 'observed',
  main = paste("state-level:",c("NDA","UPA","OTHER","Turnout")[i],"share"),
  bty = "n",
  pch = 1
)
temp = loess(y ~ x)
temp_order = order(x)
lines(y = temp$fitted[temp_order],x[temp_order],col = 'red')

  abline(0,1)
  legend("topleft",legend = c(
    paste('cor:',round(cor(x = x,y = y),3)),
    paste('mae:',round( mean(abs(y - x)) ,3))
  ),bty = "n")
}
dev.off()


HIST_Zones = HIST_Zones[which(HIST_Zones$Year==2014),]
HIST_Zones$Zones = as.factor(HIST_Zones$Zones)
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Eastern")] = "(04) Eastern 04"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North")] = "(01) North 01"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North-Central")] = "(02) North-Central 02"     
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North-Eastern")] = "(03) North-Eastern 03"    
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Southern")] = "(06) Southern 06"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Western")] = "(05) Western 05"     




temp_past_vote_frame = as.matrix(
  table(targets_imp$zones,targets_imp$alliance_vote_2014)[,!levels(targets_imp$alliance_vote_2014) %in% "stayed home"]/
    rowSums(table(targets_imp$zones,targets_imp$alliance_vote_2014)[,!levels(targets_imp$alliance_vote_2014) %in% "stayed home"])
)

temp_past_vote_frame = cbind(temp_past_vote_frame,0)
temp_past_vote_frame [match(names(table(NES14_clean$zones)),rownames(temp_past_vote_frame)),dim(temp_past_vote_frame)[2]] = 
  as.numeric(as.character(unlist(table(NES14_clean$zones))))


temp_past_vote_frame_T = 
  table(targets_imp$zones,targets_imp$alliance_vote_2014)[,levels(targets_imp$alliance_vote_2014) %in% "stayed home"]/
  rowSums(table(targets_imp$zones,targets_imp$alliance_vote_2014))

size_NES = as.numeric(as.character(unlist(  temp_past_vote_frame[,dim(temp_past_vote_frame)[2]] ) ) )/
  max(temp_past_vote_frame[,dim(temp_past_vote_frame)[2]])

pdf(file = 'Plots/preIPF_strat_density_byzone.pdf',width = 12.5,height = 3.5)
par(mfrow = c(1,4))
for(i in 1:(dim(temp_past_vote_frame)[2])){
  y = as.numeric(as.character(unlist(
    HIST_Zones[match(rownames(temp_past_vote_frame),HIST_Zones$Zones),c("Z_Vote_Share_Percentage.NDA",
                                                                        "Z_Vote_Share_Percentage.UPA",
                                                                        "Z_Vote_Share_Percentage.OTHER",
                                                                        "Z_Turnout_Percentage")[i]]
  ) ) )
  x = cbind(temp_past_vote_frame[,-dim(temp_past_vote_frame)[2]],(1-temp_past_vote_frame_T))[,i]
  
  plot(y = y, x = x,
       xlim = c(0,1),ylim = c(0,1),
       xlab = 'estimated',ylab = 'observed',
       main = paste("zone-level:",c("NDA","UPA","OTHER","Turnout")[i],"share"),
       bty = "n",
       pch = 1
  )
  
  temp = lm(y ~ x)
  temp_order = order(x)
  lines(y = temp$fitted[temp_order],x[temp_order],col = 'red')
  
  abline(0,1)
  legend("topleft",legend = c(
    paste('cor:',round(cor(x = x,y = y),3)),
    paste('mae:',round( mean(abs(y - x)) ,3))
  ),bty = "n")
}
dev.off()

# # #
# # # 8) Rake the 2014 vote to the observed proportions, and use the census to stabilize the other covariates;
# # # this is a response to curb the bias introduced by selection into the National Election Study and IHDS, 
# # # which form our training-data in the multiple imputation step. 
# # #
# prepare variables to rake in the impuited targets (we'll be raking as low as the state-level)
# first the alliance votes in 2014 at the state-level;
targets_imp$state_zone_alliance_vote_2014 = paste(targets_imp$states,
                                                  targets_imp$zones,
                                                  targets_imp$alliance_vote_2014)
## then the state-zone-age-edu-gender interaction.
targets_imp$state_zone_age_edu_gender = paste(targets_imp$states,
                                              targets_imp$zones,
                                              targets_imp$age_cat,
                                              targets_imp$education_level,
                                              targets_imp$gender)
# Create the state-level stratification weights 
# first derive the demigraphic weights from the Census:
CENSUS_counts_clean = as.data.table(CENSUS_counts_clean)
state_zone_age_edu_gender_weights_temp = CENSUS_counts_clean[,list(totcount = sum(totcount)),by = c("states","zones","age_cat","education_level","gender")]
state_zone_age_edu_gender_weights_temp$totcount = state_zone_age_edu_gender_weights_temp$totcount/sum(state_zone_age_edu_gender_weights_temp$totcount)

# Then derive the 2014 results from the historical data (have to re-count because results are as a proportion of voters, but our alliance-14 variable 
# now includes 'stayed home' as an option, hence results are needed as a proportion of the electorate)
ST_pop = CENSUS_counts_clean[,list(totcount_state = sum(totcount)),by = c("states","zones")]
ST_pop$T = ST_pop$totcount_state*HIST_ST$z_turnout_percentage_2014[match(ST_pop$states,HIST_ST$states)]
ST_pop$NO_T = ST_pop$totcount_state-ST_pop$T
ST_pop$NDA = ST_pop$T*HIST_ST$av_st_vote_share_percentage.nda_2014[match(ST_pop$states,HIST_ST$states)]/100
ST_pop$UPA = ST_pop$T*HIST_ST$av_st_vote_share_percentage.upa_2014[match(ST_pop$states,HIST_ST$states)]/100
ST_pop$OTHER = ST_pop$T*HIST_ST$av_st_vote_share_percentage.other_2014[match(ST_pop$states,HIST_ST$states)]/100

ST_pop_T_weights_1 = cbind(ST_pop[,c("states","zones","NDA"),with = FALSE],alliance_vote_2014= "NDA")
names(ST_pop_T_weights_1)[which(names(ST_pop_T_weights_1)=="NDA")] = "weight"
ST_pop_T_weights_2 = cbind(ST_pop[,c("states","zones","UPA"),with = FALSE],alliance_vote_2014= "UPA")
names(ST_pop_T_weights_2)[which(names(ST_pop_T_weights_2)=="UPA")] = "weight"
ST_pop_T_weights_3 = cbind(ST_pop[,c("states","zones","OTHER"),with = FALSE],alliance_vote_2014= "OTHER")
names(ST_pop_T_weights_3)[which(names(ST_pop_T_weights_3)=="OTHER")] = "weight"
ST_pop_T_weights_4 = cbind(ST_pop[,c("states","zones","NO_T"),with = FALSE],alliance_vote_2014 = "stayed home")
names(ST_pop_T_weights_4)[which(names(ST_pop_T_weights_4)=="NO_T")] = "weight"
ST_pop_T_weights = rbind(ST_pop_T_weights_1,ST_pop_T_weights_2,ST_pop_T_weights_3,ST_pop_T_weights_4)
ST_pop_T_weights$weight = ST_pop_T_weights$weight/sum(ST_pop_T_weights$weight)

sum(ST_pop_T_weights[ST_pop_T_weights$alliance_vote_2014=="stayed home","weight"])

# remove weights that are = 0 or below due to rounding problems 
# first from the to-be-raked data
targets_imp =  targets_imp[-which( targets_imp$state_zone_alliance_vote_2014 %in% 
                                     apply(ST_pop_T_weights[which(ST_pop_T_weights$weight<=0),-which(names(ST_pop_T_weights)=="weight"),with = FALSE],1,
                                           paste0,collapse = " ")
),]
# then from the targets 
ST_pop_T_weights = ST_pop_T_weights[-which(ST_pop_T_weights$weight<=0),]

# finally RAKE! (by state )

# create container to store state-level data and weights 
weighted_targets_list <- list()

ST = levels( targets_imp$states)[1]
for(ST in levels( targets_imp$states)){
  ST_targets_imp <- targets_imp[which(targets_imp$states==ST),]
  
  target_weights = list(
    state_zone_alliance_vote_2014 =         
      weights::wpct(x = paste(ST_pop_T_weights$states,
                              ST_pop_T_weights$zones,
                              ST_pop_T_weights$alliance_vote_2014)[which(ST_pop_T_weights$states==ST)],
                    weight = ST_pop_T_weights$weight[which(ST_pop_T_weights$states==ST)]),
    state_zone_age_edu_gender  = 
      weights::wpct(x = paste(state_zone_age_edu_gender_weights_temp$states,
                              state_zone_age_edu_gender_weights_temp$zones,
                              state_zone_age_edu_gender_weights_temp$age_cat,
                              state_zone_age_edu_gender_weights_temp$education_level,
                              state_zone_age_edu_gender_weights_temp$gender)[which(state_zone_age_edu_gender_weights_temp$states==ST)],
                    weight = state_zone_age_edu_gender_weights_temp$totcount[which(state_zone_age_edu_gender_weights_temp$states==ST)])
  )
  
  ST_targets_imp$state_zone_alliance_vote_2014 = as.factor(as.character(unlist(ST_targets_imp$state_zone_alliance_vote_2014)))
  ST_targets_imp$state_zone_age_edu_gender = as.factor(as.character(unlist(ST_targets_imp$state_zone_age_edu_gender)))
  
  # Do we have all levels in the targets? If not, remove levels we don't have targets for;
  if(!all(levels(ST_targets_imp$state_zone_alliance_vote_2014) %in% names(target_weights$state_zone_alliance_vote_2014))){
    ST_targets_imp = ST_targets_imp[which(ST_targets_imp$state_zone_alliance_vote_2014 %in% names(target_weights$state_zone_alliance_vote_2014)),]
  }
  if(!all(levels(ST_targets_imp$state_zone_age_edu_gender) %in% names(target_weights$state_zone_age_edu_gender))){
    ST_targets_imp = ST_targets_imp[which(ST_targets_imp$state_zone_age_edu_gender %in% names(target_weights$state_zone_age_edu_gender)),]
  }
  # Do we have all targets in the levels? If not, remove targets we don't have levels for;
  if(!all(names(target_weights$state_zone_alliance_vote_2014) %in% levels(ST_targets_imp$state_zone_alliance_vote_2014))){
    target_weights$state_zone_alliance_vote_2014 = target_weights$state_zone_alliance_vote_2014[which(names(target_weights$state_zone_alliance_vote_2014) %in% levels(ST_targets_imp$state_zone_alliance_vote_2014))]
  }
  if(!all(names(target_weights$state_zone_age_edu_gender) %in% levels(ST_targets_imp$state_zone_age_edu_gender))){
    target_weights$state_zone_age_edu_gender = target_weights$state_zone_age_edu_gender[which(names(target_weights$state_zone_age_edu_gender) %in% levels(ST_targets_imp$state_zone_age_edu_gender))]
  }
  
  
  levels(ST_targets_imp$state_zone_age_edu_gender)[!levels(ST_targets_imp$state_zone_age_edu_gender) %in% names(target_weights$state_zone_age_edu_gender)]
  
  names(target_weights$state_zone_age_edu_gender) %in% levels(ST_targets_imp$state_zone_age_edu_gender)
  
  
  
  levels(ST_targets_imp$state_zone_alliance_vote_2014)
  
  
  # Rake !
  raking <- anesrake(inputter = target_weights,
                     dataframe = ST_targets_imp,
                     caseid = 1:dim(ST_targets_imp)[1],
                     maxit = 10000,
                     cap =  1000,   # Maximum allowed weight per iteration - how much can we up-vote a given guy?
                     choosemethod = "total",# How are parameters compared for selection?#
                     type = "pctlim",# What selection criterion is used?
                     verbose = TRUE,                 
                     pctlim = 0 #percent  # Threshold for selection
  )
  # add weights to state-level data 
  ST_targets_imp$weights <- raking$weightvec
  weighted_targets_list = append(weighted_targets_list ,list(ST_targets_imp))
  # print statement to get an idea of how long we have left in the raking procedure
  print(paste(which(levels(targets_imp$states)==ST),"out of ",nlevels(targets_imp$states),"states raked."))
}
# bind results into a novel state-level dataset with weights 
weighted_targets = do.call('rbind',weighted_targets_list)

prop.table(questionr::wtd.table(weighted_targets$turnout_2014,weights = weighted_targets$weights))
prop.table(questionr::wtd.table(weighted_targets$PC_vote_choice_14,weights = weighted_targets$weights))

# ensure relative size of states has remained stable
weighted_targets_temp = weighted_targets
for(ST in levels( targets_imp$states)){
  weighted_targets_temp$weights[which(weighted_targets_temp$states==ST)] = 
sum(CENSUS_counts_clean$totcount[which(CENSUS_counts_clean$states==ST)])*
weighted_targets$weights[which(weighted_targets$states==ST)]/sum(weighted_targets$weights[which(weighted_targets$states==ST)])
}
weighted_targets$weights = weighted_targets_temp$weights

prop.table(questionr::wtd.table(weighted_targets$alliance_vote_2014,weights = weighted_targets$weights))
# save the weighted targets
# save(weighted_targets,file = "Generated Quantities/weighted_targets.RData",compress = TRUE)
load(file = "Generated Quantities/weighted_targets.RData")

reweighted = 
questionr::wtd.table(x = weighted_targets$states,
                     y = weighted_targets$alliance_vote_2014,
                     weights = weighted_targets$weights)


temp_past_vote_frame = as.matrix(reweighted[,!levels(weighted_targets$alliance_vote_2014) %in% "stayed home"]/
                                   rowSums(reweighted[,!levels(weighted_targets$alliance_vote_2014) %in% "stayed home"])
                                 )
temp_past_vote_frame = cbind(temp_past_vote_frame,0)
temp_past_vote_frame [match(names(questionr::wtd.table(NES14_clean$states)),rownames(temp_past_vote_frame)),dim(temp_past_vote_frame)[2]] = 
  as.numeric(as.character(unlist(questionr::wtd.table(NES14_clean$states))))

temp_past_vote_frame_T = 
  reweighted[,levels(weighted_targets$alliance_vote_2014) %in% "stayed home"]/
  rowSums(reweighted)

size_NES = as.numeric(as.character(unlist(  temp_past_vote_frame[,dim(temp_past_vote_frame)[2]] ) ) )/
  max(temp_past_vote_frame[,dim(temp_past_vote_frame)[2]])


pdf(file = 'Plots/postIPF_strat_density_bystate.pdf',width = 12.5,height = 3.5)
par(mfrow = c(1,4))
for(i in 1:(dim(temp_past_vote_frame)[2])){
  y = as.numeric(as.character(unlist(
    HIST_ST[match(rownames(temp_past_vote_frame),HIST_ST$states),c("av_st_vote_share_percentage.nda_2014",
                                                                   "av_st_vote_share_percentage.other_2014",
                                                                   "av_st_vote_share_percentage.upa_2014",
                                                                   "av_st_turnout_percentage_2014")[i],
            with=FALSE]/100
  ) ) )
  x = cbind(temp_past_vote_frame[,-dim(temp_past_vote_frame)[2]],(1-temp_past_vote_frame_T))[,i]
  
  plot(y = y, x = x,
       xlim = c(0,1),ylim = c(0,1),
       xlab = 'estimated',ylab = 'observed',
       main = paste("state-level:",c("NDA","OTHER","UPA","Turnout")[i],"share"),
       bty = "n",
       pch = 1
  )
  temp = loess(y ~ x)
  temp_order = order(x)
  lines(y = temp$fitted[temp_order],x[temp_order],col = 'red')
  
  abline(0,1)
  legend("topleft",legend = c(
    paste('cor:',round(cor(x = x,y = y),3)),
    paste('mae:',round( mean(abs(y - x)) ,3))
  ),bty = "n")
}
dev.off()

# # # # #
# # # # #
# # # # #
# # # # #

HIST_Zones = HIST_Zones[which(HIST_Zones$Year==2014),]
HIST_Zones$Zones = as.factor(HIST_Zones$Zones)
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Eastern")] = "(04) Eastern 04"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North")] = "(01) North 01"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North-Central")] = "(02) North-Central 02"     
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="North-Eastern")] = "(03) North-Eastern 03"    
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Southern")] = "(06) Southern 06"
levels(HIST_Zones$Zones)[which(levels(HIST_Zones$Zones)=="Western")] = "(05) Western 05"     


reweighted = 
  questionr::wtd.table(x = weighted_targets$zones,
                       y = weighted_targets$alliance_vote_2014,
                       weights = weighted_targets$weights)

temp_past_vote_frame = as.matrix(reweighted[,!levels(weighted_targets$alliance_vote_2014) %in% "stayed home"]/
                                   rowSums(reweighted[,!levels(weighted_targets$alliance_vote_2014) %in% "stayed home"])
)
temp_past_vote_frame = cbind(temp_past_vote_frame,0)
temp_past_vote_frame [match(names(questionr::wtd.table(NES14_clean$zones)),rownames(temp_past_vote_frame)),dim(temp_past_vote_frame)[2]] = 
  as.numeric(as.character(unlist(questionr::wtd.table(NES14_clean$zones))))

temp_past_vote_frame_T = 
  reweighted[,levels(weighted_targets$alliance_vote_2014) %in% "stayed home"]/
  rowSums(reweighted)

size_NES = as.numeric(as.character(unlist(  temp_past_vote_frame[,dim(temp_past_vote_frame)[2]] ) ) )/
  max(temp_past_vote_frame[,dim(temp_past_vote_frame)[2]])


pdf(file = 'Plots/postIPF_strat_density_byzone.pdf',width = 12.5,height = 3.5)
par(mfrow = c(1,4))
for(i in 1:(dim(temp_past_vote_frame)[2])){
  y = as.numeric(as.character(unlist(
    HIST_Zones[match(rownames(temp_past_vote_frame),HIST_Zones$Zones),c("Z_Vote_Share_Percentage.NDA",
                                                                        "Z_Vote_Share_Percentage.OTHER",
                                                                        "Z_Vote_Share_Percentage.UPA",
                                                                        "Z_Turnout_Percentage")[i]]
  ) ) )
  x = cbind(temp_past_vote_frame[,-dim(temp_past_vote_frame)[2]],(1-temp_past_vote_frame_T))[,i]
  
  plot(y = y, x = x,
       xlim = c(0,1),ylim = c(0,1),
       xlab = 'estimated',ylab = 'observed',
       main = paste("zone-level:",c("NDA","UPA","OTHER","Turnout")[i],"share"),
       bty = "n",
       pch = 1
  )
  
  temp = lm(y ~ x)
  temp_order = order(x)
  lines(y = temp$fitted[temp_order],x[temp_order],col = 'red')
  
  abline(0,1)
  legend("topleft",legend = c(
    paste('cor:',round(cor(x = x,y = y),3)),
    paste('mae:',round( mean(abs(y - x)) ,3))
  ),bty = "n")
}
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Collapse microdata into stratification frame
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
weighted_targets = as.data.table(weighted_targets)
stratification_frame = weighted_targets[,list(weights = sum(weights)),
                                        by = names(weighted_targets[,!"weights"])
                                        ]
stratification_frame$alliance_vote_2014 = as.factor(ifelse(stratification_frame$turnout_2014==0,"stayed home",as.character(unlist(stratification_frame$PC_vote_choice_14))))

save(stratification_frame,file = "Generated Quantities/stratification_frame.RData",compress = TRUE)

print(
  xtable(x =
    cbind(
          stratification_frame[c(1:5,(dim(stratification_frame)[1] - 4):dim(stratification_frame)[1]),
                               
                               c("weights",
                                 "states","zones","gender","age_cat","religion","education_level","jati","alliance_vote_2014",
                                 
                                 "av_st_vote_share_percentage.nda_2014","av_st_vote_share_percentage.upa_2014",
                                 "av_st_vote_share_percentage.other_2014","av_st_turnout_percentage_2014",
                                 
                                 "D_from_.01..Jammu...Kashmir.01","D_from_.02..Himachal.Pradesh.02",
                                 "D_from_.03..Punjab.03"

                                 )
                               
                               ])),
  include.rownames=FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Prepare Vote-Behaviour Training Data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # #
# # # 1) Upload data from non-representative surveys over the campaign.
# # #

# Upload surveys
Online_surveys = read.csv("Generated Quantities/Online_Surveys.csv",na.strings = c("",NA))
# turn characters into factors
lapply(Online_surveys,class)

Online_surveys <- as.data.table(Online_surveys)[, colnames(Online_surveys)[which(sapply(Online_surveys,class)=='character')]:= lapply(.SD,as.factor),.SDcols = colnames(Online_surveys)[which(sapply(Online_surveys,class)=='character')]]
Online_surveys = as.data.frame(Online_surveys)
# remove individuals which don't have significat variables for - missing both outcomes and covariates.
temp = 
  Online_surveys[,-c(which(names(Online_surveys )=="survey_date"),
                     which(names(Online_surveys )=="ID_GBIP"),
                     which(names(Online_surveys )=="weeks"),
                     which(names(Online_surveys )=="Source")
  )]
Online_surveys = Online_surveys [-which(apply(temp,1,function(x){sum(is.na(x))})==dim(temp)[2]) ,]
# remove obs with missin states 
Online_surveys = Online_surveys[-which(is.na(Online_surveys$survey_state)),]

# # #
# # # 3) Plot sample stats over rime, including comparison between raw numbers with actual election results at the national level 
# # # 

# upload national election results
res_obs = read.csv("Generated Quantities/2019_Results.csv",na.strings = c("",NA))
nat_res_obs = unique(res_obs[,c("alliance","Nat.Alliance.Pct.of.Votes")])

# define the week at which voting began
voting_begins_week = floor(as.numeric(difftime(as.Date("19/05/2019","%d/%m/%Y"),as.Date("11/04/2019",format = "%d/%m/%Y"))/7)) + 1
# create a clean raw-vote variable (OTHER are disaggregated in the raw data)
raw_vote =ifelse(Online_surveys$PC_vote_choice=="NDA"|Online_surveys$PC_vote_choice=="UPA",as.character(unlist(Online_surveys$PC_vote_choice)),"Other")
# interested only in raw vote conditional on turnout
raw_vote = ifelse(Online_surveys$turnout==0,NA,raw_vote )
raw_turnout = Online_surveys$turnout
# also interested in a comparison with past-vote 
raw_vote_14 =ifelse(Online_surveys$PC_vote_choice_14=="NDA"|Online_surveys$PC_vote_choice_14=="UPA",as.character(unlist(Online_surveys$PC_vote_choice_14)),"Other")
raw_vote_14 = ifelse(Online_surveys$turnout_14==0,NA,raw_vote_14 )
raw_turnout_14 = Online_surveys$turnout_14

# plot sample size evolution by medium, and comparison to election results 
pdf(file = 'Plots/sample_over_campaign.pdf',width = 15,height = 5)
par(mfrow = c(1,4))
plotthis = TRUE
if(plotthis == TRUE){
  plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
       as.numeric(table(Online_surveys$weeks)),
       xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
       pch= NA, bty ="n",xlab = "weeks to end of election",ylab="sample size",main = "weekly sample size",
       xaxt = "n",ylim =c(0,max(as.numeric(table(Online_surveys$weeks))))
  )
  axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)
  lines(min(Online_surveys$weeks):max(Online_surveys$weeks),table(Online_surveys$weeks,Online_surveys$Source)[,"AMechTurk"],lty = 2)
  lines(min(Online_surveys$weeks):max(Online_surveys$weeks),table(Online_surveys$weeks,Online_surveys$Source)[,"SubjectPool"],lty = 3)
  lines(min(Online_surveys$weeks):max(Online_surveys$weeks),as.numeric(table(Online_surveys$weeks)))
  legend("topright",lty = c(1,2,3,6),legend=c("total","mechanical turks","fb subject pool","voting begins"),bty = "n")
  abline(v = voting_begins_week + 1,lty = 6) # need to sum 1 because we are subtracting 1 in the labels 
}
# plot raw vote choices by medium, over time
# total
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "all observations"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)

lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),col = 'red')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"UPA"])/rowSums(table(Online_surveys$weeks,raw_vote)),col = 'blue')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"Other"])/rowSums(table(Online_surveys$weeks,raw_vote)),
      col = 'darkgrey')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout)[,"1"])/rowSums(table(Online_surveys$weeks,raw_turnout)),
      col = 'black')


abline(h = 0.6712,col = "black",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)

legend("bottomleft",legend=c("NDA","UPA","Other","turnout"),lty  = 1,bty = "n",col = c("red","blue","darkgrey","black"))
abline(v = voting_begins_week + 1,lty = 6) # need to sum 1 because we are subtracting 1 in the labels 

#AMT
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "mechanical turks"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)

lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"NDA","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'red',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"UPA","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'blue',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"Other","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'darkgrey',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,"1","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,,"AMechTurk"]),col = 'black',lty = 1)

abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)
abline(h = 0.6712,col = "black",lty = 4)

legend("bottomleft",legend=c("NDA","UPA","Other","turnout"),lty  = 1,bty = "n",col = c("red","blue","darkgrey","black"))
abline(v = voting_begins_week + 1,lty = 6) # need to sum 1 because we are subtracting 1 in the labels 

#FB
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "fb subject pool"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)


lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"NDA","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'red',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"UPA","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'blue',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"Other","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'darkgrey',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,"1","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,,"SubjectPool"]),col = 'black',lty = 1)

abline(h = 0.6712,col = "black",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)

legend("bottomleft",legend=c("NDA","UPA","Other","turnout"),lty  = 1,bty = "n",col = c("red","blue","darkgrey","black"))
abline(v = voting_begins_week + 1,lty = 6) # need to sum 1 because we are subtracting 1 in the labels 

dev.off()

# # #
# # # 2) Observe some salient features of the data.
# # #

# observe percentage responses with at least one missing datum 
print(sum(apply(Online_surveys,1,function(x){sum(is.na(x))})>=1)/dim(Online_surveys)[1])
# observe total number of responses
print(dim(Online_surveys)[1])
# observe total number by source
print(table(Online_surveys$Source))
# observe total number of unique respondents
print(sum(table(Online_surveys[!duplicated(as.character(unlist(Online_surveys$ID_GBIP))),]$Source)))
# observe total number of unique respondents by source
print(table(Online_surveys[!duplicated(as.character(unlist(Online_surveys$ID_GBIP))),]$Source))


# # #
# # # 4) Augment individual level data with area-level political variables
# # # 

# weeks to election threshold
#Online_surveys = Online_surveys[which(Online_surveys$weeks>=ultraparameters$weeks_threshold),]
Online_surveys = Online_surveys[which(as.Date(Online_surveys$survey_date,"%Y-%m-%d")<as.Date("2019-04-11","%Y-%m-%d")),]


# create a clean raw-vote variable (OTHER are disaggregated in the raw data)
raw_vote =ifelse(Online_surveys$PC_vote_choice=="NDA"|Online_surveys$PC_vote_choice=="UPA",as.character(unlist(Online_surveys$PC_vote_choice)),"Other")
# interested only in raw vote conditional on turnout
raw_vote = ifelse(Online_surveys$turnout==0,NA,raw_vote )
raw_turnout = Online_surveys$turnout
# also interested in a comparison with past-vote 
raw_vote_14 =ifelse(Online_surveys$PC_vote_choice_14=="NDA"|Online_surveys$PC_vote_choice_14=="UPA",as.character(unlist(Online_surveys$PC_vote_choice_14)),"Other")
raw_vote_14 = ifelse(Online_surveys$turnout_14==0,NA,raw_vote_14 )
raw_turnout_14 = Online_surveys$turnout_14

# sample evolution in the before beginning of election 
# plot sample size evolution by medium, and comparison to election results 
pdf(file = 'Plots/sample_over_campaign_before.pdf',width = 12.5,height = 3.5)
par(mfrow = c(1,4))

  plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
       as.numeric(table(Online_surveys$weeks)),
       xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
       pch= NA, bty ="n",xlab = "weeks to end of election",ylab="sample size",main = "weekly sample size",
       xaxt = "n",ylim =c(0,max(as.numeric(table(Online_surveys$weeks))))
  )
  axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)
  lines(min(Online_surveys$weeks):max(Online_surveys$weeks),table(Online_surveys$weeks,Online_surveys$Source)[,"AMechTurk"],lty = 2)
  lines(min(Online_surveys$weeks):max(Online_surveys$weeks),table(Online_surveys$weeks,Online_surveys$Source)[,"SubjectPool"],lty = 3)
  legend("topleft",lty = c(2,3),legend=c("mechanical turks","fb subject pool"),bty = "n")
# plot raw vote choices by medium, over time
# total
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "all observations"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)

lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),col = 'red')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"UPA"])/rowSums(table(Online_surveys$weeks,raw_vote)),col = 'blue')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote)[,"Other"])/rowSums(table(Online_surveys$weeks,raw_vote)),
      col = 'darkgrey')
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout)[,"1"])/rowSums(table(Online_surveys$weeks,raw_turnout)),
      col = 'black')


abline(h = 0.6712,col = "black",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)

#AMT
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "mechanical turks"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)

lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"NDA","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'red',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"UPA","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'blue',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"Other","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"AMechTurk"]),col = 'darkgrey',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,"1","AMechTurk"])/rowSums(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,,"AMechTurk"]),col = 'black',lty = 1)

abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)
abline(h = 0.6712,col = "black",lty = 4)

#FB
plot(min(Online_surveys$weeks):max(Online_surveys$weeks),
     as.numeric(table(Online_surveys$weeks,raw_vote)[,"NDA"])/rowSums(table(Online_surveys$weeks,raw_vote)),
     xlim = c(max(Online_surveys$weeks),min(Online_surveys$weeks)),
     pch= NA, bty ="n",xlab = "weeks to end of election",ylab="pct support",
     xaxt = "n",ylim =c(0,1),main = "fb subject pool"
)
axis(side = 1,at = min(Online_surveys$weeks):max(Online_surveys$weeks),labels = (min(Online_surveys$weeks):max(Online_surveys$weeks))-1)


lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"NDA","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'red',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"UPA","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'blue',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,"Other","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_vote,Online_surveys$Source)[,,"SubjectPool"]),col = 'darkgrey',lty = 1)
lines(min(Online_surveys$weeks):max(Online_surveys$weeks),
      as.numeric(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,"1","SubjectPool"])/rowSums(table(Online_surveys$weeks,raw_turnout,Online_surveys$Source)[,,"SubjectPool"]),col = 'black',lty = 1)

abline(h = 0.6712,col = "black",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[1]/100,col = "red",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[2]/100,col = "darkgrey",lty = 2)
abline(h = nat_res_obs$Nat.Alliance.Pct.of.Votes[3]/100,col = "blue",lty = 2)

dev.off()

dim(Online_surveys)
# responses by source 
print(table(Online_surveys[,]$Source))
# unique respondents by source
print(table(Online_surveys[!duplicated(as.character(unlist(Online_surveys$ID_GBIP))),]$Source))

apply(Online_surveys,2,function(x){sum(is.na(x))})/dim(Online_surveys)[1]

levels(Online_surveys$jati)
table(Online_surveys$survey_date,Online_surveys$turnout_14)
table(Online_surveys$survey_date,Online_surveys$PC_vote_choice_14)

# merge with historical state-level variables
Online_surveys = merge(Online_surveys,area.covs,by.x = c("survey_state"),by.y = "states",all.x=TRUE) 
names(Online_surveys)[which(names(Online_surveys)=="survey_state")] = "states"



# # #
# # # 5) Clean data to match targets
# # # 

# clean vote variable to have nda - upa - other 
levels(Online_surveys$PC_vote_choice) = ifelse(levels(Online_surveys$PC_vote_choice)!="NDA" & 
                                                 levels(Online_surveys$PC_vote_choice)!="UPA","OTHER",
                                               levels(Online_surveys$PC_vote_choice))
# clean vote 2014 variable to have nda - upa - other 
levels(Online_surveys$PC_vote_choice_14) = ifelse(levels(Online_surveys$PC_vote_choice_14)!="NDA" & 
                                                    levels(Online_surveys$PC_vote_choice_14)!="UPA","OTHER",
                                                  levels(Online_surveys$PC_vote_choice_14))
# clean education variables levels
levels(Online_surveys$education_level) = trimws(levels(Online_surveys$education_level),which = 'both')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Prepare NES Turnout training data if we are using NES turnout instead of current turnout
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
if(ultraparameters$turnout_2014){
  # # #
  # # # 1) Upload data from National Election Study 
  # # #
  
  NES14_clean = fread("Generated Quantities/NES14_clean.csv",na.strings = c("",NA))
  # stick to telangana still part of Andra Pradesh
  NES14_clean$states = as.factor(as.character(unlist(NES14_clean$states)))
  levels(NES14_clean$states)[which(levels(NES14_clean$states)=="(36) Telangana 36")] = "(28) Andhra Pradesh 28"
  
  # # #
  # # # 2) Augment individual level data with area-level political variables
  # # # 
  
  # merge with historical state-level variables
  NES14_clean = merge(NES14_clean,area.covs,by.x = c("states","zones"),by.y = c("states","zones"),all.x=TRUE) 
  names(NES14_clean)[which(names(NES14_clean)=="alliance_vote")]="PC_vote_choice_14"
  names(NES14_clean)[which(names(NES14_clean)=="turnout")]="turnout_14"
  
  NES14_clean = cbind(
    NES14_clean[,lapply(.SD,function(x){as.factor(as.character(unlist(x)))}),
                .SDcols = c("states","zones","gender","age_cat","religion","education_level","income_level","jati","turnout_14","PC_vote_choice_14")],
    NES14_clean[,-which(names(NES14_clean) %in% c("states","zones","gender","age_cat","religion","education_level","income_level","jati","turnout_14","PC_vote_choice_14")),with = FALSE]
  )
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Solve missing-data in the individual-level sample
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# solve missing data in training data 

Online_surveys_imp = missRanger::missRanger(data = Online_surveys,
                                            pmm.k = 30,
                                            maxiter = 30,
                                            verbose = TRUE,
                                            returnOOB = TRUE,
                                            num.trees = ultraparameters$num.trees_survey_imp)
Online_surveys_imp$alliance_vote_2014 = ifelse(Online_surveys_imp$turnout_14==0,"stayed home",as.character(unlist(Online_surveys_imp$PC_vote_choice_14)))
Online_surveys_imp$alliance_vote_2014  = as.factor(Online_surveys_imp$alliance_vote_2014 )

# Ensure all possible levels are in the sample, so we can estimate random effects for them
levels(Online_surveys_imp$states) = c(levels(Online_surveys_imp$states),levels(stratification_frame$states)[which(is.na(match(levels(stratification_frame$states),levels(Online_surveys_imp$states))))])
levels(Online_surveys_imp$zones) = c(levels(Online_surveys_imp$zones),levels(stratification_frame$zones)[which(is.na(match(levels(stratification_frame$zones),levels(Online_surveys_imp$zones))))])
levels(Online_surveys_imp$gender) = c(levels(Online_surveys_imp$gender),levels(stratification_frame$gender)[which(is.na(match(levels(stratification_frame$gender),levels(Online_surveys_imp$gender))))])
levels(Online_surveys_imp$age_cat) = c(levels(Online_surveys_imp$age_cat),levels(stratification_frame$age_cat)[which(is.na(match(levels(stratification_frame$age_cat),levels(Online_surveys_imp$age_cat))))])
levels(Online_surveys_imp$religion) = c(levels(Online_surveys_imp$religion),levels(stratification_frame$religion)[which(is.na(match(levels(stratification_frame$religion),levels(Online_surveys_imp$religion))))])
levels(Online_surveys_imp$education_level) = c(levels(Online_surveys_imp$education_level),levels(stratification_frame$education_level)[which(is.na(match(levels(stratification_frame$education_level),levels(Online_surveys_imp$education_level))))])
levels(Online_surveys_imp$income_level) = c(levels(Online_surveys_imp$income_level),levels(stratification_frame$income_level)[which(is.na(match(levels(stratification_frame$income_level),levels(Online_surveys_imp$income_level))))])
levels(Online_surveys_imp$jati) = c(levels(Online_surveys_imp$jati),levels(stratification_frame$jati)[which(is.na(match(levels(stratification_frame$jati),levels(Online_surveys_imp$jati))))])
levels(Online_surveys_imp$turnout_14) = c(levels(Online_surveys_imp$turnout_14),levels(stratification_frame$turnout_14)[which(is.na(match(levels(stratification_frame$turnout_14),levels(Online_surveys_imp$turnout_14))))])
levels(Online_surveys_imp$PC_vote_choice_14) = c(levels(Online_surveys_imp$PC_vote_choice_14),levels(stratification_frame$PC_vote_choice_14)[which(is.na(match(levels(stratification_frame$PC_vote_choice_14),levels(Online_surveys_imp$PC_vote_choice_14))))])

prop.table(table(Online_surveys_imp$turnout))
attributes(Online_surveys_imp)

save(  Online_surveys_imp,file = "Generated Quantities/Online_surveys_imp.RData",compress = TRUE)
load(file = "Generated Quantities/Online_surveys_imp.RData")


prop.table(table(NES14_clean$turnout_14))


apply(NES14_clean,2,function(x){sum(is.na(x))})/dim(NES14_clean)[1]

if(ultraparameters$turnout_2014){
  # solve missing data in training data 
  NES14_clean_imp = missRanger::missRanger(data = NES14_clean,
                                           pmm.k = 30,
                                           maxiter = 30,
                                           verbose = TRUE,
                                           returnOOB = TRUE,
                                           num.trees = ultraparameters$num.trees_survey_imp)
  # Ensure all possible levels are in the sample, so we can estimate random effects for them
  levels(NES14_clean_imp$states) = c(levels(NES14_clean_imp$states),levels(stratification_frame$states)[which(is.na(match(levels(stratification_frame$states),levels(NES14_clean_imp$states))))])
  levels(NES14_clean_imp$zones) = c(levels(NES14_clean_imp$zones),levels(stratification_frame$zones)[which(is.na(match(levels(stratification_frame$zones),levels(NES14_clean_imp$zones))))])
  levels(NES14_clean_imp$gender) = c(levels(NES14_clean_imp$gender),levels(stratification_frame$gender)[which(is.na(match(levels(stratification_frame$gender),levels(NES14_clean_imp$gender))))])
  levels(NES14_clean_imp$age_cat) = c(levels(NES14_clean_imp$age_cat),levels(stratification_frame$age_cat)[which(is.na(match(levels(stratification_frame$age_cat),levels(NES14_clean_imp$age_cat))))])
  levels(NES14_clean_imp$religion) = c(levels(NES14_clean_imp$religion),levels(stratification_frame$religion)[which(is.na(match(levels(stratification_frame$religion),levels(NES14_clean_imp$religion))))])
  levels(NES14_clean_imp$education_level) = c(levels(NES14_clean_imp$education_level),levels(stratification_frame$education_level)[which(is.na(match(levels(stratification_frame$education_level),levels(NES14_clean_imp$education_level))))])
  levels(NES14_clean_imp$income_level) = c(levels(NES14_clean_imp$income_level),levels(stratification_frame$income_level)[which(is.na(match(levels(stratification_frame$income_level),levels(NES14_clean_imp$income_level))))])
  levels(NES14_clean_imp$jati) = c(levels(NES14_clean_imp$jati),levels(stratification_frame$jati)[which(is.na(match(levels(stratification_frame$jati),levels(NES14_clean_imp$jati))))])
  levels(NES14_clean_imp$turnout_14) = c(levels(NES14_clean_imp$turnout_14),levels(stratification_frame$turnout_14)[which(is.na(match(levels(stratification_frame$turnout_14),levels(NES14_clean_imp$turnout_14))))])
  levels(NES14_clean_imp$PC_vote_choice_14) = c(levels(NES14_clean_imp$PC_vote_choice_14),levels(stratification_frame$PC_vote_choice_14)[which(is.na(match(levels(stratification_frame$PC_vote_choice_14),levels(NES14_clean_imp$PC_vote_choice_14))))])
  
  save(  NES14_clean_imp,file = "Generated Quantities/NES14_clean_imp.RData",compress = TRUE)
  load(file = "Generated Quantities/NES14_clean_imp.RData")
  
}

attributes(  NES14_clean_imp)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # Train Model - RF
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# clean workspace
rm(list=setdiff(ls(), c("ultraparameters")))
gc()
# set decimals to digits instead of scientific
options(scipen=999)
# set work directory
setwd("~/Dropbox/India 2019/")
#
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Sample function is useful but buggy - if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}
#
remove_zero_or_neg = function(x){
  if(sum(x<=0)>0){
    x = x[-which(x<=0)]
  }
  return(x)
}
#
zero_one_cap = function(x){ifelse(x<0,0,ifelse(x>1,1,x))}
#
library(ranger)
library(FNN)
library(mltools)
library(foreach)
library(data.table)
library(foreach)
library(ranger)
library(questionr)
library(dplyr)
library(forestError)
load(file = 'Generated Quantities/ultraparameters.RData')
load(file = 'Generated Quantities/NES14_clean_imp.RData')
load(file = 'Generated Quantities/Online_surveys_imp.RData')
load(file = 'Generated Quantities/stratification_frame.RData')

# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # Show Representation of Samples  # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # #

ind = c("states","zones","gender","family_role","marital_status","age_cat",
        "rurality","religion","literacy","education_level","income_level",
        "jati","alliance_vote_2014")
temp_sf_ind = 
stratification_frame[,which(colnames(stratification_frame) %in% colnames(NES14_clean_imp) & 
                            colnames(stratification_frame) %in% colnames(Online_surveys_imp) &
                            colnames(stratification_frame) %in% ind |
                            colnames(stratification_frame) %in% "weights"
                            ),with=FALSE]

NES14_clean_imp$alliance_vote_2014 = as.factor(
  ifelse(NES14_clean_imp$turnout_14==0,"stayed home",
         as.character(unlist(NES14_clean_imp$PC_vote_choice_14))))
temp_NES_ind = 
  NES14_clean_imp[,which(colnames(NES14_clean_imp) %in% colnames(stratification_frame) & 
                         colnames(NES14_clean_imp) %in% colnames(Online_surveys_imp) &
                         colnames(NES14_clean_imp) %in% ind
                         ),with=FALSE]
temp_OS_AMT_ind = 
  as.data.table(Online_surveys_imp)[Online_surveys_imp$Source=="AMechTurk",
                                    which(colnames(Online_surveys_imp) %in% colnames(stratification_frame) & 
                                           colnames(Online_surveys_imp) %in% colnames(NES14_clean_imp) &
                                           colnames(Online_surveys_imp) %in% ind
                                           ),with=FALSE]
temp_OS_FB_ind = 
  as.data.table(Online_surveys_imp)[Online_surveys_imp$Source=="SubjectPool",
                                    which(colnames(Online_surveys_imp) %in% colnames(stratification_frame) & 
                                             colnames(Online_surveys_imp) %in% colnames(NES14_clean_imp) &
                                             colnames(Online_surveys_imp) %in% ind
  ),with=FALSE]
ind = colnames(temp_sf_ind)[-which(colnames(temp_sf_ind) %in% "weights")]
temp_OS_AMT_ind = temp_OS_AMT_ind[,ind,with=FALSE]
temp_OS_FB_ind = temp_OS_FB_ind[,ind,with=FALSE]
temp_NES_ind  = temp_NES_ind [,ind,with=FALSE]


temp_sf_ind = one_hot(temp_sf_ind)
tempnames = colnames(temp_sf_ind )[-which(colnames(temp_sf_ind )=="weights")]
temp_sf_ind = 
  foreach(i = 1:length(tempnames),.combine = 'cbind') %do% 
  sum(
  as.numeric(as.character(unlist(temp_sf_ind[,i,with = FALSE])))*
  temp_sf_ind$weights)/sum(temp_sf_ind$weights)
  
colnames(temp_sf_ind) = tempnames

temp_NES_ind = colMeans(one_hot(temp_NES_ind))
temp_OS_AMT_ind = colMeans(one_hot(temp_OS_AMT_ind ))
temp_OS_FB_ind = colMeans(one_hot(temp_OS_FB_ind ))


library(questionr)
prop.table(table(NES14_clean_imp$alliance_vote_2014[-which(NES14_clean_imp$alliance_vote_2014=="stayed home")]))

prop.table(wtd.table(stratification_frame$alliance_vote_2014[-which(stratification_frame$alliance_vote_2014=="stayed home")],
                     weights = stratification_frame$weights[-which(stratification_frame$alliance_vote_2014=="stayed home")]))


prop.table(wtd.table(stratification_frame$income_level,
                     weights = stratification_frame$weights))


# # # # 
# # # #
# # # #

for(i in 1:length(ind)){
  if(ind[i]=="states"){
    pch_temp = 1
    }else{pch_temp = 1:length(temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))])
    }
pdf(file = paste('Plots/sample_v_pop',ind[i],'.pdf',sep=""),width = 12.5,height = 3.5)
par(mfrow = c(1,3),oma=c(0, 0, 0, 15),xpd = TRUE)
plot(y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))],
     x = temp_NES_ind[grep(ind[i],names(temp_NES_ind))],
     main = paste(gsub("jati","caste",gsub("_"," ",gsub("PC_vote_choice_14","2014 vote",ind[i]))),": pop v. NES 2014",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = pch_temp,bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)
legend("topleft",
       legend = c(paste("cor:",
                        round(
                        cor(x = temp_NES_ind[grep(ind[i],colnames(temp_sf_ind))],
                            y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))]),
                        3)),
                  paste("mae:",
                        round(mean(abs(
                          temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))] - 
                          temp_NES_ind[grep(ind[i],colnames(temp_sf_ind))])),
                          3))),
       bty = "n")
# # # # # 
# # # # # 
# # # # # 
plot(y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))],
     x = temp_OS_AMT_ind[grep(ind[i],names(temp_OS_AMT_ind))],
     main = paste(gsub("jati","caste",gsub("_"," ",gsub("PC_vote_choice_14","2014 vote",ind[i]))),": pop v. AMT sample",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = pch_temp,bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)
legend("topleft",
       legend = c(paste("cor:",
                        round(
                          cor(x = temp_OS_AMT_ind[grep(ind[i],colnames(temp_sf_ind))],
                              y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))]),
                          3)),
                  paste("mae:",
                        round(mean(abs(
                          temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))] - 
                            temp_OS_AMT_ind[grep(ind[i],colnames(temp_sf_ind))])),
                          3))),
       bty = "n")
# # # # # 
# # # # # 
# # # # # 
plot(y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))],
     x = temp_OS_FB_ind[grep(ind[i],names(temp_OS_FB_ind))],
     main = paste(gsub("jati","caste",gsub("_"," ",gsub("PC_vote_choice_14","2014 vote",ind[i]))),": pop v. FB sample",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = pch_temp,
     bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)

legend("topleft",
       legend = c(paste("cor:",
                        round(
                          cor(x = temp_OS_FB_ind[grep(ind[i],colnames(temp_sf_ind))],
                              y = temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))]),
                          3)),
                  paste("mae:",
                        round(mean(abs(
                          temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))] - 
                          temp_OS_FB_ind[grep(ind[i],colnames(temp_sf_ind))])),
                          3))),
       bty = "n")
if(ind[i]!="states"){
legend(par('usr')[2], par('usr')[4], xpd=NA,
       legend = gsub("stayed","stayed home",
       gsub("_","",gsub("\\₹","",gsub(ind[i],"",
       sub("\\s+[^ ]+$", "", colnames(temp_sf_ind)[grep(ind[i],colnames(temp_sf_ind))])
       )))),
       pch = 1:length(temp_sf_ind[grep(ind[i],colnames(temp_sf_ind))]),
       bty = "n")
}
dev.off()
}







pdf(file = paste('Plots/sample_v_pop.summary.pdf',sep=""),width = 9.5,height = 3.5)
par(mfrow = c(1,3),oma=c(0, 0, 0, 0),xpd = TRUE)
plot(y = temp_sf_ind,
     x = temp_NES_ind[colnames(temp_sf_ind)],
     main = paste("marginals: pop v. NES 2014",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = 1,bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)
legend("topleft",
       legend = c(paste("cor:",
                        round(
                          cor(x = temp_NES_ind[colnames(temp_sf_ind)],
                              y = as.numeric(unlist(temp_sf_ind))),
                          3)),
                  paste("mae:",
                        round(mean(abs(
                          as.numeric(unlist(temp_sf_ind)) - 
                            temp_NES_ind[colnames(temp_sf_ind)])),
                          3))),
       bty = "n")
temp = ifelse(abs(
  as.numeric(unlist(temp_sf_ind)) - 
    temp_NES_ind[colnames(temp_sf_ind)])>2*sd(abs(
      as.numeric(unlist(temp_sf_ind)) - 
        temp_NES_ind[colnames(temp_sf_ind)])),
  colnames(temp_sf_ind),NA
)
text(y = as.numeric(unlist(temp_sf_ind))+0.025,
     x = temp_NES_ind[colnames(temp_sf_ind)]+0.025,
     labels =sub("\\s+[^ ]+$", "",sub(".*? ", "", temp)),cex = 0.65)
     
# # # # # 
# # # # # 
# # # # # 
plot(y = temp_sf_ind,
     x = temp_OS_AMT_ind[colnames(temp_sf_ind)],
     main = paste("marginals: pop v. AMT sample",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = 1,bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)
legend("topleft",
       legend = c(paste("cor:",
                        round(
                          cor(x = temp_OS_AMT_ind[colnames(temp_sf_ind)],
                              y = as.numeric(unlist(temp_sf_ind))),
                          3)),
                  paste("mae:",
                        round(mean(abs(
                          as.numeric(unlist(temp_sf_ind)) - 
                            temp_OS_AMT_ind[colnames(temp_sf_ind)])),
                          3))),
       bty = "n")
temp = ifelse(abs(
  as.numeric(unlist(temp_sf_ind)) - 
    temp_OS_AMT_ind[colnames(temp_sf_ind)])>2*sd(abs(
      as.numeric(unlist(temp_sf_ind)) - 
        temp_OS_AMT_ind[colnames(temp_sf_ind)])),
  colnames(temp_sf_ind),NA
)
text(y = as.numeric(unlist(temp_sf_ind))+0.025,
     x = temp_OS_AMT_ind[colnames(temp_sf_ind)]+0.025,
     labels =sub("\\s+[^ ]+$", "",sub(".*? ", "", temp)),cex = 0.65)
# # # # # 
# # # # # 
# # # # # 
plot(y = temp_sf_ind,
     x = temp_OS_FB_ind[colnames(temp_sf_ind)],
     main = paste("marginals: pop v. FB sample",sep = ""),
     xlim = c(0,1),ylim = c(0,1),
     pch = 1,bty = "n",xlab = '% in sample',ylab = '% in pop')
abline(0,1, xpd=FALSE)
legend("topleft",
       legend = c(paste("cor:",
                        round(
                          cor(x = temp_OS_FB_ind[colnames(temp_sf_ind)],
                              y = as.numeric(unlist(temp_sf_ind))),
                          3)),
                  paste("mae:",
                        round(mean(abs(
                          as.numeric(unlist(temp_sf_ind)) - 
                            temp_OS_FB_ind[colnames(temp_sf_ind)])),
                          3))),
       bty = "n")
temp = ifelse(abs(
  as.numeric(unlist(temp_sf_ind)) - 
    temp_OS_FB_ind[colnames(temp_sf_ind)])>2*sd(abs(
      as.numeric(unlist(temp_sf_ind)) - 
        temp_OS_FB_ind[colnames(temp_sf_ind)])),
  colnames(temp_sf_ind),NA
)
text(y = as.numeric(unlist(temp_sf_ind))+0.025,
     x = temp_OS_FB_ind[colnames(temp_sf_ind)]+0.025,
     labels =sub("\\s+[^ ]+$", "",sub(".*? ", "", temp)),cex = 0.65)
dev.off()


prop.table(wtd.table(stratification_frame$states,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$states[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$zones[Online_surveys_imp$Source=="AMechTurk"]))

prop.table(wtd.table(Online_surveys_imp$gender[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$gender[Online_surveys_imp$Source=="SubjectPool"]))

prop.table(wtd.table(stratification_frame$religion,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$religion[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$religion[Online_surveys_imp$Source=="SubjectPool"]))

prop.table(wtd.table(stratification_frame$jati,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$jati[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$jati[Online_surveys_imp$Source=="SubjectPool"]))

prop.table(wtd.table(stratification_frame$income_level,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$income_level[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$income_level[Online_surveys_imp$Source=="SubjectPool"]))

prop.table(wtd.table(stratification_frame$education_level,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$education_level[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$education_level[Online_surveys_imp$Source=="SubjectPool"]))

prop.table(wtd.table(stratification_frame$turnout,weights = stratification_frame$weights))
prop.table(wtd.table(Online_surveys_imp$turnout[Online_surveys_imp$Source=="AMechTurk"]))
prop.table(wtd.table(Online_surveys_imp$turnout[Online_surveys_imp$Source=="SubjectPool"]))




# # # # # Set up Training Data 
# ensure consistent data.table format 
# want to streamline code - if source is 'both' sub with vector with both strings
if(ultraparameters$source =="Both"){ultraparameters$source = c("AMechTurk","SubjectPool")}
V = Online_surveys_imp$PC_vote_choice[which(Online_surveys_imp$Source %in% ultraparameters$source)]
# training data for vote-choice 
survey_Z = as.data.table(Online_surveys_imp[,names(Online_surveys_imp) %in% names(stratification_frame)][which(Online_surveys_imp$Source %in% ultraparameters$source),])
# data-source (pollster) identifier
P = Online_surveys_imp$Source[which(Online_surveys_imp$Source %in% ultraparameters$source)]
# weeks to election identifier
WtE = Online_surveys_imp$weeks[which(Online_surveys_imp$Source %in% ultraparameters$source)]
# IF TURNOUT 2014 (so if we are using past turnout demographics)
# make turnout numeric 
  T = as.numeric(as.character(unlist(NES14_clean_imp$turnout_14)))
  # training data for turnout 
  survey_T = NES14_clean_imp[,names(NES14_clean_imp) %in% names( survey_Z),with = FALSE]
  # turnout training data can't include alliance vote as it contains outcome variable 
  survey_T = survey_T[,!"alliance_vote_2014"]
# correct T to match past election turnout
  
  library(anesrake)
  library(questionr)
  input = list(PC_vote_choice_14 = prop.table(questionr::wtd.table(stratification_frame$PC_vote_choice_14,weights = stratification_frame$weights)),
               T = prop.table(questionr::wtd.table(ifelse(stratification_frame$turnout_2014==1,"Yes","No"),weights = stratification_frame$weights)),
               states = prop.table(questionr::wtd.table(stratification_frame$states,weights = stratification_frame$weights)),
               gender = prop.table(questionr::wtd.table(stratification_frame$gender,weights = stratification_frame$weights)),
               age_cat = prop.table(questionr::wtd.table(stratification_frame$age_cat,weights = stratification_frame$weights)),
               religion = prop.table(questionr::wtd.table(stratification_frame$religion,weights = stratification_frame$weights)),
               education_level = prop.table(questionr::wtd.table(stratification_frame$education_level,weights = stratification_frame$weights)),
               income_level = prop.table(questionr::wtd.table(stratification_frame$income_level,weights = stratification_frame$weights)),
               jati = prop.table(questionr::wtd.table(stratification_frame$jati,weights = stratification_frame$weights))
               )
  temp = data.table(survey_T[,names(input)[-which(names(input)=="T")],with=FALSE],T = as.factor(ifelse(T==1,"Yes","No")))
  T.weights = anesrake(inputter = input,dataframe = temp,caseid = 1:dim(temp)[1],cap = 10,verbose = TRUE,maxit = 1000,pctlim = 0)
  # while(prop.table(table(T))[2]>prop.table(wtd.table(stratification_frame$turnout_2014,weights = stratification_frame$weights))[2]){
  #   T[sample(which(T==1),size = 1)] = 0
  # }
  
  #save(T,file = 'Generated Quantities/T.train.RData',compress = TRUE)
  #load(file = 'Generated Quantities/T.train.RData')

# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # Model Run
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # because there are data quality issues, we rely on theoretical tuning. 
# first remove the variables we don't use in the models, and aggregate over these 
stratification_frame_augmented = stratification_frame[,lapply(.SD,function(x){sum(x)}),
                                                      by = c(colnames(stratification_frame)[
                                                        colnames(stratification_frame) %in% colnames(cbind(survey_Z,P=P,WtE=WtE))|
                                                          colnames(stratification_frame) %in% colnames(survey_T)]),
                                                      .SDcols = c("weights")
                                                      ]
# augment strat frame for each `source'
#stratification_frame_augmented$P = sample( levels(P),size = dim(stratification_frame_augmented)[1],replace = TRUE)
stratification_frame_augmented = foreach(p = levels(P),.combine = 'rbind') %do% cbind(stratification_frame_augmented,P = p,ID = 1:dim(stratification_frame_augmented)[1])
stratification_frame_augmented$weights = stratification_frame_augmented$weights/2



#########################################################################################################################################################
# # # # # Get outcome on logstic scale, take a random draw assuming logistic error in latent-variable model.
softmax = function(x){exp(x)/sum(exp(x))}
ilogit = function(x){exp(x)/(1+exp(x))}
logit = function(x){log(x/(1-x))}
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# First run a stupid RF on un-normalized data to get a prior on clusters
# Turnout
data.temp = cbind(T = T, survey_T)

hyperparameters.T = list(var.pop = dim(data.temp)[2]-1,
                         sample.fraction = 1/3,
                         min.node.size = 30,#0.1*dim(data.temp)[1],
                         num.trees_training = 1500)
 T_model_stupid = ranger (formula = T~.,
                          data = data.temp,
                          num.trees = hyperparameters.T$num.trees_training,
                          mtry = hyperparameters.T$var.pop,
                          sample.fraction = hyperparameters.T$sample.fraction,
                         importance = 'permutation',
                          write.forest = TRUE,
                          case.weights = T.weights$weightvec,
                          min.node.size = hyperparameters.T$min.node.size,
                          probability = FALSE,
                          classification = FALSE,
                          keep.inbag = TRUE,
                          replace = FALSE,
                          verbose = TRUE)
gc()
# How variable are teh predictions ? if not very, this is a good indication that the hyper-parameters need tuning. 
# Select relevant variables from strat from
X.test = stratification_frame_augmented[,colnames(stratification_frame_augmented) %in%  T_model_stupid $forest$independent.variable.names|
                                          colnames(stratification_frame_augmented) == "weights",with=FALSE]
# aggregate over same variables 
X.test = X.test[,lapply(.SD,sum),by = c(colnames(X.test)[-which(colnames(X.test)=="weights")]),.SDcols = c("weights")]
# is there enough variance in the predictions across cells ? if not, turnout is not going to have alot of impact on results 
T.pred.SF = predict(T_model_stupid,verbose = TRUE,data = X.test)
pdf(file = 'Plots/T_pred_cross_cell_density.pdf',width = 5,height = 5)
hist(T.pred.SF $predictions,main = 'turnout predictions over cells',xlab = 'probability of turnout',xlim = c(0,1))
dev.off()

#temp_VIM_p = importance_pvalues(T_model_stupid, method = "altmann", formula =  T~.,data = data.temp)
#save(temp_VIM_p ,file = 'Generated Quantities/VIM_T.RData',compress = TRUE)
load(file = 'Generated Quantities/VIM_T.RData')
#save(T_model_stupid,file = 'Generated Quantities/T_model_stupid.RData',compress = TRUE)
load(file = 'Generated Quantities/T_model_stupid.RData')


# Vote
survey_Z_T_pred = predict(T_model_stupid ,data = cbind(survey_Z,WtE = WtE,P = P))
survey_T_pred = survey_Z_T_pred$predictions

survey_Z_data = survey_Z[,!"PC_vote_choice_14"]
hyperparameters.V = list(var.pop = dim(cbind(V = V, survey_Z_data,WtE = WtE,P = P))[2]-1,
                         sample.fraction = 1/3,
                         min.node.size = 0.1*dim(survey_Z_data)[1],
                         num.trees_training = 1500)
V_model_stupid = list()
  for(j in levels(V)){
    V_model_prob_temp = ranger (formula = V_star~.,
                                data = cbind(V_star = ifelse(V==j,1,0), survey_Z_data,WtE = WtE,P = P),
                                num.trees = hyperparameters.V$num.trees_training,
                                mtry = hyperparameters.V$var.pop,
                                sample.fraction = hyperparameters.V$sample.fraction,
                                case.weights = survey_T_pred,
                                importance = 'permutation',
                                write.forest = TRUE,
                                min.node.size = hyperparameters.V$min.node.size,
                                probability = FALSE,
                                classification = FALSE,
                                keep.inbag = TRUE,
                                replace = FALSE,
                                verbose = TRUE)
    V_model_stupid = append(V_model_stupid,list(V_model_prob_temp))
    print(j)
}

save(V_model_stupid,file = 'Generated Quantities/V_model_stupid.RData',compress = TRUE)
load(file = 'Generated Quantities/V_model_stupid.RData')

#  temp_VIM_V_OTHER_p = importance_pvalues(V_model_stupid[[1]], 
#                                          method = "altmann", 
#                                          formula =  V_star~.,
#                                          data = cbind(V_star = ifelse(V=="OTHER",1,0), survey_Z_data,WtE = WtE,P = P))
#  temp_VIM_V_NDA_p = importance_pvalues(V_model_stupid[[2]], 
#                                          method = "altmann", 
#                                          formula =  V_star~.,
#                                          data = cbind(V_star = ifelse(V=="NDA",1,0), survey_Z_data,WtE = WtE,P = P))
#  temp_VIM_V_UPA_p = importance_pvalues(V_model_stupid[[3]], 
#                                        method = "altmann", 
#                                        formula =  V_star~.,
#                                        data = cbind(V_star = ifelse(V=="UPA",1,0), survey_Z_data,WtE = WtE,P = P))
#  save(temp_VIM_V_OTHER_p ,file = 'Generated Quantities/VIM_V_OTHER.RData',compress = TRUE)
#  save(temp_VIM_V_NDA_p ,file = 'Generated Quantities/VIM_V_NDA.RData',compress = TRUE)
#  save(temp_VIM_V_UPA_p ,file = 'Generated Quantities/VIM_V_UPA.RData',compress = TRUE)
load(file = 'Generated Quantities/VIM_V_OTHER.RData')
load(file = 'Generated Quantities/VIM_V_NDA.RData')
load(file = 'Generated Quantities/VIM_V_UPA.RData')


temp_VIM_p_small <- temp_VIM_p[which(temp_VIM_p[,2]<=0.2),]
temp_VIM_V_OTHER_p_small <- temp_VIM_V_OTHER_p[which(temp_VIM_V_OTHER_p[,2]<=0.2),]
temp_VIM_V_NDA_p_small <- temp_VIM_V_NDA_p[which(temp_VIM_V_NDA_p[,2]<=0.2),]
temp_VIM_V_UPA_p_small <- temp_VIM_V_UPA_p[which(temp_VIM_V_UPA_p[,2]<=0.2),]

pdf(file = 'Plots/VIM.pdf',width = 12.5, height = 18)
par(mfrow = c(4,1),mar = c(20,3,3,3))
#
plot(x = 1:dim(temp_VIM_p_small)[1],
     pch = 15,col = ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.025,'red',
                           ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.05 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.025,'lightcoral',
                                  ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.1 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.05,'grey',
                                         ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.2 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.10,'skyblue',
                                                ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.2,'blue','orange'
                                                ))))),
     y = temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),1],
     main = 'feature importance for turnout',ylab = 'VIM size',xaxt = "n",xlab = '',bty = 'n')


Map(axis,side = 1,at = 1:dim(temp_VIM_p_small)[1],
     labels = gsub("av_st_","", gsub("av_","states_",gsub("lag2","lag_two",gsub("lag3","lag_three",rownames(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),]))))),
     col.axis = ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.025,'black',
                  ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.05 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.025,'black',
                         ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.1 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.05,'darkgrey',
                                ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.2 & temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.10,'grey',
                                       ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]>0.2,'white','orange'
                                       ))))),
     font.axis =  ifelse(temp_VIM_p_small[rev(order(temp_VIM_p_small[,1])),2]<=0.025,2,1),
     las = 3,cex.axis = 1)
legend('topright',pch = c(15,15,15,15),legend = c("p <= 0.025","0.025 < p <= 0.05","0.05 < p <= 0.1","0.1 < p <= 0.2"),col = c("red","lightcoral",'grey','skyblue'))
#
#
plot(x = 1:dim(temp_VIM_V_NDA_p_small)[1],
     pch = 15,col = ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.025,'red',
                           ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.05 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.025,'lightcoral',
                                  ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.1 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.05,'grey',
                                         ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.2 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.10,'skyblue',
                                                ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.2,'blue','orange'
                                                ))))),
     y = temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),1],
     main = 'feature importance for NDA vote',ylab = 'VIM size',xaxt = "n",xlab = '',bty = 'n')


Map(axis,side = 1,at = 1:dim(temp_VIM_V_NDA_p_small)[1],
    labels = gsub("av_st_","", gsub("av_","states_",gsub("lag2","lag_two",gsub("lag3","lag_three",rownames(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),]))))),
    col.axis = ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.025,'black',
                      ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.05 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.025,'black',
                             ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.1 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.05,'darkgrey',
                                    ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.2 & temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.10,'grey',
                                           ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]>0.2,'white','orange'
                                           ))))),
    font.axis =  ifelse(temp_VIM_V_NDA_p_small[rev(order(temp_VIM_V_NDA_p_small[,1])),2]<=0.025,2,1),
    las = 3,cex.axis = 1)
legend('topright',pch = c(15,15,15,15),legend = c("p <= 0.025","0.025 < p <= 0.05","0.05 < p <= 0.1","0.1 < p <= 0.2"),col = c("red","lightcoral",'grey','skyblue'))
#
#
plot(x = 1:dim(temp_VIM_V_UPA_p_small)[1],
     pch = 15,col = ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.025,'red',
                           ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.05 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.025,'lightcoral',
                                  ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.1 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.05,'grey',
                                         ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.2 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.10,'skyblue',
                                                ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.2,'blue','orange'
                                                ))))),
     y = temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),1],
     main = 'feature importance for UPA vote',ylab = 'VIM size',xaxt = "n",xlab = '',bty = 'n')


Map(axis,side = 1,at = 1:dim(temp_VIM_V_UPA_p_small)[1],
    labels = gsub("av_st_","", gsub("av_","states_",gsub("lag2","lag_two",gsub("lag3","lag_three",rownames(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),]))))),
    col.axis = ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.025,'black',
                      ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.05 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.025,'black',
                             ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.1 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.05,'darkgrey',
                                    ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.2 & temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.10,'grey',
                                           ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]>0.2,'white','orange'
                                           ))))),
    font.axis =  ifelse(temp_VIM_V_UPA_p_small[rev(order(temp_VIM_V_UPA_p_small[,1])),2]<=0.025,2,1),
    las = 3,cex.axis = 1)
legend('topright',pch = c(15,15,15,15),legend = c("p <= 0.025","0.025 < p <= 0.05","0.05 < p <= 0.1","0.1 < p <= 0.2"),col = c("red","lightcoral",'grey','skyblue'))
#
#
plot(x = 1:dim(temp_VIM_V_OTHER_p_small)[1],
     pch = 15,col = ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.025,'red',
                           ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.05 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.025,'lightcoral',
                                  ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.1 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.05,'grey',
                                         ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.2 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.10,'skyblue',
                                                ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.2,'blue','orange'
                                                ))))),
     y = temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),1],
     main = 'feature importance for Other vote',ylab = 'VIM size',xaxt = "n",xlab = '',bty = 'n')


Map(axis,side = 1,at = 1:dim(temp_VIM_V_OTHER_p_small)[1],
    labels = gsub("av_st_","", gsub("av_","states_",gsub("lag2","lag_two",gsub("lag3","lag_three",rownames(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),]))))),
    col.axis = ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.025,'black',
                      ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.05 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.025,'black',
                             ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.1 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.05,'darkgrey',
                                    ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.2 & temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.10,'grey',
                                           ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]>0.2,'white','orange'
                                           ))))),
    font.axis =  ifelse(temp_VIM_V_OTHER_p_small[rev(order(temp_VIM_V_OTHER_p_small[,1])),2]<=0.025,2,1),
    las = 3,cex.axis = 1)
legend('topright',pch = c(15,15,15,15),legend = c("p <= 0.025","0.025 < p <= 0.05","0.05 < p <= 0.1","0.1 < p <= 0.2"),col = c("red","lightcoral",'grey','skyblue'))
#
dev.off()

# # # # #
# # # # #
# # # # #
# # # # #
n.sims = 1000
library(parallel)
# Select relevant variables from strat fram
dim(stratification_frame_augmented)
X.test = stratification_frame_augmented[,colnames(stratification_frame_augmented) %in% colnames( data.temp[,!"T"])|
                                         colnames(stratification_frame_augmented) %in% colnames( cbind(survey_Z_data,WtE = WtE,P = P))|
                                         colnames(stratification_frame_augmented) == "weights",with=FALSE]
# aggregate over same variables 
X.test = X.test[,lapply(.SD,sum),by = c(colnames(X.test)[-which(colnames(X.test)=="weights")]),.SDcols = c("weights")]
# Predict Turnout for stratification frame 
T_preds = predict(object = T_model_stupid,data = X.test,verbose = TRUE,type = 'response')
# Get errors fro 10% random sample 
# but break this calculation down into many smaller chunks, to ensure memory can handle it

             pool = 1:dim(X.test)[1]
                i = 0
T_model_mspe_list = data.table()

while(dim(T_model_mspe_list)[1]<=round(dim(X.test)[1]/10)){
  gc()
  start.time = Sys.time()
  i = i + 1
  print(paste("starting batch",i))
#
sample_id = sample(pool,size =round(dim(X.test)[1]/50)) #7000
     pool = pool[-which(pool %in% sample_id)]
#
T_model_mspe =  forestError::quantForestError(forest = T_model_stupid,
                                              X.train = data.temp[,!"T",with=FALSE],
                                              X.test = X.test[sample_id,colnames(X.test) %in% colnames(data.temp[,!"T"]),with=FALSE],
                                              Y.train = unlist(data.temp[,"T",with = FALSE]),
                                              n.cores = 4,
                                              what = c("bias","mspe"))


T_model_mspe_list = bind_rows(T_model_mspe_list,cbind(T_model_mspe,sample_id = sample_id))
#
end.time = Sys.time()
time.taken = difftime(end.time,start.time,units = 'mins')
#
print(paste("batch ended in mins",time.taken ))

# for batch size = 100, 2 cores, takes 0.5 mins
# for batch size = 500, 2 cores, takes 2.3 mins
# for batch size = 500, 4 cores, takes 1.2 mins
# for batch size = 1000, 8 cores, takes 1.6 mins
# for batch size = 1000, 12 cores, takes 1.5 mins
# for batch size = 1000, 16 cores, takes 1.6  mins
# for batch size = 2000, 16 cores, takes 2.9 mins
# for batch size = 5000, 16 cores, takes 6.8 mins
# for batch size = 7000, 16 cores, takes 8.6 mins

# increase in time is roughly linear at 7000size and 16 cores
}


# train MSPE prediction model 
T_model_stupid_mspe = ranger (formula = T_preds_mspe ~.,
                              data = cbind(T_preds_mspe = T_model_mspe_list$mspe,
                                           T_preds = T_model_mspe_list$pred,
                                           X.test[T_model_mspe_list$sample_id,]),
                              num.trees = 1000,
                              write.forest = TRUE,
                              probability = FALSE,
                              classification = FALSE,
                              keep.inbag = TRUE,
                              verbose = TRUE)


sum(T_preds$predictions==1|T_preds$predictions==0)
# predict turnout on X.Test
X.test$T_preds = T_preds$predictions
X.test$T_preds_mspe = predict(T_model_stupid_mspe,data = X.test)$predictions
# Predict Vote choice of the three parties for stratification frame 
for(w in max(WtE):min(WtE)){
  for(j in 1:nlevels(V)){
    
    print(paste("weeks:",w))
    print(paste("party:",levels(V)[j]))
    
    X.train = cbind( survey_Z_data,WtE = WtE,P = P)
    X.test.temp = cbind(X.test[,colnames(X.test) %in% colnames(cbind( survey_Z_data,WtE = WtE,P = P)),
                               with=FALSE],
                        WtE = w)
    temp_predictions = predict(V_model_stupid[[j]],verbose = TRUE,data = X.test.temp)
  
    # takes 567 seconds to do this - 10 minutes
    # Get errors fro 1% random sample 
    pool = 1:dim(X.test.temp)[1]
    i = 0
    V_model_mspe_list = data.table()
    while(dim(V_model_mspe_list)[1]<round(dim(X.test.temp)[1]/10)){
      start.time = Sys.time()
      i = i + 1
      print(paste("starting batch",i))
      #
      sample_id_10pct = sample(pool,size =round(dim(X.test.temp)[1]/50)) #7000
      pool = pool[-which(pool %in% sample_id_10pct)]
      #
    preds_list_V_sample_10pct = 
      forestError::quantForestError(V_model_stupid[[j]],
                                    X.train = X.train,
                                    X.test =  X.test.temp[sample_id_10pct,],
                                    Y.train = ifelse(V==levels(V)[j],1,0),
                                    what = c("mspe","bias"),
                                    n.cores = 4
      )
    V_model_mspe_list = bind_rows(V_model_mspe_list,cbind( preds_list_V_sample_10pct,sample_id_10pct = sample_id_10pct))
    #
    end.time = Sys.time()
    time.taken = difftime(end.time,start.time,units = 'mins')
    #
    print(paste("batch ended in mins",time.taken ))
    gc()
    }

    
    temp = data.table(temp_predictions$predictions)
    colnames(temp) = paste("pred.","week.",w,".party.",levels(V)[j],sep="")
    X.test = cbind(X.test,temp)
    
    
    temp = rep(NA,length(temp_predictions$predictions))
    temp[V_model_mspe_list$sample_id_10pct] =  V_model_mspe_list$bias
    temp = as.data.table(temp)
    colnames(temp) = paste("bias.","week.",w,".party.",levels(V)[j],sep="")
    X.test = cbind(X.test,temp)
    
    temp = rep(NA,length(temp_predictions$predictions))
    temp[V_model_mspe_list$sample_id_10pct] =  V_model_mspe_list$mspe
    temp = as.data.table(temp)
    colnames(temp) = paste("mspe.","week.",w,".party.",levels(V)[j],sep="")
    X.test = cbind(X.test,temp)
  } }
gc()

# predict out-of-sample mspe
missing <- which(apply(X.test,2,function(x){sum(is.na(x))>0}))
for(m in 1:length(missing)){
missing_train <- 
ranger (formula = pred ~.,
        data = cbind(pred = as.numeric(as.character(unlist(X.test[complete.cases(X.test[,missing[m],with=FALSE]),missing[m],with=FALSE]))),
                     X.test[complete.cases(X.test[,missing[m],with=FALSE]),!missing,with=FALSE]),
        num.trees = 1000,
        write.forest = TRUE,
        probability = FALSE,
        classification = FALSE,
        keep.inbag = TRUE,
        verbose = TRUE)
missing_pred = predict(object = missing_train, data = X.test)$predictions
X.test[,missing[m]] = missing_pred
}
# Now simulate !
# beta dist function for parameters 
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
save(X.test,file = 'Generated Quantities/Prediction_Frame.RData',compress = TRUE)
gc()
# simulate turnout
# mean

mu = X.test$T_preds 
mu_truncated = ifelse(mu==1,0.99999,ifelse(mu==0,0.00001,mu))
# variance
var_limit = mu_truncated*(1-mu_truncated) - 0.00001
var = X.test$T_preds_mspe
var_truncated = ifelse(var>=var_limit,var_limit,var)
# sims 
beta_params =as.data.table(estBetaParams(mu=mu_truncated,var=var_truncated))
turnout_sims = t(sapply(1:dim(beta_params)[1],function(x){rbeta(n = n.sims,shape1 = beta_params$alpha[x],shape2 = beta_params$beta[x])}))
# some missng values due to degenerate distribution - 1 with almost 0 variance - replace sims with 1 directly
for(i in 1:dim(turnout_sims)[2]){
  if(sum(is.na(turnout_sims[,i]))>0){
    turnout_sims[which(is.na(turnout_sims[,i]) ),i] = 1
  }
}
# simulate vote choice 
V_sims_list = list()
for(i in grep("pred.week",colnames( X.test))){
# mean
mu = as.numeric(as.character(unlist(X.test[,i,with=FALSE])))
mu_truncated = ifelse(mu==1,0.99999,ifelse(mu==0,0.00001,mu))
# variance
var_limit = mu_truncated*(1-mu_truncated) - 0.00001
var = as.numeric(as.character(unlist(X.test[,i+2,with=FALSE])))
var_truncated = ifelse(var>=var_limit,var_limit,var)
# sims 
beta_params =as.data.table(estBetaParams(mu=mu_truncated,var=var_truncated))
sims = t(sapply(1:dim(beta_params)[1],function(x){rbeta(n = n.sims,shape1 = beta_params$alpha[x],shape2 = beta_params$beta[x])}))
print(colnames(X.test)[i])
V_sims_list = append(V_sims_list ,list(sims))
} 

# effects of truncation on density 
pdf(file = 'Plots/correction_effect.pdf',width = 15,height = 15)
sample_id = sample(1:dim(X.test)[1],size = 10000)
par(mfrow = c(5,(length(grep("pred.week",colnames(X.test)))+1)/5))
plot(x = X.test$T_preds[sample_id],y = X.test$T_preds_mspe[sample_id],
     col = adjustcolor(ifelse(X.test$T_preds_mspe[sample_id]<(X.test$T_preds[sample_id]*(1-X.test$T_preds[sample_id])),'blue','red'),0.05),
     main = 'turnout',
     xlab = 'mu',ylab = 'mspe',bty = "n",
     xlim = c(min(X.test[sample_id,grep("pred.week",colnames(X.test)),with=FALSE]),max(X.test[sample_id,grep("pred.week",colnames(X.test)),with=FALSE])),
     ylim = c(min(as.numeric(as.character(unlist(X.test[sample_id,c(grep("T_preds.mspe",colnames(X.test)),grep("pred.week",colnames(X.test))) + 2,with=FALSE])))),max(as.numeric(as.character(unlist(X.test[sample_id,c(grep("T_preds.mspe",colnames(X.test)),grep("pred.week",colnames(X.test))) + 2,with=FALSE]))))))
x = seq(0,1,by = 0.01)
j = order(x)
y = x*(1-x)
lines(x = x[j], y = y[j],lty = 2,lwd = 2)
# 
for(i in 1:dim(X.test[,grep("pred.week",colnames(X.test)),with=FALSE])[2]){
plot(x = as.numeric(as.character(unlist(X.test[sample_id,grep("pred.week",colnames(X.test))[i],with=FALSE]))),
     y = as.numeric(as.character(unlist(X.test[sample_id,grep("pred.week",colnames(X.test))[i] + 2,with=FALSE]))),
     col =  adjustcolor(ifelse(X.test[sample_id,grep("pred.week",colnames(X.test))[i] + 2,with=FALSE]<(X.test[sample_id,grep("pred.week",colnames(X.test))[i],with=FALSE]*(1-X.test[sample_id,grep("pred.week",colnames(X.test))[i],with=FALSE])),'blue','red'),0.05),
     main = colnames(X.test)[grep("pred.week",colnames(X.test))][i],
     xlab = 'mu',ylab = 'mspe',bty = "n",xlim = c(min(X.test[sample_id,grep("pred.week",colnames(X.test)),with=FALSE]),max(X.test[sample_id,grep("pred.week",colnames(X.test)),with=FALSE])),
     ylim = c(min(as.numeric(as.character(unlist(X.test[sample_id,c(grep("T_preds.mspe",colnames(X.test)),grep("pred.week",colnames(X.test))) + 2,with=FALSE])))),max(as.numeric(as.character(unlist(X.test[sample_id,c(grep("T_preds.mspe",colnames(X.test)),grep("pred.week",colnames(X.test))) + 2,with=FALSE]))))))
x = seq(0,1,by = 0.01)
j = order(x)
y = x*(1-x)
lines(x = x[j], y = y[j],lty = 2,lwd = 2)
}
dev.off()

# Aggregate 
library(mc2d)
# # # # #
# # # # #
# # # # #
# # # # #
# identify redundant cells for turnout 
T.redundant.cells = which(duplicated(X.test[,colnames(X.test) %in% c("weights",T_model_stupid$forest$independent.variable.names),with=FALSE]))
# identify redundant cells for vote-choice
V.redundant.cells =  which(duplicated(X.test[,colnames(X.test) %in% c("weights",V_model_stupid[[1]]$forest$independent.variable.names),with=FALSE]))
V.redundant.cells_list = c()
for(q in 1:length(unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames( X.test)[grep("pred.week",colnames( X.test))]))))){
  V.redundant.cells_list = c(V.redundant.cells_list,V.redundant.cells + (q-1)*dim(X.test)[1] )
}

table(X.test[which(is.na(turnout_sims[,1])),]$T_preds)
apply(turnout_sims,2,function(x){sum(is.na(x))})

#
NAT_RESULTS = list()
STATE_RESULTS = list()
ZONE_RESULTS = list()
for(draw in 1:n.sims){
  # start with first draw
  print(paste("draw:",draw))
  start.time.draw = Sys.time()
  print(paste("start time for this draw :",start.time.draw))
  start.time = Sys.time()


PREDS_temp = data.table()
for(w_id in sort(as.integer(as.factor(unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames( X.test)[grep("pred.week",colnames( X.test))]))))))){
# store predictions
# vote preds by party 
temp = cbind(V_sims_list[[(w_id-1)*3+1]][,draw],V_sims_list[[(w_id-1)*3+2]][,draw],V_sims_list[[ (w_id-1)*3+3 ]][,draw])
colnames(temp) =  levels(V)
  
PREDS_temp_WtE = as.data.table(cbind(T = turnout_sims[,draw],
                                     temp,
                                     WtE =  unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames( X.test)[grep("pred.week",colnames( X.test))])))[w_id]))
PREDS_temp = rbind(PREDS_temp,PREDS_temp_WtE)
}
# # # # #
RESULTS = PREDS_temp#append(RESULTS,list(cbind(PREDS_temp ,draw = draw)))
#save(RESULTS ,file = 'Result Files/RESULTS.RData',compress = TRUE)
# # # AGGREGATE # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# sto

# for vote choice 
RESULTS[,levels(V)] = as.data.table(t(apply(RESULTS [,levels(V),with =FALSE],1,function(x){x/sum(x)})))
# first aggregate

# # # CHALLENGE: aggregating over redundant cells will shrink variance of simulations by shrinking each sim. toward the overall mean. 

# Turnout Aggregation - this only metters when we want nice confidence intervals around T, won't change point estimate 
RESULTS_T = cbind( foreach(w = max(WtE):min(WtE) ,.combine = 'rbind') %do% (X.test[-T.redundant.cells,1:which(colnames(X.test)=="weights"),with=FALSE]), RESULTS[which(RESULTS$WtE==6),][-T.redundant.cells,])
# Turnout sims incorporate uncertainty about the parameter; let us now simulate a binomial distribution sample for each simulated parameter
RESULTS_T$T_W = rbinom(n = dim( RESULTS_T)[1],prob = RESULTS_T$T,size = round(RESULTS_T$weights))

# Vote-Choice Aggregation
RESULTS = cbind( foreach(w = max(WtE):min(WtE) ,.combine = 'rbind') %do% (X.test[-V.redundant.cells,1:which(colnames(X.test)=="weights"),with=FALSE]), 
                 RESULTS[-V.redundant.cells_list,])
# Turnout sims incorporate uncertainty about the parameter; let us now simulate a binomial distribution sample for each simulated parameter
RESULTS$T_W = rbinom(n = dim( RESULTS)[1],prob = RESULTS$T,size = round(RESULTS$weights))
# vote choice sims incorporate uncertainty about conditional mean; let's simulate from it 
TEMP = rbinom(n = dim( RESULTS)[1],size = RESULTS$T_W,prob = as.numeric(unlist(RESULTS[,levels(V)[1],with=FALSE])))
TEMP = cbind(TEMP, rbinom(n = dim( RESULTS)[1],size = RESULTS$T_W-TEMP,prob = as.numeric(unlist(RESULTS[,levels(V)[2],with=FALSE]))))
TEMP = cbind(TEMP,  RESULTS$T_W-rowSums(TEMP))
colnames(TEMP) = paste(levels(V),"W",sep="_")
RESULTS = cbind(RESULTS,TEMP)

gc()
# STATE-LEVEL
STATE_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                                  by= c("WtE","states","zones"),
                                  .SDcols = c("weights","T_W",colnames(TEMP))]
STATE_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% STATE_TEMP[,colnames(TEMP)[j],with=FALSE]/STATE_TEMP$T_W)
STATE_TEMP$T_W = STATE_TEMP$T_W/STATE_TEMP$weights
STATE_RESULTS = append(STATE_RESULTS,list(STATE_TEMP))
save(STATE_RESULTS,file = 'Generated Quantities/STATE_RESULTS.RData',compress = TRUE)
gc()
#
#
#
# ZONE-LEVEL
ZONE_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                             by= c("WtE","zones"),
                             .SDcols = c("weights","T_W",colnames(TEMP))]
ZONE_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% ZONE_TEMP[,colnames(TEMP)[j],with=FALSE]/ZONE_TEMP$T_W)
ZONE_TEMP$T_W = ZONE_TEMP$T_W/ZONE_TEMP$weights
# append and save results 
ZONE_RESULTS = append(ZONE_RESULTS,list( ZONE_TEMP))
save(ZONE_RESULTS,file = 'Generated Quantities/ZONE_RESULTS.RData',compress = TRUE)
#
#
#
gc()
# NAT-LEVEL


NAT_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                            by= c("WtE"),
                            .SDcols = c("weights","T_W",colnames(TEMP))]
NAT_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% NAT_TEMP[,colnames(TEMP)[j],with=FALSE]/NAT_TEMP$T_W)

# NAT-LEVEL Turnout Slim
NAT_T_TEMP = RESULTS_T[,lapply(.SD,function(x){sum(x)}),
                   by= c("WtE"),
                   .SDcols = c("weights","T_W")]
NAT_T_TEMP$T_W = NAT_T_TEMP$T_W/NAT_T_TEMP$weights
NAT_TEMP$T_W = rep(NAT_T_TEMP$T_W,dim(NAT_TEMP)[1])

if(!all(rowSums(NAT_TEMP[,paste(levels(V),"_W",sep=""),with=FALSE])==1)){print("trouble - nat. estimates not summing to 1")}
#
#
#
# predict each cell onwards via random walk
RTEMP = NAT_TEMP
# calculate random walk sd with weeks known thus far
NAT_TEMP_temp_sd = RTEMP[,lapply(.SD,sd),.SDcols = colnames(TEMP) ]
while(min(RTEMP$WtE)>1){
  
  TEMP2 = 
  as.data.table(t(
    sapply(1:nlevels(V),
           function(x){rnorm(n = 1,
                             mean = as.numeric(as.character(unlist(RTEMP[which(RTEMP$WtE==min(RTEMP$WtE)),colnames(TEMP),with=FALSE])))[x], 
                             sd = as.numeric(as.character(unlist(NAT_TEMP_temp_sd)))[x])
           } ) ))
  colnames(TEMP2) = colnames(TEMP)
  #TEMP2 = TEMP2/sum(TEMP2)
  
  RTEMP = 
  rbind(
    RTEMP,
    data.table(
      WtE = min(RTEMP$WtE)-1,
      weights = RTEMP$weights[which(RTEMP$WtE==min(RTEMP$WtE))],
      T_W = RTEMP$T_W[which(RTEMP$WtE==min(RTEMP$WtE))],
      TEMP2)
  )

}
gc()
# append and save results 
NAT_RESULTS = append(NAT_RESULTS,list( RTEMP))
save(NAT_RESULTS,file = 'Generated Quantities/NAT_RESULTS.RData',compress = TRUE)
gc()
#
#
#
end.time.draw = Sys.time()
print(paste('time taken for this draw:',difftime(end.time.draw,start.time.draw,units = 'mins'),"mins"))
}

load(file = 'Generated Quantities/NAT_RESULTS.RData')

# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # Load election results for comparison
# # # # # 
# # # # # 
# # # # # 
# # # # # 
res_obs = read.csv("Generated Quantities/2019_Results.csv",na.strings = c("",NA))
# # # #
# # # # PC-level results 
# # # #
pc_res_obs = data.table(res_obs[,c("PC_name","state","alliance","Pct.of.Votes")])
# consolidate alliances by PC
pc_res_obs = unique(pc_res_obs[,Pct.of.Votes:=sum(Pct.of.Votes),by = c("PC_name","state","alliance")])
# wide-format 
pc_res_obs = reshape(pc_res_obs,timevar = "alliance",idvar = c("PC_name","state"),direction = 'wide')
# if there is an NA here, set to 0
pc_res_obs$Pct.of.Votes.NDA = ifelse(is.na(pc_res_obs$Pct.of.Votes.NDA),0,pc_res_obs$Pct.of.Votes.NDA)
pc_res_obs$Pct.of.Votes.OTHER = ifelse(is.na(pc_res_obs$Pct.of.Votes.NDA),0,pc_res_obs$Pct.of.Votes.OTHER)
pc_res_obs$Pct.of.Votes.UPA = ifelse(is.na(pc_res_obs$Pct.of.Votes.NDA),0,pc_res_obs$Pct.of.Votes.UPA)
# assigne a `zone` ID 
pc_res_obs = merge(pc_res_obs,unique(stratification_frame [,c("states","zones")]),by.x ="state", by.y = "states",all=TRUE)
# # # #
# # # # State-level results 
# # # #
state_res_obs = unique(res_obs[,c("state","alliance","State.Alliance.Pct.of.Votes")])
# turnout 
state_T_res = read.csv("Generated Quantities/Turnout_2019_state.csv",na.strings = c("",NA))
state_res_obs_temp = reshape(state_res_obs,idvar = "state", timevar = "alliance",direction = 'wide')
state_res_obs_temp[,-1] = state_res_obs_temp[,-1] /100
state_res = merge(state_T_res,state_res_obs_temp,by.x = "State",by.y = "state")
names(state_res)[which(names(state_res) =="State.Alliance.Pct.of.Votes.NDA")] = "V_NDA_pred"
names(state_res)[which(names(state_res) =="State.Alliance.Pct.of.Votes.OTHER")] = "V_OTHER_pred"
names(state_res)[which(names(state_res) =="State.Alliance.Pct.of.Votes.UPA")] = "V_UPA_pred"
state_res  = as.data.table(state_res )
state_res = state_res[,!c("Electors","Total_Votes")]
# if there is an NA here, set to 0
state_res$V_NDA_pred = ifelse(is.na(state_res$V_NDA_pred),0,state_res$V_NDA_pred)
state_res$V_OTHER_pred = ifelse(is.na(state_res$V_OTHER_pred),0,state_res$V_OTHER_pred)
state_res$V_UPA_pred = ifelse(is.na(state_res$V_UPA_pred),0,state_res$V_UPA_pred)
# assigne a `zone` ID 
state_res_obs = merge(state_res,unique(stratification_frame [,c("states","zones")]),by.x ="State", by.y = "states",all=TRUE)
names(state_res_obs) = c("states","T_pred","V_NDA_pred","V_OTHER_pred","V_UPA_pred","zones")

# # # #
# # # # National-level results 
# # # #
nat_res_obs = unique(res_obs[,c("alliance","Nat.Alliance.Pct.of.Votes")])
# turnout
nat_T_res = as.data.table(state_T_res)
nat_T_res = nat_T_res[,lapply(.SD,sum),.SDcols = c("Electors","Total_Votes")]
nat_T_res$T_pred = nat_T_res$Total_Votes/nat_T_res$Electors
nat_res_obs = cbind(nat_T_res,data.frame(V_NDA_pred = nat_res_obs$Nat.Alliance.Pct.of.Votes[nat_res_obs$alliance=="NDA"],
                                         V_UPA_pred = nat_res_obs$Nat.Alliance.Pct.of.Votes[nat_res_obs$alliance=="UPA"],
                                         V_OTHER_pred = nat_res_obs$Nat.Alliance.Pct.of.Votes[nat_res_obs$alliance=="OTHER"]))
nat_res_obs[,c("V_NDA_pred","V_UPA_pred","V_OTHER_pred")] = nat_res_obs[,c("V_NDA_pred","V_UPA_pred","V_OTHER_pred")]/100
nat_res_obs = nat_res_obs[,c("T_pred","V_NDA_pred","V_UPA_pred","V_OTHER_pred")]
# # # #
# # # # Zone-level results 
# # # #
zones_res_obs = merge(res_obs,unique(stratification_frame [,c("states","zones")]),by.x = "state",by.y = 'states',all=TRUE) 
zones_res_obs = as.data.table(zones_res_obs)
temp_1 = zones_res_obs[,lapply(.SD,function(x){sum(x)}),by= c("zones",'alliance'),.SDcols = c('Total.Votes')]
temp_2 = zones_res_obs[,lapply(.SD,function(x){sum(x)}),by= c("zones"),.SDcols = c('Total.Votes')]
names(temp_2) = c('zones','zone_T_count')
temp_3 = merge(state_T_res[,c("State","Electors")], unique(stratification_frame [,c("states","zones")]),by.x ="State",by.y = "states",all=TRUE)
temp_3 = as.data.table(temp_3)[,lapply(.SD,function(x){sum(x)}),by = 'zones',.SDcols = 'Electors']
temp_1 = merge(temp_1,temp_2,by = 'zones',all=TRUE)             
temp_1 = merge(temp_1 ,temp_3,by = 'zones',all=TRUE)
temp_1 $T = temp_1 $zone_T_count/temp_1 $Electors
temp_1$Zones.Pct.of.Votes = temp_1$Total.Votes/temp_1$zone_T_count
temp_1 = temp_1[,!c("Total.Votes","zone_T_count","Electors")]
# wide-format 
zones_res_obs = reshape(temp_1,timevar = "alliance",idvar = c("T","zones"),direction = 'wide')
names(zones_res_obs) = c("zones","T_pred","V_OTHER_pred","V_NDA_pred","V_UPA_pred")
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# NATIONAL RESULTS PLOT 
NAT_RES_TABLE = foreach(i = 1:length(NAT_RESULTS),.combine = 'rbind') %do% cbind(NAT_RESULTS[[i]],draw = i)

pdf(file = 'Plots/NAT_vote_share_over_time.pdf',height = 7.5,width = 7.5)
plot(y = unlist(NAT_RES_TABLE[NAT_RES_TABLE$draw==1,"OTHER_W"]),
     x = unlist(NAT_RES_TABLE[NAT_RES_TABLE$draw==1,"WtE"]),
     pch = NA,bty = "n",main = 'national vote over time',xlab = 'weeks to end of election',ylab = 'estimated prob.',
     xlim = c(max(WtE),1),
     xaxt = "n",
     ylim = c(.2,.55))
axis(side = 1,at = max(WtE):1,labels =(max(WtE):1)-1)
for(i in 1:max(NAT_RES_TABLE$draw)){ for(j in 1:nlevels(V)){
  lines(
    y = unlist(NAT_RES_TABLE[NAT_RES_TABLE$draw==i,paste(levels(V)[j],"W",sep="_"),with = FALSE]),
    x = unlist(NAT_RES_TABLE[NAT_RES_TABLE$draw==i,"WtE"]),
    col = adjustcolor(c("darkgrey","orange","skyblue")[j],0.25)
  )
} }
Y = NAT_RES_TABLE[,lapply(.SD,function(x){mean(x)}),by = 'WtE',.SDcols = paste(levels(V),"W",sep="_")]
for(j in 1:nlevels(V)){
  lines(x = Y$WtE,
        y = as.numeric(as.character(unlist(Y[,paste(levels(V)[j],"W",sep="_"),with=FALSE]))),
        col = c("black","red","blue")[j],lwd = 2)
}
abline(h = nat_res_obs$V_OTHER_pred,col = 'black',lty = 2,lwd = 1.5)
abline(h = nat_res_obs$V_NDA_pred,col = 'red',lty = 2,lwd = 1.5)
abline(h = nat_res_obs$V_UPA_pred,col = 'blue',lty = 2,lwd = 1.5)
abline(v = min(WtE))
legend("topleft",
       legend = c(paste("NDA error:",
                        round(nat_res_obs$V_NDA_pred-mean(as.numeric(as.character(unlist(Y[,paste("NDA","W",sep="_"),with=FALSE])))[Y$WtE==6]),
                              3)),
                  paste("UPA error:",
                        round(nat_res_obs$V_UPA_pred-mean(as.numeric(as.character(unlist(Y[,paste("UPA","W",sep="_"),with=FALSE])))[Y$WtE==6]),
                              3)),
                  paste("OTHER error:",
                        round(nat_res_obs$V_OTHER_pred-mean(as.numeric(as.character(unlist(Y[,paste("OTHER","W",sep="_"),with=FALSE])))[Y$WtE==6]),
                              3))
                  ),
       bty = "n")
dev.off()


sd(NAT_RES_TABLE[NAT_RES_TABLE$WtE==1,]$NDA_W)*3
sd(NAT_RES_TABLE[NAT_RES_TABLE$WtE==1,]$UPA_W)*3
sd(NAT_RES_TABLE[NAT_RES_TABLE$WtE==1,]$OTHER_W)*3


# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# STATE RESULTS PLOT 
STATE_RES_TABLE = foreach(i = 1:length(STATE_RESULTS),.combine = 'rbind') %do% cbind(STATE_RESULTS[[i]],draw = i)
# ok to use wte = 6 as point estimate because only uncertainty added later 
STATE_RES_TABLE = STATE_RES_TABLE[which(STATE_RES_TABLE$WtE==6),]
# average over draws 
STATE_RES_TABLE = STATE_RES_TABLE[,lapply(.SD,function(x){mean(x)}),by = c("states","zones"),.SDcols = c("T_W","OTHER_W","NDA_W","UPA_W")]
#
pdf(file = 'Plots/State_Results.pdf',width = 10,height = 10)
par(mfrow = c(2,2))
for(i in c("T_pred","NDA_pred","UPA_pred","OTHER_pred")){
  x = cbind(T_pred = STATE_RES_TABLE$T_W,
            NDA_pred = STATE_RES_TABLE$NDA_W,
            OTHER_pred = STATE_RES_TABLE$OTHER_W,
            UPA_pred = STATE_RES_TABLE$UPA_W)[,i]
  y = as.numeric(as.character(unlist(state_res_obs[match(STATE_RES_TABLE$states,state_res_obs$states),grepl(i,names(state_res_obs)),with=FALSE])))
  plot(x = x,
       y = y,
       xlab = 'predicted',ylab = 'observed',
       bty = "n",
       main = gsub("_pred","",i),
       ylim = c(0,1),xlim = c(0,1)
  )
  abline(0,1)
  legend("topleft",
         legend = c(
           paste("cor:",round(cor(x = x, y = y),3)),
           paste("rmse:",round( sqrt(mean((100*y - 100*x)^2)),2)),
           paste("mae:",round( mean(abs(100*y - 100*x)),2))
         ),
         bty = "n")
  fit <- loess(formula = y ~ x)
  j <- order(x)
  lines(y=fit$fitted[j],x=x[j],col = "red")
}
dev.off()
# # # # #
# # # # #
# # # # #
# # # # #
# # # # #
# ZONE RESULTS PLOT 
ZONE_RES_TABLE = foreach(i = 1:length(ZONE_RESULTS),.combine = 'rbind') %do% cbind(ZONE_RESULTS[[i]],draw = i)
# ok to use wte = 6 as point estimate because only uncertainty added later 
ZONE_RES_TABLE = ZONE_RES_TABLE[which(ZONE_RES_TABLE$WtE==6),]
# average over draws 
ZONE_RES_TABLE = ZONE_RES_TABLE[,lapply(.SD,function(x){mean(x)}),by = c("zones"),.SDcols = c("T_W","OTHER_W","NDA_W","UPA_W")]
#
pdf(file = 'Plots/Zone_Results.pdf',width = 10,height = 10)
par(mfrow = c(2,2))
for(i in c("T_pred","NDA_pred","UPA_pred","OTHER_pred")){
  x = cbind(T_pred = ZONE_RES_TABLE$T_W,
            NDA_pred = ZONE_RES_TABLE$NDA_W,
            OTHER_pred = ZONE_RES_TABLE$OTHER_W,
            UPA_pred = ZONE_RES_TABLE$UPA_W)[,i]
  y = as.numeric(as.character(unlist(zones_res_obs[match(ZONE_RES_TABLE$zones,zones_res_obs$zones),grepl(i,names(zones_res_obs)),with=FALSE])))
  plot(x = x,
       y = y,
       xlab = 'predicted',ylab = 'observed',
       bty = "n",
       main = gsub("_pred","",i),
       ylim = c(0,1),xlim = c(0,1)
  )
  abline(0,1)
  legend("topleft",
         legend = c(
           paste("cor:",round(cor(x = x, y = y),3)),
           paste("rmse:",round( sqrt(mean((100*y - 100*x)^2)),2)),
           paste("mae:",round( mean(abs(100*y - 100*x)),2))
         ),
         bty = "n")
  fit <- lm(formula = y ~ x)
  j <- order(x)
  lines(y=fit$fitted[j],x=x[j],col = "red")
}
dev.off()
# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # Constituency-level predictions
# # # # # 
# # # # # 
# # # # # 
# # # # # 
HIST = fread(file = 'Generated Quantities/HIST.csv')
H_temp = HIST[which(HIST$Year==2014),c("State","Zones","PC_name","Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")]
USwing_Winner_list = data.table()
delta_list = c()
for(i in 1:n.sims){
  x = cbind(T_W = NAT_RES_TABLE$T_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6],
            V_NDA_pred =  NAT_RES_TABLE$NDA_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6],
            V_OTHER_pred =  NAT_RES_TABLE$OTHER_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6],
            V_UPA_pred =  NAT_RES_TABLE$UPA_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6] )
  y = as.numeric(as.character(unlist(nat_res_obs)))[match(colnames(x),colnames(nat_res_obs))]
  x = as.numeric(as.character(unlist(x)))
  
  delta = 100*x[-1]-unique(HIST[which(HIST$Year==2014),c("NAT_Percentage.Votes.NDA","NAT_Percentage.Votes.UPA","NAT_Percentage.Votes.OTHER")])
  USwing_Preds = cbind(H_temp[,c("State","Zones","PC_name")],
                       foreach(i = 1:dim(H_temp)[1],.combine = 'rbind') %do% 
                         (H_temp[i,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] + delta))
  USwing_Winner = apply(H_temp[,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] ,1,function(x){c("NDA","UPA","OTHER")[which(x==max(x))]})
  USwing_Winner_list  = cbind(USwing_Winner_list ,USwing_Winner)
  delta_list = rbind(delta_list,delta)
}

# # # # Calculate new seat total simulations
temp_xx = foreach(k = 1:n.sims,.combine = 'cbind') %do% 
  c("NDA","UPA","OTHER")[apply(
    as.matrix(t(  
      sapply(1:dim(H_temp)[1],function(x){
        as.numeric(as.character(unlist(
          (H_temp[x,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] + delta_list[k,])
        )))
      })
    )),
    1,function(z){which(z==max(z))})
    ]
# Add polls
polls = fread(file = 'Seats_Polls_India.csv')

temp19 = polls[which(polls=="2019"),]
temp19$wte = difftime(as.Date("19-05-2019","%d-%m-%Y"),as.Date(temp19$max_date,"%d-%B-%y"))
temp19 = as.data.table(temp19)
temp19[,min_date :=min(wte),by = c("Stats")]
temp19 = temp19[which(temp19$wte==temp19$min_date),]




error_table = 
data.table(
pollster = c('Cerina & Duch 2019','2014 Res.',temp19$Stats),
NDA_estimate=  c(round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),])),
                 table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                          c("Vote_Share_Percentage.NDA",
                                                            "Vote_Share_Percentage.UPA",
                                                            "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"], temp19$NDA),
NDA_error = c(354 - round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),])),354-table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                                                                                                     c("Vote_Share_Percentage.NDA",
                                                                                                                                                       "Vote_Share_Percentage.UPA",
                                                                                                                                                       "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"],354 - temp19$NDA),
UPA_estimate = c(round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),])),table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                          c("Vote_Share_Percentage.NDA",
                                                            "Vote_Share_Percentage.UPA",
                                                            "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"],temp19$UPA),
UPA_error = c(99-round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),])),99-table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                                                                                                 c("Vote_Share_Percentage.NDA",
                                                                                                                                                   "Vote_Share_Percentage.UPA",
                                                                                                                                                   "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"],99 - temp19$UPA),
OTHER_estimate = c(round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),])),table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                            c("Vote_Share_Percentage.NDA",
                                                              "Vote_Share_Percentage.UPA",
                                                              "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["OTHER"],temp19$OTHER),
OTHER_error = c(89-round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),])),
                89-table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["OTHER"],
                89 - temp19$OTHER),
NDA_UPA_Lead_Estimate = c(round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),])-mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),])),
                          table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                  c("Vote_Share_Percentage.NDA",
                                                                    "Vote_Share_Percentage.UPA",
                                                                    "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"] - 
                          table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                  c("Vote_Share_Percentage.NDA",
                                                                    "Vote_Share_Percentage.UPA",
                                                                    "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"],
                          temp19$NDA-temp19$UPA),
NDA_UPA_Lead_Error = (354-99)-c(round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),])-mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),])),
                       table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                               c("Vote_Share_Percentage.NDA",
                                                                 "Vote_Share_Percentage.UPA",
                                                                 "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"] - 
                         table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                 c("Vote_Share_Percentage.NDA",
                                                                   "Vote_Share_Percentage.UPA",
                                                                   "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"],
                       temp19$NDA-temp19$UPA))
  


error_table$AbsoluteError = abs(error_table$NDA_error) + abs(error_table$UPA_error) + abs(error_table$OTHER_error)
error_table$DtE = c(as.numeric(as.character(unlist(difftime(as.Date("19-05-2019","%d-%m-%Y"),as.Date("10-04-2019","%d-%m-%Y"))))),NA,
                    as.numeric(as.character(unlist(temp19$wte))))


library(xtable)
print(xtable(error_table[,c("pollster","DtE","NDA_estimate","NDA_error","UPA_estimate","UPA_error","OTHER_estimate","OTHER_error","AbsoluteError","NDA_UPA_Lead_Estimate","NDA_UPA_Lead_Error")]), include.rownames=FALSE)




#
pdf(file = 'Plots/uswing_seats_distribution.pdf',width = 11.5,height = 8.5)
# # # # # # # # # #
par(mfrow = c(3,1),xpd = TRUE,oma = c(0,0,0,0),mar = c(7,5,2,2))
# # # # # # # # # #
hist(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),],
     main = 'NDA',xlab = '',breaks = 10,
     xlim = c(min(as.numeric(unlist(apply(temp_xx,2,table)))) ,
              max(as.numeric(unlist(apply(temp_xx,2,table)))) + 50 ),
     border = "white",col = 'darkgrey',xaxt = "n"
)
axis(side = 1,at = seq(0,500,by = 25),labels = seq(0,500,by = 25),xpd = FALSE)
abline(v = 272,col = 'black',xpd = FALSE)
abline(v = 354,col = 'red',lty = 2,lwd = 2,xpd = FALSE)
abline(v = mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),]),col = 'black',lty = 2,lwd = 2,xpd = FALSE)
abline(v = table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                   c("Vote_Share_Percentage.NDA",
                                                     "Vote_Share_Percentage.UPA",
                                                     "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"],
       col = 'black',lty = 3,lwd = 2,xpd = FALSE)
legend("topleft",
       lty = c(2,2,3,1),col = c("red","black","black","black"),
       legend = c(paste("2019 observed:",354),
                  paste("point estimate:",round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),]))),
                  paste("2014 observed:",table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                                 c("Vote_Share_Percentage.NDA",
                                                                                   "Vote_Share_Percentage.UPA",
                                                                                   "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["NDA"]),
                  paste("Pr(majority):",
                        sum(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),]>272)/n.sims
                  )),
       cex = 1.25,
       bty = "n")
points(y = rep(25,dim(temp19)[1]),
       x = temp19$NDA,pch = as.integer(as.factor(temp19 $Stats))-1,
       cex = 1.5)

# # # # # # # # # #
hist(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),],
     main = 'UPA',xlab = '',
     xlim = c(min(as.numeric(unlist(apply(temp_xx,2,table)))),
              max(as.numeric(unlist(apply(temp_xx,2,table)))) + 50),
     border = 'white',col = 'darkgrey',xaxt = "n",breaks = 10)
axis(side = 1,at = seq(0,500,by = 25),labels = seq(0,500,by = 25),xpd = FALSE)
abline(v = 272,col = 'black',xpd = FALSE)
abline(v = 99,col = 'red',lty = 2,lwd = 2,xpd = FALSE)
abline(v = mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),]),col = 'black',lty = 2,lwd = 2,xpd = FALSE)
abline(v = table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                   c("Vote_Share_Percentage.NDA",
                                                     "Vote_Share_Percentage.UPA",
                                                     "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"],
       col = 'black',lty = 3,lwd = 2,xpd = FALSE)
legend("topright",
       lty = c(2,2,3,1),col = c("red","black","black","black"),
       legend = c(paste("2019 observed:",99),
                  paste("point estimate:",round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),]))),
                  paste("2014 observed:",table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                                 c("Vote_Share_Percentage.NDA",
                                                                                   "Vote_Share_Percentage.UPA",
                                                                                   "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["UPA"]),
                  paste("Pr(majority):",
                        sum(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),]>272)/n.sims
                  )),
       cex = 1.25,
       bty = "n")
points(y = rep(25,dim(temp19)[1]),
       x = temp19$UPA,pch = as.integer(as.factor(temp19 $Stats))-1,
       cex = 1.5)
# # # # # # # # # #
hist(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),],
     main = 'OTHER',xlab = '',
     xlim = c(min(as.numeric(unlist(apply(temp_xx,2,table)))),
              max(as.numeric(unlist(apply(temp_xx,2,table)))) + 50),
     border = 'white',col = 'darkgrey',xaxt = "n",breaks = 10
)
axis(side = 1,at = seq(0,500,by = 25),labels = seq(0,500,by = 25),xpd = FALSE)

abline(v = 272,col = 'black',xpd = FALSE)
abline(v = 89,col = 'red',lty = 2,lwd = 2,xpd = FALSE)
abline(v = mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),]),col = 'black',lty = 2,lwd = 2,xpd = FALSE)
abline(v = table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                   c("Vote_Share_Percentage.NDA",
                                                     "Vote_Share_Percentage.UPA",
                                                     "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["OTHER"],
       col = 'black',lty = 3,lwd = 2,xpd = FALSE)
legend("topright",
       lty = c(2,2,3,1),col = c("red","black","black","black"),
       legend = c(paste("2019 observed:",89),
                  paste("point estimate:",round(mean(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),]))),
                  paste("2014 observed:",table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,
                                                                                 c("Vote_Share_Percentage.NDA",
                                                                                   "Vote_Share_Percentage.UPA",
                                                                                   "Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["OTHER"]),
                  paste("Pr(majority):",
                        sum(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),]>272)/n.sims
                  )),
       cex = 1.25,
       bty = "n")
points(y = rep(25,dim(temp19)[1]),
       x = temp19$OTHER,pch = as.integer(as.factor(temp19 $Stats))-1,
       cex = 1.5)
######
legend(x = 65,y = -70,
       legend = temp19$Stats,
       pch = as.integer(as.factor(temp19 $Stats))-1,
       xpd = TRUE,
       horiz = TRUE,
       cex = 1,
       bty = "n"
)
dev.off()

range(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="OTHER"),])
 range(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="UPA"),])

range(apply(temp_xx,2,table)[which(rownames(apply(temp_xx,2,table))=="NDA"),])
141-125

pdf(file = 'Plots/NAT_turnout_density.pdf',width = 5.5,height = 4)
hist(NAT_RES_TABLE$T_W[NAT_RES_TABLE$WtE==1],main = 'national turnout density',xlab = 'turnout proportion',ylab = 'frequency',
     xlim = c(.65,.85),border = 'white',col = 'darkgrey')
abline(v = mean(NAT_RES_TABLE$T_W[NAT_RES_TABLE$WtE==1]),col = 'black',lwd = 1)
abline(v = nat_res_obs$T_pred,col = 'black',lty = 2)
legend("topright",legend = c(paste("T obs:",round(nat_res_obs$T_pred,3)),
                             paste("T pred mean:",round(mean(NAT_RES_TABLE$T_W[NAT_RES_TABLE$WtE==6]),3))
),
lty = c(2,1),
bty = "n")
dev.off()

# # # # # 
# # # # # 
# # # # # 
# # # # # 
# # # # # Constituency-level predictions - no model
# # # # # 
# # # # # 
# # # # # 
# # # # # 
HIST = fread(file = 'Generated Quantities/HIST.csv')
H_temp = HIST[which(HIST$Year==2014),c("State","Zones","PC_name","Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")]
USwing_Winner_list = data.table()
delta_list = c()


NAT_RES_TABLE = prop.table(table(Online_surveys_imp$PC_vote_choice[which(Online_surveys_imp$turnout==1 & Online_surveys_imp$weeks==6)]))
NAT_RES_TABLE$T = mean(Online_surveys_imp$turnout[Online_surveys_imp$weeks==6])
  
  x = cbind(T_pred = NAT_RES_TABLE$T,
            V_NDA_pred =  NAT_RES_TABLE$NDA,
            V_OTHER_pred =  NAT_RES_TABLE$OTHER,
            V_UPA_pred =  NAT_RES_TABLE$UPA )
  y = as.numeric(as.character(unlist(nat_res_obs)))[match(colnames(x),colnames(nat_res_obs))]
  x = as.numeric(as.character(unlist(x)))
  
  delta = 100*x[-1]-unique(HIST[which(HIST$Year==2014),c("NAT_Percentage.Votes.NDA","NAT_Percentage.Votes.UPA","NAT_Percentage.Votes.OTHER")])
  USwing_Preds = cbind(H_temp[,c("State","Zones","PC_name")],
                       foreach(i = 1:dim(H_temp)[1],.combine = 'rbind') %do% 
                         (H_temp[i,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] + delta))
  USwing_Winner = apply(H_temp[,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] ,1,function(x){c("NDA","UPA","OTHER")[which(x==max(x))]})
  USwing_Winner_list  = cbind(USwing_Winner_list ,USwing_Winner)

  # # # # Calculate new seat total simulations
  temp_xx = 
    c("NDA","UPA","OTHER")[apply(
      as.matrix(t(  
        sapply(1:dim(H_temp)[1],function(x){
          as.numeric(as.character(unlist(
            (H_temp[x,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")] + delta)
          )))
        })
      )),
      1,function(z){which(z==max(z))})
    ]
  
  
  
  print(xtable(  data.table(Alliance = c("NDA","OTHER","UPA"),
                            pct_Vote =   x [-1],
                            U.Swing.Pred = as.numeric(as.character(unlist(table(temp_xx)))),
                            No.Model.Error = c(table(temp_xx)["NDA"]-354,
                                               table(temp_xx)["OTHER"]-89,
                                               table(temp_xx)["UPA"]-99)
                            )), include.rownames=FALSE)
  

