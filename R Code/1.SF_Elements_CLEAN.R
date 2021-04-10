rm(list=ls())
options(scipen=999)
setwd(dir = "~/Desktop/India 2019/")
library(reshape2)
library(data.table)
library(foreign)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Part 1: Create Stratification Frame # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# a) IHDS - Large survey, many crosstabs, nationally representative but small (many cells 1 or 2 entries)

# Load relevant parts from Human Development Survey
load("SF Data/IHDS_Data.rda")
dt_1 = as.data.frame(da36151.0001)
rm(da36151.0001)
# Select variables we are interested in having target counts for (and a couple of side questions that could be useful for multiple imputation purposes)
vars = c(
  "STATEID", # state id
  "RO3", # sex
  "RO4", # relationship to head of household
  "RO5", # age
  "RO6", # marital status
  "URBAN2011", # urban/rural
  "ID11", # religion
  "ED2", # literacy
  "EDUC7", # educational attainement
  "INCEARN", # net income (rupees)
  "ID13" # caste category
)
# Edit some variables to match survey questions
# use data.table format as it's much faster 
dt_subset = data.table(dt_1[,vars])

# Income
dt_subset$INCEARN = cut(dt_subset$INCEARN,
                        c(min(dt_subset$INCEARN,na.rm=TRUE),59999,89999,199999,239999,599999,max(dt_subset$INCEARN,na.rm=TRUE)),
                        labels = c("(01) Less than ₹60,000 01","(02) ₹60,000 - ₹89,999 02","(03) ₹90,000 - ₹1,19,999 03","(04) ₹1,20,000 - ₹2,39,999 04","(05) ₹2,40,000 - ₹5,99,999 05","(06) ₹6,00,000 and over 06")
)
# Age 
dt_subset$RO5 = cut(dt_subset$RO5,c(-1,17,24,34,44,54,64,max(dt_subset$RO5,na.rm=TRUE)),
                    labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-100]")
)

# Create Zones Variable
dt_subset$ZONES = ifelse(dt_subset$STATEID=="(04) Chandigarh 04"|
                           dt_subset$STATEID=="(06) Haryana 06"|
                           dt_subset$STATEID=="(02) Himachal Pradesh 02"|
                           dt_subset$STATEID=="(01) Jammu & Kashmir 01"|
                           dt_subset$STATEID=="(03) Punjab 03"|
                           dt_subset$STATEID=="(08) Rajasthan 08","(01) North 01",
                         ifelse(dt_subset$STATEID=="(10) Bihar 10"|
                                  dt_subset$STATEID=="(07) Delhi 07"|
                                  dt_subset$STATEID== "(23) Madhya Pradesh 23"|
                                  dt_subset$STATEID=="(09) Uttar Pradesh 09"|
                                  dt_subset$STATEID=="(05) Uttarakhand 05","(02) North-Central 02",
                                ifelse(dt_subset$STATEID=="(18) Assam 18"|
                                         dt_subset$STATEID=="(12) Arunachal Pradesh 12"|
                                         dt_subset$STATEID=="(14) Manipur 14"|
                                         dt_subset$STATEID=="(17) Meghalaya 17"|
                                         dt_subset$STATEID=="(15) Mizoram 15"|
                                         dt_subset$STATEID=="(13) Nagaland 13"|
                                         dt_subset$STATEID=="(16) Tripura 16","(03) North-Eastern 03",
                                       ifelse(dt_subset$STATEID=="(35) Anadman/Nicobar 35"|
                                                dt_subset$STATEID=="(22) Chhattisgarh 22"|
                                                dt_subset$STATEID=="(20) Jharkhand 20"|
                                                dt_subset$STATEID=="(21) Orissa 21"|
                                                dt_subset$STATEID=="(11) Sikkim 11"|
                                                dt_subset$STATEID=="(19) West Bengal 19","(04) Eastern 04",
                                              ifelse(dt_subset$STATEID=="(26) Dadra+Nagar Haveli 26"|
                                                       dt_subset$STATEID=="(25) Daman & Diu 25"|
                                                       dt_subset$STATEID=="(30) Goa 30"|
                                                       dt_subset$STATEID=="(24) Gujarat 24"|
                                                       dt_subset$STATEID=="(27) Maharashtra 27","(05) Western 05",
                                                     ifelse(dt_subset$STATEID=="(28) Andhra Pradesh 28"|
                                                              dt_subset$STATEID=="(29) Karnataka 29"|
                                                              dt_subset$STATEID=="(32) Kerala 32"|
                                                              dt_subset$STATEID=="(31) Lakshadweep 31"|
                                                              dt_subset$STATEID=="(34) Pondicherry 34"|
                                                              dt_subset$STATEID=="(33) Tamil Nadu 33"|
                                                              dt_subset$STATEID=="(36) Telangana 36","(06) Southern 06",
                                                            dt_subset$STATEID))))))
dt_subset$ZONES = as.factor(dt_subset$ZONES)
# Education
dt_subset$EDUC7  = ifelse(dt_subset$EDUC7=="(00) none 0","(01) No Formal Edu 01",
                          ifelse(dt_subset$EDUC7=="(03) 1-4 3"|
                                   dt_subset$EDUC7=="(05) primary 5","(02) Primary or Lower 02",
                                 ifelse(dt_subset$EDUC7=="(08) 6-9 8"|
                                          dt_subset$EDUC7=="(10) Secondary(&11) 10","(03) Middle or Secondary 03",
                                        ifelse(dt_subset$EDUC7=="(12) Higher sec(&13,14) 12","(04) Higher Secondary 04",
                                               ifelse(dt_subset$EDUC7=="(15) graduate 15"|
                                                        dt_subset$EDUC7=="(16) some post-grad 16","(05) Some Graduate or Higher 05",NA
                                               )))))
dt_subset$EDUC7 = as.factor(dt_subset$EDUC7)
levels(as.factor(dt_subset$EDUC7))
# Religion
dt_subset$ID11 = as.character(unlist(dt_subset$ID11 ))
dt_subset$ID11 = 
  ifelse(dt_subset$ID11=="(7) Tribal 7"|
           dt_subset$ID11=="(8) Others 8"|
           dt_subset$ID11=="(9) None 9","(7) Other 7",dt_subset$ID11)
dt_subset$ID11 = as.factor(dt_subset$ID11)


# only keep attributes which are meant to be informative for voting population 
dt_subset = dt_subset[dt_subset$RO5!="[0-17]",]
names(dt_subset) = c("states","gender","family_role","age_cat","marital_status","rurality","religion","literacy","education_level","income_level","jati","zones")

fwrite(dt_subset,file = "Generated Quantities/IHDS_clean.csv",row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# b) CENSUS data, less crosstabs, more precise/big

# Load census data (EDU-AGE-SEX-STATE-RURAL)
# This is the most extensive census cross-tabs which we have census cross-tabs for.
# stacking up we obtain cross-tabs for states too. 
CENSUS = read.csv("SF Data/CENSUS_edu_age_sex_state_crosstabs.csv")

# clean census data to be in comparable format to IHDS
census_temp = sapply(1:dim(CENSUS)[2],function(x){paste(CENSUS[1:6,x],collapse = "_")})

CENSUS_temp = data.frame(State = CENSUS[,4],Rurality = CENSUS[,5],Age_Category = CENSUS[,6],
                         Total = CENSUS[,7],
                         Illiterate_Males = CENSUS[,11],Illiterate_Females = CENSUS[,12],
                         # Literate_Males = CENSUS[,14],Literate_Females = CENSUS[,15],
                         Literate_NoEdu_Males = CENSUS[,17],Literate_NoEdu_Females = CENSUS[,18],
                         Literate_BelowPrimary_Males = CENSUS[,20],Literate_BelowPrimary_Females = CENSUS[,21],
                         Literate_Primary_Males = CENSUS[,23],Literate_Primary_Females = CENSUS[,24],
                         Literate_Middle_Males = CENSUS[,26],Literate_Middle_Females = CENSUS[,27],
                         Literate_Secondary_Males = CENSUS[,29],Literate_Secondary_Females = CENSUS[,30],
                         Literate_PreUni_Males = CENSUS[,32],Literate_PreUni_Females = CENSUS[,33],
                         Literate_NonTechDiplomaNoDegree_Males = CENSUS[,35],Literate_NonTechDiplomaNoDegree_Females = CENSUS[,36],
                         Literate_TechDiplomaNoDegree_Males = CENSUS[,38],Literate_TechDiplomaNoDegree_Females = CENSUS[,39],
                         Literate_GradPlus_Males = CENSUS[,41],Literate_GradPlus_Females = CENSUS[,42],
                         Literate_Unclassified_Males = CENSUS[,44],Literate_Unclassified_Females = CENSUS[,45]
)[-c(1:6),]
CENSUS_temp =  data.frame(CENSUS_temp[,1:3],apply(CENSUS_temp[,-c(1:3)],2,function(x){as.numeric(as.character(unlist(x)))}))


# Sort out Age categories by state 
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$State=="INDIA"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="Age not stated"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="All ages"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="0-6"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="7"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="8"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="9"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="10"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="11"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="12"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="13"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="14"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="15"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="16"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Age_Category=="17"),]
CENSUS_temp = CENSUS_temp[-which(CENSUS_temp$Rurality=="Total"),]
CENSUS_temp$State = gsub("State - ","",CENSUS_temp$State)
CENSUS_temp = CENSUS_temp[,-which(names(CENSUS_temp)=="Total")]

# melt education categories so they are row-categories like rurality state age 
CENSUS_temp = melt(CENSUS_temp,id.vars = c("State","Rurality","Age_Category"))
# remove underscores in categroy names for education
CENSUS_temp$Gender = sub('.*_\\s*', '', CENSUS_temp$variable)
# clean categroey names - literacy
CENSUS_temp$Literacy =  ifelse(grepl("Illiterate",CENSUS_temp$variable),"Illiterate","Literate")
# derive category names - remove male/female
CENSUS_temp$variable =  gsub("Literate_","",gsub("Illiterate_","",gsub("_Female","",gsub("_Male","",CENSUS_temp$variable))))
names(CENSUS_temp)[which(names(CENSUS_temp)=="variable")]="Education"

# standardize names and categories to match the IHDS data
# first states: 
names(CENSUS_temp)[which(names(CENSUS_temp)=="State")] = "states"
CENSUS_temp$states = tolower(CENSUS_temp$states)
CENSUS_temp$states =  
  ifelse(CENSUS_temp$states=="andaman & nicobar islands","(35) Anadman/Nicobar 35",
         ifelse(CENSUS_temp$states=="lakshadweep","(31) Lakshadweep 31",
                ifelse(CENSUS_temp$states=="madhya pradesh","(23) Madhya Pradesh 23",
                       ifelse(CENSUS_temp$states=="mizoram","(15) Mizoram 15",
                              ifelse(CENSUS_temp$states=="nct of delhi","(07) Delhi 07",
                                     ifelse(CENSUS_temp$states=="puducherry","(34) Pondicherry 34",
                                            ifelse(CENSUS_temp$states=="dadra & nagar haveli","(26) Dadra+Nagar Haveli 26",
                                                   ifelse(CENSUS_temp$states=="daman & diu","(25) Daman & Diu 25",
                                                          ifelse(CENSUS_temp$states=="odisha","(21) Orissa 21",
                                                                 ifelse(CENSUS_temp$states=="andhra pradesh","(28) Andhra Pradesh 28",
                                                                        ifelse(CENSUS_temp$states=="arunachal pradesh","(12) Arunachal Pradesh 12",
                                                                               ifelse(CENSUS_temp$states=="assam","(18) Assam 18",
                                                                                      ifelse(CENSUS_temp$states=="bihar","(10) Bihar 10",
                                                                                             ifelse(CENSUS_temp$states=="chandigarh","(04) Chandigarh 04",
                                                                                                    ifelse(CENSUS_temp$states=="chhattisgarh","(22) Chhattisgarh 22",
                                                                                                           ifelse(CENSUS_temp$states=="delhi","(07) Delhi 07",
                                                                                                                  ifelse(CENSUS_temp$states=="goa","(30) Goa 30",
                                                                                                                         ifelse(CENSUS_temp$states=="gujarat","(24) Gujarat 24",
                                                                                                                                ifelse(CENSUS_temp$states=="haryana","(06) Haryana 06",
                                                                                                                                       ifelse(CENSUS_temp$states=="himachal pradesh","(02) Himachal Pradesh 02",
                                                                                                                                              ifelse(CENSUS_temp$states=="jammu & kashmir","(01) Jammu & Kashmir 01",
                                                                                                                                                     ifelse(CENSUS_temp$states=="jharkhand","(20) Jharkhand 20",
                                                                                                                                                            ifelse(CENSUS_temp$states=="karnataka","(29) Karnataka 29",
                                                                                                                                                                   ifelse(CENSUS_temp$states=="kerala","(32) Kerala 32",
                                                                                                                                                                          ifelse(CENSUS_temp$states=="madya pradesh","(23) Madhya Pradesh 23",
                                                                                                                                                                                 ifelse(CENSUS_temp$states=="maharashtra","(27) Maharashtra 27",
                                                                                                                                                                                        ifelse(CENSUS_temp$states=="manipur","(14) Manipur 14",
                                                                                                                                                                                               ifelse(CENSUS_temp$states=="meghalaya","(17) Meghalaya 17",
                                                                                                                                                                                                      ifelse(CENSUS_temp$states=="nagaland","(13) Nagaland 13",
                                                                                                                                                                                                             ifelse(CENSUS_temp$states=="orissa","(21) Orissa 21",
                                                                                                                                                                                                                    ifelse(CENSUS_temp$states=="pondicherry","(34) Pondicherry 34",
                                                                                                                                                                                                                           ifelse(CENSUS_temp$states=="punjab","(03) Punjab 03",
                                                                                                                                                                                                                                  ifelse(CENSUS_temp$states=="rajasthan","(08) Rajasthan 08",
                                                                                                                                                                                                                                         ifelse(CENSUS_temp$states=="sikkim","(11) Sikkim 11",
                                                                                                                                                                                                                                                ifelse(CENSUS_temp$states=="tamil nadu","(33) Tamil Nadu 33",
                                                                                                                                                                                                                                                       ifelse(CENSUS_temp$states=="tripura","(16) Tripura 16",
                                                                                                                                                                                                                                                              ifelse(CENSUS_temp$states=="uttar pradesh","(09) Uttar Pradesh 09",
                                                                                                                                                                                                                                                                     ifelse(CENSUS_temp$states=="west bengal","(19) West Bengal 19",
                                                                                                                                                                                                                                                                            ifelse(CENSUS_temp$states=="uttarakhand","(05) Uttarakhand 05",
                                                                                                                                                                                                                                                                                   CENSUS_temp$states
                                                                                                                                                                                                                                                                            )))))))))))))))))))))))))))))))))))))))
# gender: 
names(CENSUS_temp)[which(names(CENSUS_temp)=="Gender")] = "gender"
CENSUS_temp$gender = ifelse(CENSUS_temp$gender=="Males","(1) Male 1","(2) Female 2")
# age - need to then aggregate over these new broader cells
CENSUS_temp$Age_Category = ifelse(CENSUS_temp$Age_Category=="18"|
                                    CENSUS_temp$Age_Category=="19"|
                                    CENSUS_temp$Age_Category=="20-24","(17-24]",
                                  ifelse(CENSUS_temp$Age_Category=="25-29"|
                                           CENSUS_temp$Age_Category=="30-34","(24-34]",
                                         ifelse(CENSUS_temp$Age_Category=="35-39"|
                                                  CENSUS_temp$Age_Category=="40-44","(34-44]",
                                                ifelse(CENSUS_temp$Age_Category=="45-49"|
                                                         CENSUS_temp$Age_Category=="50-54","(44-54]",
                                                       ifelse(CENSUS_temp$Age_Category=="55-59"|
                                                                CENSUS_temp$Age_Category=="60-64","(54-64]",
                                                              ifelse(CENSUS_temp$Age_Category=="65-69"|
                                                                       CENSUS_temp$Age_Category=="70-74"|
                                                                       CENSUS_temp$Age_Category=="75-79"|
                                                                       CENSUS_temp$Age_Category=="80+","(64-100]",NA)
                                                       )))))
names(CENSUS_temp)[which(names(CENSUS_temp)=="Age_Category")] = "age_cat"

# education level - recategroize to match survey/IHDS
names(CENSUS_temp)[which(names(CENSUS_temp)=="Education")] = "education_level"
CENSUS_temp$education_level = ifelse(CENSUS_temp$education_level=="Illiterates","(00) none 0",
                                     ifelse(CENSUS_temp$education_level=="NoEdus","(00) none 0",
                                            ifelse(CENSUS_temp$education_level=="BelowPrimarys","(03) 1-4 3",
                                                   ifelse(CENSUS_temp$education_level=="Primarys","(05) primary 5",
                                                          ifelse(CENSUS_temp$education_level=="Middles","(08) 6-9 8",
                                                                 ifelse(CENSUS_temp$education_level=="Secondarys","(10) Secondary(&11) 10",
                                                                        ifelse(CENSUS_temp$education_level=="NonTechDiplomaNoDegrees","(12) Higher sec(&13,14) 12",
                                                                               ifelse(CENSUS_temp$education_level=="PreUnis","(12) Higher sec(&13,14) 12",
                                                                                      ifelse(CENSUS_temp$education_level=="TechDiplomaNoDegrees","(12) Higher sec(&13,14) 12",
                                                                                             ifelse(CENSUS_temp$education_level=="GradPluss","(15plus) graduate plus 15",
                                                                                                    ifelse(CENSUS_temp$education_level=="Unclassifieds",NA,CENSUS_temp$education_level
                                                                                                    )))))))))))


CENSUS_temp$education_level  = ifelse(CENSUS_temp$education_level=="(00) none 0","(01) No Formal Edu 01",
                                      ifelse(CENSUS_temp$education_level=="(03) 1-4 3"|
                                               CENSUS_temp$education_level=="(05) primary 5","(02) Primary or Lower 02",
                                             ifelse(CENSUS_temp$education_level=="(08) 6-9 8"|
                                                      CENSUS_temp$education_level=="(10) Secondary(&11) 10","(03) Middle or Secondary 03",
                                                    ifelse(CENSUS_temp$education_level=="(12) Higher sec(&13,14) 12","(04) Higher Secondary 04",
                                                           ifelse(CENSUS_temp$education_level=="(15plus) graduate plus 15","(05) Some Graduate or Higher 05",NA
                                                           )))))

# now rurality 
names(CENSUS_temp)[which(names(CENSUS_temp)=="Rurality")]="rurality"
CENSUS_temp$rurality = ifelse(CENSUS_temp$rurality=="Rural","(0) rural 0 ","(1) urban 1")

# remove unclassified edu people
CENSUS_temp = CENSUS_temp[-which(is.na(CENSUS_temp$education_level)),]
# ignore literacy (highly correlated with noedu)
CENSUS_temp = CENSUS_temp [,-which(names(CENSUS_temp)=="Literacy")]
# now aggregate over new cats 
CENSUS_temp = data.table(CENSUS_temp)
CENSUS_temp = CENSUS_temp[,totcount := sum(value),by = c("states","rurality","age_cat","education_level","gender")]
CENSUS_temp = CENSUS_temp[-which(duplicated(paste(states,rurality,age_cat,education_level,gender))),]
CENSUS_temp = CENSUS_temp[,value:=NULL]


# Create Zones Variable
CENSUS_temp$zones = ifelse(CENSUS_temp$states=="(04) Chandigarh 04"|
                           CENSUS_temp$states=="(06) Haryana 06"|
                           CENSUS_temp$states=="(02) Himachal Pradesh 02"|
                           CENSUS_temp$states=="(01) Jammu & Kashmir 01"|
                           CENSUS_temp$states=="(03) Punjab 03"|
                           CENSUS_temp$states=="(08) Rajasthan 08","(01) North 01",
                         ifelse(CENSUS_temp$states=="(10) Bihar 10"|
                                  CENSUS_temp$states=="(07) Delhi 07"|
                                  CENSUS_temp$states== "(23) Madhya Pradesh 23"|
                                  CENSUS_temp$states=="(09) Uttar Pradesh 09"|
                                  CENSUS_temp$states=="(05) Uttarakhand 05","(02) North-Central 02",
                                ifelse(CENSUS_temp$states=="(18) Assam 18"|
                                         CENSUS_temp$states=="(12) Arunachal Pradesh 12"|
                                         CENSUS_temp$states=="(14) Manipur 14"|
                                         CENSUS_temp$states=="(17) Meghalaya 17"|
                                         CENSUS_temp$states=="(15) Mizoram 15"|
                                         CENSUS_temp$states=="(13) Nagaland 13"|
                                         CENSUS_temp$states=="(16) Tripura 16","(03) North-Eastern 03",
                                       ifelse(CENSUS_temp$states=="(35) Anadman/Nicobar 35"|
                                                CENSUS_temp$states=="(22) Chhattisgarh 22"|
                                                CENSUS_temp$states=="(20) Jharkhand 20"|
                                                CENSUS_temp$states=="(21) Orissa 21"|
                                                CENSUS_temp$states=="(11) Sikkim 11"|
                                                CENSUS_temp$states=="(19) West Bengal 19","(04) Eastern 04",
                                              ifelse(CENSUS_temp$states=="(26) Dadra+Nagar Haveli 26"|
                                                       CENSUS_temp$states=="(25) Daman & Diu 25"|
                                                       CENSUS_temp$states=="(30) Goa 30"|
                                                       CENSUS_temp$states=="(24) Gujarat 24"|
                                                       CENSUS_temp$states=="(27) Maharashtra 27","(05) Western 05",
                                                     ifelse(CENSUS_temp$states=="(28) Andhra Pradesh 28"|
                                                              CENSUS_temp$states=="(29) Karnataka 29"|
                                                              CENSUS_temp$states=="(32) Kerala 32"|
                                                              CENSUS_temp$states=="(31) Lakshadweep 31"|
                                                              CENSUS_temp$states=="(34) Pondicherry 34"|
                                                              CENSUS_temp$states=="(33) Tamil Nadu 33"|
                                                              CENSUS_temp$states=="(36) Telangana 36","(06) Southern 06",
                                                            CENSUS_temp$states))))))
CENSUS_temp$zones = as.factor(CENSUS_temp$zones)

# tweak factors in rurarilty to match bettwe
CENSUS_temp = as.data.frame(apply(CENSUS_temp,2,function(x){as.character(unlist(x))}))
CENSUS_temp$rurality = gsub("0 ","0",CENSUS_temp$rurality)

fwrite(CENSUS_temp,file = "Generated Quantities/CENSUS_counts_clean.csv",row.names = FALSE)


# now get 2014 Indian election study and prepare for analysis
NES14 = read.spss(file = "SF Data/All India NES 2014 Postpoll data file.sav",to.data.frame = TRUE)
# state
names(NES14)[which(names(NES14)=="state_id")] = "states"
levels(NES14$states)[which(levels(NES14$states)=="01: Seemandhra")] = "(28) Andhra Pradesh 28"
levels(NES14$states)[which(levels(NES14$states)=="02: Arunachal Pradesh")] = "(12) Arunachal Pradesh 12"
levels(NES14$states)[which(levels(NES14$states)=="03: Assam")] = "(18) Assam 18"
levels(NES14$states)[which(levels(NES14$states)=="04: Bihar")] = "(10) Bihar 10"
levels(NES14$states)[which(levels(NES14$states)=="05: Goa")] = "(30) Goa 30"
levels(NES14$states)[which(levels(NES14$states)=="06: Gujarat")] = "(24) Gujarat 24"
levels(NES14$states)[which(levels(NES14$states)=="07: Haryana")] = "(06) Haryana 06"
levels(NES14$states)[which(levels(NES14$states)=="08: Himachal Pradesh")] = "(02) Himachal Pradesh 02"
levels(NES14$states)[which(levels(NES14$states)=="09: Jammu & Kashmir")] = "(01) Jammu & Kashmir 01"
levels(NES14$states)[which(levels(NES14$states)=="10: Karnataka")] ="(29) Karnataka 29"
levels(NES14$states)[which(levels(NES14$states)=="11: Kerala")] = "(29) Karnataka 29"
levels(NES14$states)[which(levels(NES14$states)=="12: Madhya Pradesh")] = "(23) Madhya Pradesh 23"
levels(NES14$states)[which(levels(NES14$states)=="13: Maharashtra")] = "(27) Maharashtra 27"
levels(NES14$states)[which(levels(NES14$states)=="14: Manipur")] = "(14) Manipur 14"
levels(NES14$states)[which(levels(NES14$states)=="15: Meghalaya")] = "(17) Meghalaya 17"
levels(NES14$states)[which(levels(NES14$states)=="16: Mizoram")] = "(15) Mizoram 15"
levels(NES14$states)[which(levels(NES14$states)=="17: Nagaland")] = "(13) Nagaland 13"
levels(NES14$states)[which(levels(NES14$states)=="18: Odisha")] = "(21) Orissa 21"
levels(NES14$states)[which(levels(NES14$states)=="19: Punjab")] = "(03) Punjab 03"
levels(NES14$states)[which(levels(NES14$states)=="20: Rajasthan")] = "(08) Rajasthan 08"
levels(NES14$states)[which(levels(NES14$states)=="21: Sikkim")] = "(11) Sikkim 11"
levels(NES14$states)[which(levels(NES14$states)=="22: Tamil Nadu")] = "(33) Tamil Nadu 33"
levels(NES14$states)[which(levels(NES14$states)=="23: Tripura")] = "(16) Tripura 16"
levels(NES14$states)[which(levels(NES14$states)=="24: Uttar Pradesh")] = "(09) Uttar Pradesh 09"
levels(NES14$states)[which(levels(NES14$states)=="25: West Bengal")] = "(19) West Bengal 19"
levels(NES14$states)[which(levels(NES14$states)=="27: Chandigarh")] = "(04) Chandigarh 04"
levels(NES14$states)[which(levels(NES14$states)=="30: Delhi")] = "(07) Delhi 07"
levels(NES14$states)[which(levels(NES14$states)=="32: Pondicherry")] = "(34) Pondicherry 34"
levels(NES14$states)[which(levels(NES14$states)=="33: Jharkhand")] = "(20) Jharkhand 20"
levels(NES14$states)[which(levels(NES14$states)=="34: Chhattisgarh")] = "(22) Chhattisgarh 22"
levels(NES14$states)[which(levels(NES14$states)=="35: UttaraKhand")] = "(05) Uttarakhand 05"
levels(NES14$states)[which(levels(NES14$states)=="36: Telangana")] = "(36) Telangana 36"
# zones
NES14$zones = ifelse(NES14$states=="(04) Chandigarh 04"|
                       NES14$states=="(06) Haryana 06"|
                       NES14$states=="(02) Himachal Pradesh 02"|
                       NES14$states=="(01) Jammu & Kashmir 01"|
                       NES14$states=="(03) Punjab 03"|
                       NES14$states=="(08) Rajasthan 08","(01) North 01",
                     ifelse(NES14$states=="(10) Bihar 10"|
                              NES14$states=="(07) Delhi 07"|
                              NES14$states== "(23) Madhya Pradesh 23"|
                              NES14$states=="(09) Uttar Pradesh 09"|
                              NES14$states=="(05) Uttarakhand 05","(02) North-Central 02",
                            ifelse(NES14$states=="(18) Assam 18"|
                                     NES14$states=="(12) Arunachal Pradesh 12"|
                                     NES14$states=="(14) Manipur 14"|
                                     NES14$states=="(17) Meghalaya 17"|
                                     NES14$states=="(15) Mizoram 15"|
                                     NES14$states=="(13) Nagaland 13"|
                                     NES14$states=="(16) Tripura 16","(03) North-Eastern 03",
                                   ifelse(NES14$states=="(35) Anadman/Nicobar 35"|
                                            NES14$states=="(22) Chhattisgarh 22"|
                                            NES14$states=="(20) Jharkhand 20"|
                                            NES14$states=="(21) Orissa 21"|
                                            NES14$states=="(11) Sikkim 11"|
                                            NES14$states=="(19) West Bengal 19","(04) Eastern 04",
                                          ifelse(NES14$states=="(26) Dadra+Nagar Haveli 26"|
                                                   NES14$states=="(25) Daman & Diu 25"|
                                                   NES14$states=="(30) Goa 30"|
                                                   NES14$states=="(24) Gujarat 24"|
                                                   NES14$states=="(27) Maharashtra 27","(05) Western 05",
                                                 ifelse(NES14$states=="(28) Andhra Pradesh 28"|
                                                          NES14$states=="(29) Karnataka 29"|
                                                          NES14$states=="(32) Kerala 32"|
                                                          NES14$states=="(31) Lakshadweep 31"|
                                                          NES14$states=="(34) Pondicherry 34"|
                                                          NES14$states=="(33) Tamil Nadu 33"|
                                                          NES14$states=="(36) Telangana 36","(06) Southern 06",
                                                        NES14$states))))))
NES14$zones = as.factor(NES14$zones)
# gender
names(NES14)[which(names(NES14)=="z2")] ="gender"
levels(NES14$gender)[which(levels(NES14$gender)=="1: Male")] = "(1) Male 1"
levels(NES14$gender)[which(levels(NES14$gender)=="2: Female")] = "(2) Female 2"
# age_cat
names(NES14)[which(names(NES14)=="z1")] ="age_cat"
NES14$age_cat = as.numeric(as.character(unlist(NES14$age_cat)))
NES14$age_cat = 
  cut(x = NES14$age_cat,
      breaks = c(0,17,24,34,44,54,64,max(NES14$age_cat,na.rm=TRUE)),
      labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-100]"))
NES14$age_cat = as.factor(as.character(unlist(NES14$age_cat)))

# religion
names(NES14)[which(names(NES14)=="z6")] ="religion"
levels(NES14$religion)[which(levels(NES14$religion)=="1: Hindu")] = "(1) Hindu 1"
levels(NES14$religion)[which(levels(NES14$religion)=="2: Muslim")] = "(2) Muslim 2"
levels(NES14$religion)[which(levels(NES14$religion)=="3: Christian")] = "(3) Christian 3"
levels(NES14$religion)[which(levels(NES14$religion)=="4: Sikh")] = "(4) Sikh 4"
levels(NES14$religion)[which(levels(NES14$religion)=="5: Buddhist/Neo Buddhist")] = "(5) Buddhist 5"
levels(NES14$religion)[which(levels(NES14$religion)=="6: Jain")] = "(6) Jain 6"
levels(NES14$religion)[which(levels(NES14$religion)=="7: Animism")] = "(7) Other 7"
levels(NES14$religion)[which(levels(NES14$religion)=="8: No religion")] = "(7) Other 7"
levels(NES14$religion)[which(levels(NES14$religion)=="9: Others")] = "(7) Other 7"
# literacy
NES14$literacy = NES14$z3a_f
levels(NES14$literacy) = ifelse(levels(NES14$z3a_f)=="0: Non Literate","(0) No 0","(1) Yes 1")
# education level
names(NES14)[which(names(NES14)=="z3a_f")] ="education_level"
levels(NES14$education_level)[which(levels(NES14$education_level)=="0: Non Literate")] = "(01) No Formal Edu 01"
levels(NES14$education_level)[which(levels(NES14$education_level)=="1: Below Primary")] = "(02) Primary or Lower 02"
levels(NES14$education_level)[which(levels(NES14$education_level)=="2: Primary pass/ Middle fail")] = "(02) Primary or Lower 02"
levels(NES14$education_level)[which(levels(NES14$education_level)=="3: Middle pass/Matric Fail")] = "(03) Middle or Secondary 03"
levels(NES14$education_level)[which(levels(NES14$education_level)=="4: Matric")] = "(04) Higher Secondary 04"
levels(NES14$education_level)[which(levels(NES14$education_level)=="5: Intermediate/ College no degree")] = "(04) Higher Secondary 04"
levels(NES14$education_level)[which(levels(NES14$education_level)=="6: Graduate or equivalent")] = "(05) Some Graduate or Higher 05"
levels(NES14$education_level)[which(levels(NES14$education_level)=="7: Post Graduate")] = "(05) Some Graduate or Higher 05"
levels(NES14$education_level)[which(levels(NES14$education_level)=="8: Professional Degrees and Higher Research")] = "(05) Some Graduate or Higher 05"
# income_level
# this is monthly income - need to set to yearly income 
names(NES14)[which(names(NES14)=="z13")] ="income_level"
NES14$income_level = NES14$income_level*12
NES14$income_level = cut(NES14$income_level,
                         c(min(NES14$income_level,na.rm=TRUE)-1,59999,89999,199999,239999,599999,max(NES14$income_level,na.rm=TRUE)),
                         labels = c("(01) Less than ₹60,000 01","(02) ₹60,000 - ₹89,999 02","(03) ₹90,000 - ₹1,19,999 03","(04) ₹1,20,000 - ₹2,39,999 04","(05) ₹2,40,000 - ₹5,99,999 05","(06) ₹6,00,000 and over 06")
)
# jati
NES14$jati = sub(".*? ", "",NES14$z5)
NES14$jati = trimws(tolower(NES14$jati),which = 'both')
# Clean Jati and assign broader Caste category
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both") =="pubjabi","Punjabi",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="shatriya","Kshatriya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vanniya kula kshatriyar","Kshatriya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="agamudiyar","Agamudiyar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="andi pandaram","Andipandaram",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="bc","OBC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="badaga","Badaga",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="baishya","Baishya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="banya","Banya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="baniya","Banya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="bc-b","OBC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="bhandari","Bhandari",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="chettiar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="chettiyar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="chettiyear","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="chowdhary(kamma)","Kamma",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="csi","Dalit",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="dalith","Dalit",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="devangar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="devar","Dewar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="fishermen","Dewar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="ezhava","Ezhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="gavara","Gavara",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="goundar","Goundar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="gounder","Goundar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="gowda","Gowda",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="gowdas","Gowda",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="humanity","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="hindhu","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="hindu","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="hindu vaniya settiyar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="india","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="jain","Jain",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kallar","Kallar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kamma","Kamma",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kammalar","Kammalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kammavar","Kamma",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kapu","Kapu",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kasar","Kasar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kongu vellala gounder","Goundar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kongu vellalar gounder","Goundar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kongu veller","Goundar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kshatriya","Kshatriya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="labbai","Labbai",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="latin catholic","Upper Caste Christians",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mala arayan","Dheevara",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mannar","Other",NES14$jati) # I think this is a location..
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mappila","Mappila",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="maravar","Maravar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mbc","OBC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="most backward","OBC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="menon","Nair",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mpc","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mudaliyar","Mudaliar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="muslim","Muslim",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="nadar","Nadar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="naadar","Nadar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="naidu","Kamma",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="obc","OBC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="nair","Nair",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="padmanayaka velama","Velama",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="patel","Patel",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="pattunulkarar","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="pillai","Pillai",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="pillamar","Vellalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="saiva vellalar","Vellalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="saliar","Saliya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="saliya","Saliya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="saliyar","Saliya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sc","SC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="schedule caste","SC",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="scheduled tribe","ST",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="shahib",NA,NES14$jati) #couldn't find correspondent 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="shetty","Bunt",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sourashtra","Sourashtra",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sourastra","Sourashtra",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sozhila vellalar","Vellalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sozhlia vellaler","Vellalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sri karuneegar","Karuneegar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sunni","Muslim",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="tamil","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="tamilan","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="telugu chettiar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="thachan","Thachan",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="thiya","Ezhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="thiyya","Ezhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="thuluva vellala","Thuluva Vellala",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="valaiyar","Valaiyar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vaniga chettiyar","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vaniya","Banya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vaniyan","Banya",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vaniyar","Vanniyar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vannier kula satisreyar","Other",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vanniyar","Vanniyar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="veera shaiva","Veerasaiva",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vellalar","Vellalar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="viswakarma","Viswakarma",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vysya","Komati",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="yadav","Yadhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="yadava","Yadhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="yadhav","Yadhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="lohar(obc)","Lohar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="",NA,NES14$jati)
# second wave of "other" checks
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="adi dravidar","Dalit",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="agamudaiyar","Kallar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="ambalakarar","Ambalakarar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="ampalathar","Ampalathar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="arora","Arora",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="backwardclass","OBC",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="ballija","Kapu",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="chamar","Chamar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="dakkani","Dakkhani",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="devang","Chettiar",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="eezhava","Ezhava",NES14$jati)
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="gounders","Goundar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kalinga","Kalinga",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="khatri","Arora",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="khukrain","Arora",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kongu gounder","Goundar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="kongu vellalar","Vellalar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mahar","Dalit",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mapla","Mappila",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="maratha","Maratha",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="maravr","Maravar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="maruthuvar","Maruthuvar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="marwadi",NA,NES14$jati) # marwari is a regional belonging not cast system
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="muslim mappila","Mappila",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="mutharayar","Mutharayar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="muthuraja","Mutharayar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="obc","OBC",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="padmanayaka velams","Kapu",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="padmasali","Chettiar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="parkavakulam","Parkavakulam",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="pbc","OBC",NES14$jati) # pbc means poor backeards caste
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="pillaymar","Vellalar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="reddy","Reddy",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="rowther","Rowther",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sc (adi dravida)","Dalit",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="shree karuneegar","Karuneegar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="siva pillai","Vellalar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sozhilavellalor","Vellalar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sozhia vellalar","Vellalar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="sonar","Sunar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="upper caste","OFC",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="upper caste christians","OFC",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vadugar","Vadugar",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="varier","Nair",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vishwabrahmin","Brahmin",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vishwa brahmin","Brahmin",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vishwabramin","Brahmin",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vishwakarma","Viswakarma",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="vishwakarma","Viswakarma",NES14$jati) 
NES14$jati = ifelse(trimws(tolower(NES14$jati),which = "both")  =="yadavs","Yadhava",NES14$jati) 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="agamudiyar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="andipandaram")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="badaga")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="baishya")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="banya")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhandari")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bunt")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chettiar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dalit")]="(4) Scheduled Castes (SC) 4"#"Dalit"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dewar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dheevara")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ezhava")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gavara")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="goundar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gowda")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jain")]=NA#"Jain" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kallar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kamma")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kammalar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kapu")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karuneegar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kasar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="komati")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kshatriya")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="labbai")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mappila")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maravar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mudaliar")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="muslim")]=NA#"Muslim"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nadar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nair")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other")]="(6) Others 6"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="patel")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pillai")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="saliya")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sc")]="(4) Scheduled Castes (SC) 4"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sourashtra")]="(5) Scheduled Tribes (ST) 5"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thachan")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thuluva vellala")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="upper caste christians")]="Upper Caste Christians"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="valaiyar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vanniyar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="veerasaiva")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="velama")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vellalar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="viswakarma")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="yadhava")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lohar")]="(3) Other Backward Castes (OBC) 3"
# second wave of checks 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="obc")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ofc")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="st")]="(5) Scheduled Tribes (ST) 5"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="reddy")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="arora")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="brahmin")]="(1) Brahmin 1"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maratha")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="parkavakulam")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kalinga")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vadugar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="arunthathiyar")]="(4) Scheduled Castes (SC) 4"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rowther")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mutharayar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ambalakarar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dakkhani")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ampalathar")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maruthuvar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chamar")]="(4) Scheduled Castes (SC) 4"
# wave three of edits
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="agarwal")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Does-the-Agarwal-community-come-under-the-OBC-category
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ahir")]="(3) Other Backward Castes (OBC) 3" # https://timesofindia.indiatimes.com/city/nagpur/More-castes-included-in-backward-list/articleshow/29787773.cms
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ahl-e-quraish")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="assamese")]=NA # region not caste 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="baishnab")]="(3) Other Backward Castes (OBC) 3" #https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="balija")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bania")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="besthar")]="(5) Scheduled Tribes (ST) 5" # have asked to be part of ST https://en.wikipedia.org/wiki/Bestha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhat")]="(3) Other Backward Castes (OBC) 3" #http://www.rtifoundationofindia.com/appellant-why-caste-%E2%80%9Cbhat%E2%80%9D-was-included-obcs-while#.XIvhTx_njRY
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhumihar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="businessmen teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chatriya")]="(3) Other Backward Castes (OBC) 3" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dawoodi bhora")]="(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ghanchi")]="(3) Other Backward Castes (OBC) 3" #https://en.wikipedia.org/wiki/Ghanchi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gouda")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gujjar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Gurjar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gupta")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="harijans (merchants/traders of frowned products like alcohol)")]="(4) Scheduled Castes (SC) 4" # https://www.britannica.com/topic/untouchable
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hindu mangela")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="islam")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jatav")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Jatav
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kahar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kahar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kaisth")]="(2) Forward/General (except Brahmin) 2" #https://en.wikipedia.org/wiki/Kayastha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kalita")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kalita_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karmas")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kashmiri pandit")]="(6) Others 6"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kayasth")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kayastha")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khan")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khandayat")]="(3) Other Backward Castes (OBC) 3" # https://www.telegraphindia.com/states/jharkhand/demand-to-be-on-obc-list/cid/717275
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="koeri")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/What-is-the-difference-between-Kurmi-Koeri-and-Kushwaha-castes-of-Bihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kutchi visa oswal")]="(6) Others 6" # these are jain 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lingayath")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="malwadi")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="marwari")]= NA # this is a region https://www.quora.com/Are-marwaris-OBC
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maurya")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Which-caste-does-the-surname-Maurya-belong-to
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mudhaliyar")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="naga")]="(5) Scheduled Tribes (ST) 5" # https://www.quora.com/What-is-the-exact-difference-between-various-reserved-categories-like-scheduled-caste-schedule-tribe-and-OBC
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="none")]="(6) Others 6"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="not known")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="not sure")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="others")]="(6) Others 6"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="paswan")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Paswan
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="phatan")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-all-Muslims-in-India-fall-under-OBC-category
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="patidar")]="(4) Scheduled Castes (SC) 4"  # https://en.wikipedia.org/wiki/Patidar_reservation_agitation
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="phatans")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pathans")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="punjabi")]="(6) Others 6"  # regional denomination
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="punjabi khatri")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sahu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sahu
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sanamahi")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-manipur/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sharma")]="(1) Brahmin 1" # https://www.quora.com/What-category-does-Anurag-Sharma-belong-to-general-or-OBC
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sud")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunri")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sundhi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="syed shia")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Syed-Muslims-in-India-belong-in-the-general-category
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="unknown")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vatishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vokkaliga")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-the-3A-Vokkaliga-community-of-Karnataka-come-under-OBC-for-central-government-reservations
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vyshyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vysyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="yadavas")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kshyatriya")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vannya")]="(3) Other Backward Castes (OBC) 3" #
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ramgarhia")]="(3) Other Backward Castes (OBC) 3" #

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="atheist from brahmin family")]="(1) Brahmin 1" #
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="don't know")]=NA #
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kalinga vysya")]="(3) Other Backward Castes (OBC) 3" # https://www.thehindu.com/news/national/andhra-pradesh/obc-status-for-kalinga-vysyas-sistakaranams/article7537421.ece
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lingayat")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lohana")]="(3) Other Backward Castes (OBC) 3" # http://entrance-exam.net/forum/general-discussion/does-lohana-caste-gujarati-come-under-obc-category-state-maharashtra-257223.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mistri")]= "(3) Other Backward Castes (OBC) 3"# https://en.wikipedia.org/wiki/Mistri_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="momin ansar")]= "(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="shia's")]=NA

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhumihaar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="forward / general")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hnidu")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kshatriyas")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="naicker")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nayakar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="parayan")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Does-the-parayan-caste-come-under-SC
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rawuthar")]="(3) Other Backward Castes (OBC) 3" # http://www.ncbc.nic.in/Writereaddata/1123.PDF
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sozhiya vellalar")]= "(3) Other Backward Castes (OBC) 3"# http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="upper caste hindu")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishnava")]="(2) Forward/General (except Brahmin) 2"

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="arya vyshya")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Under-which-category-does-Arya-Vysya-come
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bori")]="(3) Other Backward Castes (OBC) 3" # http://www.firstfoundation.in/Soc/List-OBC-Mah.htm
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="harijans (businessman/trader of immoral goods e.g. alcohol)")]="(4) Scheduled Castes (SC) 4"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishya")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="buddihist")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bunts")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/How-do-Bunts-belonged-to-OBC-or-general
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chakkiliar")]="(4) Scheduled Castes (SC) 4" #  https://en.wikipedia.org/wiki/Arunthathiyar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chittiyar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="devanga")]="(3) Other Backward Castes (OBC) 3" # these are chettiar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hindu-nadar")]="(3) Other Backward Castes (OBC) 3" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karnam")]="(2) Forward/General (except Brahmin) 2" # http://pkpi4u.blogspot.com/2014/07/karanam-or-karana-is-caste-mostly.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kodava")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kodava_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="manipuri")]=NA # this is a region
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="muthaliyar")]="(2) Forward/General (except Brahmin) 2" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nair (upper caste)")]="(2) Forward/General (except Brahmin) 2" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pattani")]="(3) Other Backward Castes (OBC) 3" # https://wikivisually.com/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="saini")]="(3) Other Backward Castes (OBC) 3" # https://punjabxp.com/list-backward-other-classes/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishyas")]="(2) Forward/General (except Brahmin) 2" 

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="backward class")]="(3) Other Backward Castes (OBC) 3" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hindu - nair")]="(2) Forward/General (except Brahmin) 2" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kaikolar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="padmanayaka velamas")]="(2) Forward/General (except Brahmin) 2"  # https://en.wikipedia.org/wiki/List_of_Telugu_castes
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thiyya(obc)")]="(3) Other Backward Castes (OBC) 3" 


NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bouddh")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ganaka")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-kerala/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hindu salian")]="(3) Other Backward Castes (OBC) 3"  # https://sanjaykumarnishad.wordpress.com/category/obc/central-list-of-other-backward-castes-obcs/page/2/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maheshwari")]= "(2) Forward/General (except Brahmin) 2" #https://www.jeevansathi.com/rajasthani-maheshwari-matrimony-matrimonials
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pramalai kallar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="velar")]="(3) Other Backward Castes (OBC) 3" # https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes


NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chattiyar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chettyiar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nagker")]=NA#nagar? 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nayar")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Why-is-Nair-not-in-OBC
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thevar")]="(3) Other Backward Castes (OBC) 3" # https://www.thenewsminute.com/article/thevar-factor-who-real-aiadmk-dominant-obc-community-70804

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishya (agrawal)")]="(2) Forward/General (except Brahmin) 2" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="aggarwal")]="(2) Forward/General (except Brahmin) 2"

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ambastha")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Ambashtha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="badhai")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="baidya")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bengali zamindari kayastha")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhatia")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Bhatia_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="brahmin by birth but married to  non brahmin")]="(1) Brahmin 1"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chambhar")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chamar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chowdaries")]="(2) Forward/General (except Brahmin) 2" # kamma https://en.wikipedia.org/wiki/Kamma_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dhakad")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/rajasthan.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="don't wish to mention")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gaund")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Shah_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="goldsmith")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="halba")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="halbi")]="(5) Scheduled Tribes (ST) 5"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jhojha")]="(3) Other Backward Castes (OBC) 3" # https://bloodlineage.wordpress.com/2012/05/13/list-of-muslim-backward-castes-in-uttar-pradesh/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="julaha (momin)")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khati")]="(3) Other Backward Castes (OBC) 3" # http://haryanascbc.gov.in/list-of-backward-classes
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khatris")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Khatri
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khatriya")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kishwaha")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kushwaha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="koch")]="(3) Other Backward Castes (OBC) 3" # https://www.telegraphindia.com/states/north-east/koch-rajbongshis-on-obc-list/cid/474458
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kurmi")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/What-do-you-know-about-KURMI-Hindu-Caste-and-their-level-in-Indian-society
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ladshakhiya wani")]="(3) Other Backward Castes (OBC) 3" # https://timesofindia.indiatimes.com/city/nagpur/More-castes-included-in-backward-list/articleshow/29787773.cms
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lone")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mer rajput")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="multaani punjabi")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Jat_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="murai")]="(3) Other Backward Castes (OBC) 3" # http://www.delhi.gov.in/wps/wcm/connect/071ace004fc0691e9e8eff2402db5dfd/OBC_LIST.pdf?MOD=AJPERES
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nai")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Nai_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nayadu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Talk%3ANaidu
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="oc")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="patil")]="(2) Forward/General (except Brahmin) 2" # https://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Patil.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="scheduled caste")]="(4) Scheduled Castes (SC) 4"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sheikh")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Shaikh_of_Bihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sheikh muslim")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sikh")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sobti")]="(2) Forward/General (except Brahmin) 2" # potentially khatri https://www.google.com/search?client=ubuntu&hs=GEU&ei=3VqbXIiADZ6e1fAP_q6xaA&q=sobti+caste+in+india&oq=sobti+caste+in+india&gs_l=psy-ab.3...24741.24741..24903...0.0..0.53.53.1......0....2j1..gws-wiz.......0i71.yDwEPH1_CXU
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="tribal")]="(5) Scheduled Tribes (ST) 5"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="urs")]="(3) Other Backward Castes (OBC) 3" # https://www.deccanherald.com/content/219115/urs-community-placed-bc-list.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaish")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="zoroastrian")]=NA

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dawoodi bohra")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gujarati vaishnav")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jangra")]="(3) Other Backward Castes (OBC) 3" # http://www.delhi.gov.in/wps/wcm/connect/071ace004fc0691e9e8eff2402db5dfd/OBC_LIST.pdf?MOD=AJPERES
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kuduvakallige")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-the-3A-Vokkaliga-community-of-Karnataka-come-under-OBC-for-central-government-reservations
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumawat")]="(3) Other Backward Castes (OBC) 3" # http://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Kumawat.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="madhava brahmin")]="(1) Brahmin 1"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="shilpkar")]="(4) Scheduled Castes (SC) 4" # http://bedupako.com/blog/?p=14042
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="traders")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishnavas")]="(2) Forward/General (except Brahmin) 2"

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="aachari")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Is-the-Vishwakarma-caste-above-below-or-equal-to-the-Brahmin-caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="barujibi")]="(3) Other Backward Castes (OBC) 3" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bohra")]="(3) Other Backward Castes (OBC) 3"# https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="daivadhna")]="(2) Forward/General (except Brahmin) 2"  # https://www.quora.com/Is-Daivadnya-Brahmin-in-an-OBC-list
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khatri punjabi")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mahajan")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pathan")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="r.c")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="schedule tribe")]="(5) Scheduled Tribes (ST) 5"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunni muslim")]=NA 


NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="arya vysa")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Under-which-category-does-Arya-Vysya-come
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhandhari")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Bhandari_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bodo kochari")]="(5) Scheduled Tribes (ST) 5" # https://www.telegraphindia.com/india/st-status-plan-for-hill-bodos/cid/1479700
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dont know")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ezhva")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gond")]="(3) Other Backward Castes (OBC) 3" # https://www.asianage.com/india/all-india/200418/yogi-government-set-to-include-17-obc-castes-in-sc-list.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gurjar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Gurjar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hajam")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jain swetamber")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kamma choudhary")]="(2) Forward/General (except Brahmin) 2" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kashyap")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kashyap_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kaystha")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kshtriya")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kuduvakkalige")]="(3) Other Backward Castes (OBC) 3"  #https://en.wikipedia.org/wiki/Vokkaliga
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumhar")]="(4) Scheduled Castes (SC) 4" #https://en.wikipedia.org/wiki/Kumhar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kuruba")]="(3) Other Backward Castes (OBC) 3" #https://www.deccanchronicle.com/nation/current-affairs/271216/karnataka-cms-poll-gamble-kurubas-in-st-list.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mali")]="(3) Other Backward Castes (OBC) 3" #https://peoplegroupsindia.com/profiles/mali/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="marathi")]="(3) Other Backward Castes (OBC) 3" 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="oswal")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pardesi")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="prefer not to say")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="roman catholic")]=NA 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sali")]="(3) Other Backward Castes (OBC) 3" #http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sayed")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Syed-Muslims-in-India-belong-in-the-general-category
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="s.c.")]="(4) Scheduled Castes (SC) 4"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="soni")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-Saini-surname-comes-under-SC-caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="srivaishnava")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thakur")]="(2) Forward/General (except Brahmin) 2"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vohra")]="(3) Other Backward Castes (OBC) 3"

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="coorgi")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kodava_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="do not believe in caste system")]=NA
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jatavas")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chamar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumbhar")]="(3) Other Backward Castes (OBC) 3"
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kunbi")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kushwaha")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kushwaha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mohammad shaikh")]="(2) Forward/General (except Brahmin) 2"

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nepali")]=NA

NES14$jati = as.factor(as.character(unlist(NES14$jati)))

# complete the unknown st/sc by leveraging classification of NES14 
levels(NES14$jati)[which(levels(NES14$jati)=="brahmins")] = "(1) Brahmin 1"

NES14$jati = as.character(unlist(NES14$jati))
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="adi dravida")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Adi_Dravida
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="adi karnataka")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chalavadi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="agri")]="(3) Other Backward Castes (OBC) 3" # https://sje.gujarat.gov.in/ddcw/showpage.aspx?contentid=1738&lang=english
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ahom")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Ahom_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="andh/kondh")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ashrafs (sayyad shaikh)")]="(2) Forward/General (except Brahmin) 2" # http://twocircles.net/2010aug30/bihar_protests_and_politics_over_obc_status_ashraf_muslims.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="badhai (carpenters)")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/OBC_list.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bagdi duley")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Bagdi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="baiga")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Baiga_tribe
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="balmiki")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Valmiki_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="banik/bania/bjujel")]="(2) Forward/General (except Brahmin) 2" # http://en.banglapedia.org/index.php?title=Banik
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="banjara")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Banjara
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="barela")]="(5) Scheduled Tribes (ST) 5" # http://censusindia.gov.in/Tables_Published/SCST/ST%20Lists.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bauri")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Barujibi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhambi")]="(4) Scheduled Castes (SC) 4" # https://sje.gujarat.gov.in/dscw/showpage.aspx?contentid=1607&lang=english
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bharia bhum")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhattra")]="(3) Other Backward Castes (OBC) 3" # http://ncbc.nic.in/Writereaddata/cl/punjab.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhil")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhil mina")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Meena
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhilala")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Bhilala
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhotia")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Bhotiya
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhovi")]="(3) Other Backward Castes (OBC) 3" # https://www.thehindu.com/news/national/karnataka/identify-bhovi-community-members-as-schedule-caste/article7898808.ece
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhuian")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhumihars")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Bhumihar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhumij")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Bhumij
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bhuyan")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Bhuyan
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="binjhwar: binjhawar, binjhawar")]="(5) Scheduled Tribes (ST) 5" # https://indiankanoon.org/doc/32341750/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="boya, valmiki(in agency areas)")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Valmiki_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="boyar/mang")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="brus")]="(5) Scheduled Tribes (ST) 5" # https://countercurrents.org/2019/02/chakmas-won-the-case-of-reservation-in-guwahati-high-court
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="buddhists")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="bunkar(weavers)")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/OBC_list.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="can't say/don't know/no response")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chakma")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Scheduled_Tribes_in_West_Bengal
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chalavadi")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chalavadi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chaudhra, chaudhri")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chauhan")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Rajput
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chenchu")]="(5) Scheduled Tribes (ST) 5" # https://tribal.nic.in/DivisionsFiles/clm/3.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chero")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chero
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chettri")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="christians no caste/other christians")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="chutiya, koch")]="(3) Other Backward Castes (OBC) 3" # http://ncbc.nic.in/Writereaddata/cl/assam.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="craftsmen/weavers")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other peasent obc")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other service obcs")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="obc christians")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="trader obcs")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other muslim obc")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="obc sikh")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nomadic/service sc")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lowest sc")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other sc")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kuki tribes")]="(5) Scheduled Tribes (ST) 5" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other sts")]="(5) Scheduled Tribes (ST) 5" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dalit buddhists")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ex-untouchables/muslim dalits")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dalit christians")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other upper castes")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="general upper caste of assam")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other upper caste muslim")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="darzee(tailors)")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="das, malakar")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Malakar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="deh, bhat, bharbhuja, pinje")]="(3) Other Backward Castes (OBC) 3" # http://ncbc.nic.in/Writereaddata/cl/uk.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="devendrakula vellars")]="(4) Scheduled Castes (SC) 4" # https://www.thehindu.com/news/national/tamil-nadu/demand-to-exclude-devendra-kula-vellalars-from-scheduled-caste-list-fraught-with-risk/article23788609.ece
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dewan/khatri")]="(3) Other Backward Castes (OBC) 3" # https://wbxpress.com/list-other-backward-classes-west-bengal/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dhangar")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dhanuk")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Dhanuk
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dhobi (washermen)")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Dhobi
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dhobi,julaha,kewat(non-obc)")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="dom")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Biharis
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="entertaining castes")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ezhavas")]="(3) Other Backward Castes (OBC) 3" # https://www.outlookindia.com/website/story/in-kerala-temple-priest-appointments-backward-caste-ezhavas-overrun-brahmins/303133
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gaderia")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/uttarpradesh.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gamit")]="(5) Scheduled Tribes (ST) 5" # https://tribal.nic.in/DivisionsFiles/clm/3.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gond/rajgond")]="(5) Scheduled Tribes (ST) 5" # https://tribal.nic.in/DivisionsFiles/clm/3.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="gowari")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hadi")]="(4) Scheduled Castes (SC) 4" # http://stscodisha.gov.in/pdf/ScheduledCast_List.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="halpati, dubla")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/List_of_Scheduled_Tribes_in_Gujarat
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="hindu no caste")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="ho")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="holaya")]="(4) Scheduled Castes (SC) 4" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jalia kaibartta")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Jalia_Kaibarta
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jat (hindu only)")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Jat_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jat sikh")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jhalo malo malo")]="(4) Scheduled Castes (SC) 4" # https://shodhganga.inflibnet.ac.in/bitstream/10603/163227/11/11_conclusion.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jhimar")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Jhinwar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="jogi")]="(4) Scheduled Castes (SC) 4" # http://haryanascbc.gov.in/list-of-backward-classes
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="yerukula")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="yanadi")]="(5) Scheduled Tribes (ST) 5" # http://socialjustice.nic.in/writereaddata/UploadFile/Draft%20List%20of%20Denotified%20Tribes%20for%20Mail.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vettuva")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/list-scheduled-castes-kerala-state
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="veluthedathu nair")]="(3) Other Backward Castes (OBC) 3" # https://bcdd.kerala.gov.in/communities/state-obc-list/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="velan")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/list-scheduled-castes-kerala-state
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vanniyars")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Vanniyar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vankar")]="(4) Scheduled Castes (SC) 4" # https://sje.gujarat.gov.in/dscw/showpage.aspx?contentid=1607&lang=english
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vanjari")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaishya/bania")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="vaghri")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Vagri
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="toddytappers")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="todas")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thurpu kapu")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kapu_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thondai mandala saiva vellala")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thiruvalluvar")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thigala")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Thigala
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thatihar(make vessel)")]="(3) Other Backward Castes (OBC) 3" # http://adfdell.pstc.brown.edu/arisreds_data/reds99annexpartial.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="tharu")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Caste_system_in_Nepal
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="thandan")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Thandan
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="teli (oil pressers)")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Teli
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kamars")]="(3) Other Backward Castes (OBC) 3" # http://www.serviceonline.gov.in/resources/pdf/20/OBC.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kamboj")]="(3) Other Backward Castes (OBC) 3" # https://punjabxp.com/list-backward-other-classes/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kaora")]="(3) Other Backward Castes (OBC) 3" # http://www.scbc.bih.nic.in/ObcList.htm
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kapu, balija, telaga, ontari")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kapu_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karana")]="(2) Forward/General (except Brahmin) 2" # https://mahantysurname.blogspot.com/2017/06/mohanty-mahanti-mahanty-surname.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karku")]="(5) Scheduled Tribes (ST) 5" # https://tribal.nic.in/DivisionsFiles/clm/3.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="karwa patel/patidar")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kawar")]="(5) Scheduled Tribes (ST) 5" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kayasthas")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kayastha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kewat(fishermen & boatmen)")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Kewat
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khandayat/chasha, gudia")]="(2) Forward/General (except Brahmin) 2" # https://www.thehindu.com/news/national/other-states/Centrersquos-no-to-Orissa-on-including-Khandayat-caste-in-OBC-list/article16851310.ece
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kharia")]="(5) Scheduled Tribes (ST) 5" # https://serviceonline.gov.in/resources/pdf/20/ST.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kharwar")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Kharwar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khatik")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Khatik
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="khatri/arora sikh")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Khatri
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kirat rai")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="koli")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Koli_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="konda, dora, kapu")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="koppulu velama")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/OBC_list.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="korama")]="(2) Forward/General (except Brahmin) 2" # https://www.thehindu.com/news/national/karnataka/statewide-protest-for-removal-of-touchable-castes-from-sc/article21562207.ece
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kori")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Kori_caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="korwa")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Korwa_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kosti-sali-padmashali")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="koya, bhil")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumar")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Which-caste-General-OBC-SC-ST-does-the-Kumar-surname-belong-to
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumar, mali, koibarta")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kumhar(potters)")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="kuruva")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/list-scheduled-castes-kerala-state
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lakhera (make lac bangles)")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/rajasthan.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lambadi, sugali")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="landless labourers")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="leuva patel/patidar")]="(2) Forward/General (except Brahmin) 2" # https://time.com/4011001/hardik-patel-protest-arrest-gujarat-obc/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="leva-patil")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lingayats")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Lingayatism
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lodh")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Lodhi_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lohar (black smith)")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Lohar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lohra")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Lohar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="lotha")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunwar")]="(3) Other Backward Castes (OBC) 3" # http://www.ncbc.nic.in/Writereaddata/cl/sikkim.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunri(excluding saha)")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/OBC_list.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sunar(gold smith)")]="(5) Scheduled Tribes (ST) 5" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sumi")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sindhi")]="(2) Forward/General (except Brahmin) 2" # https://timesofindia.indiatimes.com/city/nagpur/Somes-Sindhis-demand-OBC-status/articleshow/36601089.cms
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sikh no caste/other sikhs")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="service muslims")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sawar")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/List_of_Scheduled_Tribes_in_India
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="savara")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Sora_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="satnami")]="(3) Other Backward Castes (OBC) 3" # http://adfdell.pstc.brown.edu/arisreds_data/annex-5.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="sanyasi")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/OBC_list.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="santhal")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Scheduled_Tribes_in_West_Bengal

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mahadev-koli")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mahli")]="(3) Other Backward Castes (OBC) 3" # http://www.serviceonline.gov.in/resources/pdf/20/OBC.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mal")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Mal_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mal pahariya")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Mal_Paharia_people
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mala")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Mala_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mala hannai")]="(4) Scheduled Castes (SC) 4" # https://rdpr.kar.nic.in/document/3.LIST_OF_SC&ST.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="malayali")]="(3) Other Backward Castes (OBC) 3" # https://bcdd.kerala.gov.in/communities/central-obc-list/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mali/saini")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="manjhi/nag")]="(4) Scheduled Castes (SC) 4" # https://timesofindia.indiatimes.com/city/lucknow/Manjhi-Kandu-castes-given-SC-status/articleshow/960303758.cms
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="maratha-kunbi")]="(3) Other Backward Castes (OBC) 3" # https://www.firstpost.com/india/marathas-and-kunbis-not-separate-castes-both-should-be-under-obc-maharashtra-backward-class-panel-report-5986451.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="marathas")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="matang/sarki")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Sarki_(ethnic_group)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mayavanshi")]="(4) Scheduled Castes (SC) 4" # https://www.change.org/p/narendra-modi-mahyavanshi-cast-certificate
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="medara, mahendra")]="(3) Other Backward Castes (OBC) 3" # https://bcdd.kerala.gov.in/communities/state-obc-list/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="megh")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Meghwal
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mina")]="(5) Scheduled Tribes (ST) 5" # https://www.quora.com/What-is-the-category-of-Meena-caste
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mudaliars")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Which-caste-is-higher-mudaliar-or-nadar-in-the-Hindu-caste-system
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mudiraj, mutraju, tenugollu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Mudiraju
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mughal (khan)")]="(2) Forward/General (except Brahmin) 2" # https://pib.gov.in/newsite/PrintRelease.aspx?relid=76106
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="munda")]="(5) Scheduled Tribes (ST) 5" # https://serviceonline.gov.in/resources/pdf/20/ST.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="munnuru kapu")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="muslim no caste/other muslims")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="mutharayars")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nagesia")]="(5) Scheduled Tribes (ST) 5" # https://en.wikipedia.org/wiki/Scheduled_Tribes_in_West_Bengal
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="nai(barber)")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Nai_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="naikda")]="(5) Scheduled Tribes (ST) 5" # https://tribal.nic.in/DivisionsFiles/clm/3.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="namashudras")]="(4) Scheduled Castes (SC) 4" # https://indiankanoon.org/doc/1251238/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="namasudra, dami")]="(4) Scheduled Castes (SC) 4" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="naqshbandi, suherwardi")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="newar")]="(2) Forward/General (except Brahmin) 2" # http://voiceofsikkim.com/2018/01/11/chettri-bahun-and-newar-communities-demands-central-obc-status/

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="oraon")]="(4) Scheduled Castes (SC) 4" # https://serviceonline.gov.in/resources/pdf/20/ST.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other craftsmen: mali, malakar")]="(3) Other Backward Castes (OBC) 3" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other minorities")]=NA # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="other pp")]=NA # 

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pano")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Pano_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="paraya")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/index.php/list-scheduled-castes-kerala-state
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pasi")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Pasi_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="patel/patidar")]="(2) Forward/General (except Brahmin) 2" # https://time.com/4011001/hardik-patel-protest-arrest-gujarat-obc/
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="patelia")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="peasants/traders: kayastha")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kayastha
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pod poundra")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Pod_(caste)
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="powar")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="pulaya")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/index.php/list-scheduled-castes-kerala-state
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="punjabi khatris")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Khatri-a-scheduled-caste

NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rajbanshis")]="(4) Scheduled Castes (SC) 4" # http://censusindia.gov.in/Tables_Published/SCST/dh_sc_westbengal.pdf
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rajbhar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Rajbhar
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rajput (peasant proprietors)")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rajputs")]="(2) Forward/General (except Brahmin) 2" # 
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="raju")]="(3) Other Backward Castes (OBC) 3" # https://www.newindianexpress.com/states/karnataka/2016/aug/01/Raju-Kshatriyas-Kunchitigas-promised-OBC-category-1500163.html
NES14$jati[which(trimws(tolower(NES14$jati),which = "both")  =="rathva")]="(5) Scheduled Tribes (ST) 5" # https://trti.gujarat.gov.in/rathwa
table(NES14$jati)

# Turnout
NES14$turnout <- NES14$q1
NES14$turnout = as.character(unlist(NES14$turnout))
NES14$turnout = ifelse(NES14$turnout=="1: I did not vote",0,
                       ifelse(NES14$turnout=="2: I am sure I voted",1,
                              NA))
# Vote Choice 
prop.table(table(NES14$q1a))
NES14$alliance_vote <- NES14$q1a
levels(NES14$alliance_vote) = sub(".*? ", "", levels(NES14$alliance_vote ))
levels(NES14$alliance_vote ) = gsub("\\(","",levels(NES14$alliance_vote ))
levels(NES14$alliance_vote ) = gsub("\\)","",levels(NES14$alliance_vote ))

NES14$alliance_vote = as.character(unlist(NES14$alliance_vote))
NES14$alliance_vote = ifelse(NES14$alliance_vote=="N.A.",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="say/No response",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="to reveal",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="NOTA",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="Party",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="parties",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="Don't want to reveal",NA,NES14$alliance_vote)
NES14$alliance_vote = ifelse(NES14$alliance_vote=="Don’t know/Can’t say/No response",NA,NES14$alliance_vote)
sort(unique(NES14$alliance_vote))

# # # # The following alliance members reflect onnly 2014 MEMBERSHIP
NES14$alliance_vote[which(NES14$alliance_vote=="AINRC")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="AD")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="Apna Dal AD")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="BJP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="DMDK")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="HJC")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="LJP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="MDMK")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="MNF")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="NPP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="PMK")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="RLSP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="RSPS")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="Paksha")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="RPI-A")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="RSP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="SAD")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="SS")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="Shiv Sena SS")] = "NDA"
#NES14$alliance_vote[which(NES14$alliance_vote=="SP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="Swabhiman Paksha")] = "NDA"
#NES14$alliance_vote[which(NES14$alliance_vote=="Samajwadi Party SP")] = "NDA"
NES14$alliance_vote[which(NES14$alliance_vote=="TDP")] = "NDA"
sort(unique(NES14$alliance_vote))

# now parties in the UPA
NES14$alliance_vote[which(NES14$alliance_vote=="BPF")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="INC")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="Congress")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="IUML")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="JKNC")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="JMM")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="NCP")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="RJD")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="RLD")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="RSP")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="Kerala Congress")] = "UPA" #
NES14$alliance_vote[which(NES14$alliance_vote=="JD-S")] = "UPA" #

#NES14$alliance_vote[which(NES14$alliance_vote=="CPI")] = "UPA" 
sort(unique(NES14$alliance_vote))
prop.table(table(NES14$alliance_vote))

# remaining = OTHER 
NES14$alliance_vote = ifelse(NES14$alliance_vote!="UPA" & NES14$alliance_vote!="NDA","OTHER",NES14$alliance_vote)
sort(unique(NES14$alliance_vote))
prop.table(table(NES14$alliance_vote))

NES14_clean <- NES14[,c("states","zones","gender","age_cat","religion","education_level","income_level","jati","turnout","alliance_vote")]
fwrite(NES14_clean,file = "Generated Quantities/NES14_clean.csv",row.names = FALSE)

