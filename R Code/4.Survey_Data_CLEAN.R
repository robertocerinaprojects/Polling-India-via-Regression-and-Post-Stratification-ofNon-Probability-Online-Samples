rm(list=ls())
options(scipen=999)
# set work directory
setwd(dir = "~/Dropbox/India 2019/")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Part 2: Load and consolidate oline surveys  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# a) CESS subject pool 
SP = read.csv("AMT Survey Data/Vote_India_main_March 20, 2019_03.45.csv")
SP_side = read.csv("AMT Survey Data/Vote_India_main - in progress-wave1-week803_March 20, 2019_03.33.csv")
SP_second = read.csv("AMT Survey Data/Vote_India_main_with_quota_April 8, 2019_02.12.csv")
SP_second$Q514_1 = NA; SP_second$Q514_2 = NA; SP_second$Q515_1 = NA; SP_second$Q515_2 = NA; SP_second$Q515_3 = NA
SP_third = read.csv("AMT Survey Data/Vote_India_main_with_quota_Wave 3_April 17, 2019_06.29.csv")
SP_second = rbind(SP_second,SP_third[-c(1:2),])

# Survey date 
survey_date = c(as.Date(substr(SP$EndDate,1,10),"%Y-%m-%d"),
                as.Date(substr(SP_side$EndDate,1,10),"%Y-%m-%d"),
                as.Date(substr(SP_second$EndDate,1,10),"%Y-%m-%d")
)
# Survey state
survey_state = c(ifelse(c(NA,NA,as.character(unlist(SP$Q136[-c(1,2)])))=="",NA,
                        c(NA,NA,as.character(unlist(SP$Q136[-c(1,2)])))),
                 
                 ifelse(c(NA,NA,as.character(unlist(SP_side$Q136[-c(1,2)])))=="",NA,
                        c(NA,NA,as.character(unlist(SP_side$Q136[-c(1,2)])))),
                 
                 ifelse(c(NA,NA,as.character(unlist(SP_second$Q136[-c(1,2)])))=="",NA,
                        c(NA,NA,as.character(unlist(SP_second$Q136[-c(1,2)]))))
)

# 2014 turnout
turnout_14 = c(NA,NA)
for(i in 3:dim(SP)[1]){
  turnout_14 = c(turnout_14,
              
              ifelse(length(which(SP[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                            as.character(unlist(SP[1,])))]!=""))==0,NA,
                     as.character(unlist(
                       SP[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                 as.character(unlist(SP[1,])))][
                                   which(SP[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                   as.character(unlist(SP[1,])))]!="")]))) ) }

turnout_14_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  turnout_14_side = c(turnout_14_side,
                   
                   ifelse(length(which(SP_side[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                      as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                          as.character(unlist(
                            SP_side[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                           as.character(unlist(SP_side[1,])))][
                                             which(SP_side[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                                  as.character(unlist(SP_side[1,])))]!="")]))) )
}


turnout_14_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  turnout_14_second = c(turnout_14_second,
                     
                     ifelse(length(which(SP_second[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                          as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                            as.character(unlist(
                              SP_second[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                               as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))][
                                                 which(SP_second[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                                        as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!="")]))) )
}

turnout_14 = c(turnout_14,turnout_14_side,turnout_14_second)

turnout_14 = 
ifelse(turnout_14=="Did not vote",'No',
       ifelse(turnout_14=="Was not eligible",NA,
              ifelse(turnout_14=="Can' remember / Don't want to say",NA,
                     ifelse(turnout_14=="Can't remember / don't want to say",NA,
                            ifelse(turnout_14=="Can't remember / Don't want to say",NA,"Yes")))))
# 2014 Vote_Choice
PC_vote_choice_14_Other = c(NA,NA)
for(i in 3:dim(SP)[1]){
  PC_vote_choice_14_Other = c(PC_vote_choice_14_Other, 
                           
                           ifelse(length(which(SP[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                         as.character(unlist(SP[1,])))]!=""))==0,NA,
                                  as.character(unlist(
                                    SP[i,
                                       which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP[1,]))) & 
                                               grepl('Other',as.character(unlist(SP[1,]))))][
                                                 which(SP[i,
                                                          which(
                                                            grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP[1,]))) & 
                                                              grepl('Other',as.character(unlist(SP[1,]))))
                                                          ]!="")]))) ) }

PC_vote_choice_14_Other_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  PC_vote_choice_14_Other_side = c(PC_vote_choice_14_Other_side, 
                                
                                ifelse(length(which(SP_side[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                                   as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                                       as.character(unlist(
                                         SP_side[i,
                                                 which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP_side[1,]))) & 
                                                         grepl('Other',as.character(unlist(SP_side[1,]))))][
                                                           which(SP_side[i,
                                                                         which(
                                                                           grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP_side[1,]))) & 
                                                                             grepl('Other',as.character(unlist(SP_side[1,]))))
                                                                         ]!="")]))) )
}

PC_vote_choice_14_Other_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  PC_vote_choice_14_Other_second = c(PC_vote_choice_14_Other_second, 
                                  
                                  ifelse(length(which(SP_second[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                                       as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                                         as.character(unlist(
                                           SP_second[i,
                                                     which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                             grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))][
                                                               which(SP_second[i,
                                                                               which(
                                                                                 grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                                                   grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))
                                                                               ]!="")]))) )
}
PC_vote_choice_14_Other = c(PC_vote_choice_14_Other,PC_vote_choice_14_Other_side,PC_vote_choice_14_Other_second )

# Clean 'Other' response in vote choice question 2014
PC_vote_choice_14 = c(NA,NA)
for(i in 3:dim(SP)[1]){
  PC_vote_choice_14 = c(PC_vote_choice_14, 
                     
                     ifelse(length(which(SP[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                   as.character(unlist(SP[1,])))]!=""))==0,NA,
                            as.character(unlist(
                              SP[i,
                                 which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP[1,]))) & 
                                         !grepl('Other',as.character(unlist(SP[1,]))))][
                                           which(SP[i,
                                                    which(
                                                      grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP[1,]))) & 
                                                        !grepl('Other',as.character(unlist(SP[1,]))))
                                                    ]!="")]))) ) }
PC_vote_choice_14_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  PC_vote_choice_14_side = c(PC_vote_choice_14_side, 
                          ifelse(length(which(SP_side[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                             as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                                 as.character(unlist(
                                   SP_side[i,
                                           which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP_side[1,]))) & 
                                                   !grepl('Other',as.character(unlist(SP_side[1,]))))][
                                                     which(SP_side[i,
                                                                   which(
                                                                     grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP_side[1,]))) & 
                                                                       !grepl('Other',as.character(unlist(SP_side[1,]))))
                                                                   ]!="")]))) ) }

PC_vote_choice_14_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  PC_vote_choice_14_second = c(PC_vote_choice_14_second, 
                            ifelse(length(which(SP_second[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                                 as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                                   as.character(unlist(
                                     SP_second[i,
                                               which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                       !grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))][
                                                         which(SP_second[i,
                                                                         which(
                                                                           grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                                             !grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))
                                                                         ]!="")]))) )}
PC_vote_choice_14 = c(PC_vote_choice_14,PC_vote_choice_14_side ,PC_vote_choice_14_second)

# CONVERT `OTHER` VOTES TO CORRECT CATEGORY
PC_vote_choice_14_Other = ifelse(grepl("\\bnational people's party\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bNPP\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bPMK\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bTMC\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\baap\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv admk\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjanasena\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnda\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnever vote\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnone\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnota\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot decided yet\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot sure\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot sure at this point in time\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bprefer not to say\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bundecided\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bun sure\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bagp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcannot discolose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcant say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bchoose not to disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdepends on candidate from my constituency\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdo not wish to answer\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdo not wish to specify\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi don't know yet.\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bisn't it a secret ballot?\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi will not say but it's not a constant answer. \\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi won’t say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi would vote for a candidate rather than the party\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnata\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bndy\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bno comments\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot interested to reply\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot yet decided\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnpp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bntk\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpolitical affiliation not to be revealed\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bprakash raj\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bregional party\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bstill making up my mind after party's evaluation\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btelugu desam party\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bthis information is confidential. i need not share such data.\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bxyz\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bno disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjammu and kashmir peoples movement\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnational democratic alliance\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcannot disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi would not like to say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bshivsena\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi will vote for the best candidate, not the party\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bindependent candidate\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsecret\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bbjp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwill not vote\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjammu and kashmir people's movement\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdon't want to disclose\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnaga peoples front\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))

PC_vote_choice_14_Other = tolower(PC_vote_choice_14_Other)

PC_vote_choice_14_Other = ifelse(grepl("\\bi don't vote\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi vote for the candidate and not the party\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjsp\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bmahagathbandhan\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))

PC_vote_choice_14_Other = ifelse(grepl("\\bi did not have my name on voter's list\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsdf\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi got married and found out that my name was struck off the voter list. it was too late to make amends. \\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bhad election duty\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\baiadmk\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwas not in india\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsamajwadi party\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwas on election duty\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))

# Turn into NDA, UPA, OTHER or NA
PC_vote_choice_14 = ifelse(grepl("\\bnational democratic alliance\\b",PC_vote_choice_14_Other),"National Democratic Alliance",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bunited progressive alliance\\b",PC_vote_choice_14_Other),"United Progressive Alliance",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bother\\b",PC_vote_choice_14_Other),"Other",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bndy\\b",PC_vote_choice_14_Other),NA,PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bno disclose\\b",PC_vote_choice_14_Other),NA,PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bHave not decided yet\\b",PC_vote_choice_14),NA,PC_vote_choice_14)
PC_vote_choice_14[which(PC_vote_choice_14=="Other" & is.na(PC_vote_choice_14_Other))]=NA

PC_vote_choice_14 = ifelse(PC_vote_choice_14=="Did not vote",NA,
                           ifelse(PC_vote_choice_14=="Was not eligible",NA,
                                  ifelse(PC_vote_choice_14=="Can' remember / Don't want to say",NA,
                                         ifelse(PC_vote_choice_14=="Can't remember / don't want to say",NA,
                                                ifelse(PC_vote_choice_14=="Can't remember / Don't want to say",NA,PC_vote_choice_14)))))

# Clean turnout question
turnout = c(NA,NA)
for(i in 3:dim(SP)[1]){
  turnout = c(turnout,
              
              ifelse(length(which(SP[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                            as.character(unlist(SP[1,])))]!=""))==0,NA,
                     as.character(unlist(
                       SP[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                 as.character(unlist(SP[1,])))][
                                   which(SP[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                   as.character(unlist(SP[1,])))]!="")]))) ) }

turnout_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  turnout_side = c(turnout_side,
                   
                   ifelse(length(which(SP_side[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                      as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                          as.character(unlist(
                            SP_side[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                           as.character(unlist(SP_side[1,])))][
                                             which(SP_side[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                                  as.character(unlist(SP_side[1,])))]!="")]))) )
}


turnout_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  turnout_second = c(turnout_second,
                     
                     ifelse(length(which(SP_second[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                          as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                            as.character(unlist(
                              SP_second[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                               as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))][
                                                 which(SP_second[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                                        as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!="")]))) )
}

turnout = c(turnout,turnout_side,turnout_second)

# Clean constituency vote choice
PC_vote_choice_Other = c(NA,NA)
for(i in 3:dim(SP)[1]){
  PC_vote_choice_Other = c(PC_vote_choice_Other, 
                           
                           ifelse(length(which(SP[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                         as.character(unlist(SP[1,])))]!=""))==0,NA,
                                  as.character(unlist(
                                    SP[i,
                                       which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP[1,]))) & 
                                               grepl('Other',as.character(unlist(SP[1,]))))][
                                                 which(SP[i,
                                                          which(
                                                            grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP[1,]))) & 
                                                              grepl('Other',as.character(unlist(SP[1,]))))
                                                          ]!="")]))) ) }

PC_vote_choice_Other_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  PC_vote_choice_Other_side = c(PC_vote_choice_Other_side, 
                                
                                ifelse(length(which(SP_side[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                                   as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                                       as.character(unlist(
                                         SP_side[i,
                                                 which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP_side[1,]))) & 
                                                         grepl('Other',as.character(unlist(SP_side[1,]))))][
                                                           which(SP_side[i,
                                                                         which(
                                                                           grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP_side[1,]))) & 
                                                                             grepl('Other',as.character(unlist(SP_side[1,]))))
                                                                         ]!="")]))) )
}

PC_vote_choice_Other_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  PC_vote_choice_Other_second = c(PC_vote_choice_Other_second, 
                                  
                                  ifelse(length(which(SP_second[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                                       as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                                         as.character(unlist(
                                           SP_second[i,
                                                     which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                             grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))][
                                                               which(SP_second[i,
                                                                               which(
                                                                                 grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                                                   grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))
                                                                               ]!="")]))) )
}
PC_vote_choice_Other = c(PC_vote_choice_Other,PC_vote_choice_Other_side,PC_vote_choice_Other_second )

# Clean 'Other' response in vote choice question 
# examine vote choice other - if NDA or UPA plug back in 
PC_vote_choice = c(NA,NA)
for(i in 3:dim(SP)[1]){
  PC_vote_choice = c(PC_vote_choice, 
                     
                     ifelse(length(which(SP[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                   as.character(unlist(SP[1,])))]!=""))==0,NA,
                            as.character(unlist(
                              SP[i,
                                 which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP[1,]))) & 
                                         !grepl('Other',as.character(unlist(SP[1,]))))][
                                           which(SP[i,
                                                    which(
                                                      grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP[1,]))) & 
                                                        !grepl('Other',as.character(unlist(SP[1,]))))
                                                    ]!="")]))) ) }
PC_vote_choice_side = c(NA,NA)
for(i in 3:dim(SP_side)[1]){
  PC_vote_choice_side = c(PC_vote_choice_side, 
                          ifelse(length(which(SP_side[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                             as.character(unlist(SP_side[1,])))]!=""))==0,NA,
                                 as.character(unlist(
                                   SP_side[i,
                                           which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP_side[1,]))) & 
                                                   !grepl('Other',as.character(unlist(SP_side[1,]))))][
                                                     which(SP_side[i,
                                                                   which(
                                                                     grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP_side[1,]))) & 
                                                                       !grepl('Other',as.character(unlist(SP_side[1,]))))
                                                                   ]!="")]))) ) }

PC_vote_choice_second = c(NA,NA)
for(i in 3:dim(SP_second)[1]){
  PC_vote_choice_second = c(PC_vote_choice_second, 
                            ifelse(length(which(SP_second[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                                 as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)])))]!=""))==0,NA,
                                   as.character(unlist(
                                     SP_second[i,
                                               which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                       !grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))][
                                                         which(SP_second[i,
                                                                         which(
                                                                           grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))) & 
                                                                             !grepl('Other',as.character(unlist(SP_second[1,1:(which(names(SP_second)=="Q514_1")-1)]))))
                                                                         ]!="")]))) )}
PC_vote_choice = c(PC_vote_choice,PC_vote_choice_side ,PC_vote_choice_second)

# CONVERT `OTHER` VOTES TO CORRECT CATEGORY
PC_vote_choice_Other = ifelse(grepl("\\bnational people's party\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bNPP\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bPMK\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_Other)),"United Progressive Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bTMC\\b",tolower(PC_vote_choice_Other)),"United Progressive Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\baap\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv admk\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bjanasena\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnda\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnever vote\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnone\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnota\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot decided yet\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot sure\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot sure at this point in time\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bprefer not to say\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_Other)),"United Progressive Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bundecided\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bun sure\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bagp\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcannot discolose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcant say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bchoose not to disclose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdepends on candidate from my constituency\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdo not wish to answer\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdo not wish to specify\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi don't know yet.\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bisn't it a secret ballot?\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi will not say but it's not a constant answer. \\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi won’t say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi would vote for a candidate rather than the party\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnata\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bndy\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bno comments\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot interested to reply\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot yet decided\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnpp\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bntk\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpolitical affiliation not to be revealed\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bprakash raj\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bregional party\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bstill making up my mind after party's evaluation\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btelugu desam party\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bthis information is confidential. i need not share such data.\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bxyz\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bno disclose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bjammu and kashmir peoples movement\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnational democratic alliance\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcannot disclose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi would not like to say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bshivsena\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi will vote for the best candidate, not the party\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bindependent candidate\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bsecret\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bbjp\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bwill not vote\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bjammu and kashmir people's movement\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdon't want to disclose\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaga peoples front\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = tolower(PC_vote_choice_Other)

PC_vote_choice_Other = ifelse(grepl("\\bi don't vote\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi vote for the candidate and not the party\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bjsp\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmahagathbandhan\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))

# Turn into NDA, UPA, OTHER or NA
PC_vote_choice = ifelse(grepl("\\bnational democratic alliance\\b",PC_vote_choice_Other),"National Democratic Alliance",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bunited progressive alliance\\b",PC_vote_choice_Other),"United Progressive Alliance",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bother\\b",PC_vote_choice_Other),"Other",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bndy\\b",PC_vote_choice_Other),NA,PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bno disclose\\b",PC_vote_choice_Other),NA,PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bHave not decided yet\\b",PC_vote_choice),NA,PC_vote_choice)
PC_vote_choice[which(PC_vote_choice=="Other" & is.na(PC_vote_choice_Other))]=NA

# PREPARE REMAINING COVARIATES TO PUT IN A DATAFRAME 
# Gender
gender = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q2"])),
           NA,NA,as.character(unlist(SP_side[-c(1:2),"Q2"])),
           NA,NA,as.character(unlist(SP_second[-c(1:2),"Q2"]))
)
# Age
age_cat = 
  c(as.character(unlist(
    cut(
      round(as.numeric(difftime(
        Sys.Date(),
        as.Date(apply(
          data.frame(byear = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q67"]))),
                     bmonth = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q69"]))),
                     bdate = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q328"])))),
          1,FUN = function(x){paste0(x,collapse = " ")}),"%Y %B %d"),units = "day"))/365),
      breaks = c(-1,17,24,34,44,54,64,150),
      labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-max]")))),
    
    as.character(unlist(cut(
      round(as.numeric(difftime(
        Sys.Date(),
        as.Date(apply(
          data.frame(byear = c(NA,NA,as.character(unlist(SP_side[-c(1:2),"Q67"]))),
                     bmonth = c(NA,NA,as.character(unlist(SP_side[-c(1:2),"Q69"]))),
                     bdate = c(NA,NA,as.character(unlist(SP_side[-c(1:2),"Q328"])))),
          1,FUN = function(x){paste0(x,collapse = " ")}),"%Y %B %d"),units = "day"))/365),
      breaks = c(-1,17,24,34,44,54,64,150),
      labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-max]")))),
    
    as.character(unlist(cut(
      round(as.numeric(difftime(
        Sys.Date(),
        as.Date(apply(
          data.frame(byear = c(NA,NA,as.character(unlist(SP_second[-c(1:2),"Q67"]))),
                     bmonth = c(NA,NA,as.character(unlist(SP_second[-c(1:2),"Q69"]))),
                     bdate = c(NA,NA,as.character(unlist(SP_second[-c(1:2),"Q328"])))),
          1,FUN = function(x){paste0(x,collapse = " ")}),"%Y %B %d"),units = "day"))/365),
      breaks = c(-1,17,24,34,44,54,64,150),
      labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-max]"))))
  )
# income
income_level = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q80"])),
                 NA,NA,as.character(unlist(SP_side[-c(1:2),"Q80"])),
                 NA,NA,as.character(unlist(SP_second[-c(1:2),"Q80"]))
)

# religion
religion = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q83"])),
             NA,NA,as.character(unlist(SP_side[-c(1:2),"Q83"])),
             NA,NA,as.character(unlist(SP_second[-c(1:2),"Q83"]))
)
# edu
edu = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q22"])),
        NA,NA,as.character(unlist(SP_side[-c(1:2),"Q22"])),
        NA,NA,as.character(unlist(SP_second[-c(1:2),"Q22"]))
)
# relevant edu levels from census targets: 
#"(00) none 0" # No formal education
#"(03) 1-4 3" # Incomplete primary school
#"(05) primary 5" # Completed primary school
#"(08) 6-9 8" # Middle pass / Matric fail
#"(10) Secondary(&11) 10" # Matric pass / 10th pass - 11th pass, not completed intermediate
##"(12) Higher sec(&13,14) 12" # 12th pass / Intermediate 
#"(15) graduate 15" # Bachelor's degree - Undergraduate, still in college
#"(16) some post-grad 16" # Postgraduate degree
edu = 
  ifelse(edu=="No formal education","(01) No Formal Edu 01",
         ifelse(edu=="Incomplete primary school","(02) Primary or Lower 02" ,
                ifelse(edu=="Completed primary school","(02) Primary or Lower 02" ,
                       ifelse(edu=="Middle pass / Matric fail","(03) Middle or Secondary 03",
                              ifelse(edu=="Matric pass / 10th pass","(03) Middle or Secondary 03",
                                     ifelse(edu=="11th pass, not completed intermediate","(03) Middle or Secondary 03",
                                            ifelse(edu=="12th pass / Intermediate"," (04) Higher Secondary 04",
                                                   ifelse(edu=="Bachelor's degree","(05) Some Graduate or Higher 05",
                                                          ifelse(edu=="Undergraduate, still in college","(05) Some Graduate or Higher 05",
                                                                 ifelse(edu=="Postgraduate degree","(05) Some Graduate or Higher 05",NA))))))))))

# Prepare Jati variable
jati = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q91"])),
         NA,NA,as.character(unlist(SP_side[-c(1:2),"Q91"])),
         NA,NA,as.character(unlist(SP_second[-c(1:2),"Q91"]))
)
jati_other = c(NA,NA,as.character(unlist(SP[-c(1:2),"Q91_13_TEXT"])),
               NA,NA,as.character(unlist(SP_side[-c(1:2),"Q91_13_TEXT"])),
               NA,NA,as.character(unlist(SP_second[-c(1:2),"Q91_13_TEXT"]))
)

# Clean Jati and assign broader Caste category
jati_other = ifelse(trimws(tolower(jati_other),which = "both") =="pubjabi","Punjabi",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shatriya","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vanniya kula kshatriyar","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="agamudiyar","Agamudiyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="andi pandaram","Andipandaram",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="badaga","Badaga",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="baishya","Baishya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="banya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="baniya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bc-b","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bhandari","Bhandari",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiyear","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chowdhary(kamma)","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="csi","Dalit",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="dalith","Dalit",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devangar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devar","Dewar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="fishermen","Dewar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ezhava","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gavara","Gavara",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="goundar","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gowda","Gowda",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gowdas","Gowda",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="humanity","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindhu","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindu","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindu vaniya settiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="india","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="jain","Jain",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kallar","Kallar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kamma","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kammalar","Kammalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kammavar","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kapu","Kapu",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kasar","Kasar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellala gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellalar gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu veller","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kshatriya","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="labbai","Labbai",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="latin catholic","Upper Caste Christians",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mala arayan","Dheevara",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mannar","Other",jati_other) # I think this is a location..
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mappila","Mappila",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maravar","Maravar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mbc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="most backward","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="menon","Nair",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mpc","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mudaliyar","Mudaliar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muslim","Muslim",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="nadar","Nadar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="naadar","Nadar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="naidu","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="obc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="nair","Nair",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmanayaka velama","Velama",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="patel","Patel",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pattunulkarar","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillai","Pillai",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillamar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saiva vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliar","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliya","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliyar","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sc","SC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="schedule caste","SC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="scheduled tribe","ST",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shahib",NA,jati_other) #couldn't find correspondent 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shetty","Bunt",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sourashtra","Sourashtra",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sourastra","Sourashtra",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhila vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhlia vellaler","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sri karuneegar","Karuneegar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sunni","Muslim",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="tamil","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="tamilan","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="telugu chettiar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thachan","Thachan",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thiya","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thiyya","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thuluva vellala","Thuluva Vellala",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="valaiyar","Valaiyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniga chettiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniyan","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniyar","Vanniyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vannier kula satisreyar","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vanniyar","Vanniyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="veera shaiva","Veerasaiva",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="viswakarma","Viswakarma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vysya","Komati",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadav","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadava","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadhav","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="lohar(obc)","Lohar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="",NA,jati_other)
# second wave of "other" checks
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="adi dravidar","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="agamudaiyar","Kallar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ambalakarar","Ambalakarar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ampalathar","Ampalathar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="arora","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="backwardclass","OBC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ballija","Kapu",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chamar","Chamar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="dakkani","Dakkhani",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devang","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="eezhava","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gounders","Goundar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kalinga","Kalinga",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="khatri","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="khukrain","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu gounder","Goundar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellalar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mahar","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mapla","Mappila",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maratha","Maratha",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maravr","Maravar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maruthuvar","Maruthuvar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="marwadi",NA,jati_other) # marwari is a regional belonging not cast system
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muslim mappila","Mappila",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mutharayar","Mutharayar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muthuraja","Mutharayar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="obc","OBC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmanayaka velams","Kapu",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmasali","Chettiar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="parkavakulam","Parkavakulam",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pbc","OBC",jati_other) # pbc means poor backeards caste
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillaymar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="reddy","Reddy",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="rowther","Rowther",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sc (adi dravida)","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shree karuneegar","Karuneegar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="siva pillai","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhilavellalor","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhia vellalar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sonar","Sunar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="upper caste","OFC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="upper caste christians","OFC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vadugar","Vadugar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="varier","Nair",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwabrahmin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwa brahmin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwabramin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwakarma","Viswakarma",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwakarma","Viswakarma",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadavs","Yadhava",jati_other) 

jati_other[which(trimws(tolower(jati_other),which = "both")  =="agamudiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="andipandaram")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="badaga")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="baishya")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="banya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhandari")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bunt")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chettiar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dalit")]="(4) Scheduled Castes (SC) 4"#"Dalit"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dewar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dheevara")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ezhava")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gavara")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="goundar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gowda")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jain")]=NA#"Jain" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kallar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kamma")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kammalar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kapu")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karuneegar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kasar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="komati")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshatriya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="labbai")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mappila")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maravar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mudaliar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muslim")]=NA#"Muslim"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nadar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nair")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="other")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="patel")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pillai")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="saliya")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sc")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sourashtra")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thachan")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thuluva vellala")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="upper caste christians")]="Upper Caste Christians"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="valaiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vanniyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="veerasaiva")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="velama")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vellalar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="viswakarma")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="yadhava")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lohar")]="(3) Other Backward Castes (OBC) 3"
# second wave of checks 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="obc")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ofc")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="st")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="reddy")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="arora")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="brahmin")]="(1) Brahmin 1"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maratha")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="parkavakulam")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalinga")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vadugar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="arunthathiyar")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rowther")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mutharayar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ambalakarar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dakkhani")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ampalathar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maruthuvar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chamar")]="(4) Scheduled Castes (SC) 4"
# wave three of edits
jati_other[which(trimws(tolower(jati_other),which = "both")  =="agarwal")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Does-the-Agarwal-community-come-under-the-OBC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ahir")]="(3) Other Backward Castes (OBC) 3" # https://timesofindia.indiatimes.com/city/nagpur/More-castes-included-in-backward-list/articleshow/29787773.cms
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ahl-e-quraish")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="assamese")]=NA # region not caste 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="baishnab")]="(3) Other Backward Castes (OBC) 3" #https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
jati_other[which(trimws(tolower(jati_other),which = "both")  =="balija")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bania")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="besthar")]="(5) Scheduled Tribes (ST) 5" # have asked to be part of ST https://en.wikipedia.org/wiki/Bestha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhat")]="(3) Other Backward Castes (OBC) 3" #http://www.rtifoundationofindia.com/appellant-why-caste-%E2%80%9Cbhat%E2%80%9D-was-included-obcs-while#.XIvhTx_njRY
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhumihar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="businessmen teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chatriya")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dawoodi bhora")]="(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ghanchi")]="(3) Other Backward Castes (OBC) 3" #https://en.wikipedia.org/wiki/Ghanchi
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gouda")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gujjar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Gurjar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gupta")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not
jati_other[which(trimws(tolower(jati_other),which = "both")  =="harijans (merchants/traders of frowned products like alcohol)")]="(4) Scheduled Castes (SC) 4" # https://www.britannica.com/topic/untouchable
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu mangela")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="islam")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jatav")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Jatav
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kahar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kahar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kaisth")]="(2) Forward/General (except Brahmin) 2" #https://en.wikipedia.org/wiki/Kayastha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalita")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kalita_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karmas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kashmiri pandit")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kayasth")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kayastha")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khan")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khandayat")]="(3) Other Backward Castes (OBC) 3" # https://www.telegraphindia.com/states/jharkhand/demand-to-be-on-obc-list/cid/717275
jati_other[which(trimws(tolower(jati_other),which = "both")  =="koeri")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/What-is-the-difference-between-Kurmi-Koeri-and-Kushwaha-castes-of-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kutchi visa oswal")]="(6) Others 6" # these are jain 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lingayath")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
jati_other[which(trimws(tolower(jati_other),which = "both")  =="malwadi")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="marwari")]= NA # this is a region https://www.quora.com/Are-marwaris-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maurya")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Which-caste-does-the-surname-Maurya-belong-to
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mudhaliyar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="naga")]="(5) Scheduled Tribes (ST) 5" # https://www.quora.com/What-is-the-exact-difference-between-various-reserved-categories-like-scheduled-caste-schedule-tribe-and-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="none")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="not known")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="not sure")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="others")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="paswan")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Paswan
jati_other[which(trimws(tolower(jati_other),which = "both")  =="phatan")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-all-Muslims-in-India-fall-under-OBC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="patidar")]="(4) Scheduled Castes (SC) 4"  # https://en.wikipedia.org/wiki/Patidar_reservation_agitation
jati_other[which(trimws(tolower(jati_other),which = "both")  =="phatans")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pathans")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="punjabi")]="(6) Others 6"  # regional denomination
jati_other[which(trimws(tolower(jati_other),which = "both")  =="punjabi khatri")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sahu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sahu
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sanamahi")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-manipur/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sharma")]="(1) Brahmin 1" # https://www.quora.com/What-category-does-Anurag-Sharma-belong-to-general-or-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sud")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunri")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sundhi
jati_other[which(trimws(tolower(jati_other),which = "both")  =="syed shia")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Syed-Muslims-in-India-belong-in-the-general-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="unknown")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vatishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vokkaliga")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-the-3A-Vokkaliga-community-of-Karnataka-come-under-OBC-for-central-government-reservations
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vyshyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vysyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
jati_other[which(trimws(tolower(jati_other),which = "both")  =="yadavas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya

jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshyatriya")]="(2) Forward/General (except Brahmin) 2" # 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vannya")]="(3) Other Backward Castes (OBC) 3" #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ramgarhia")]="(3) Other Backward Castes (OBC) 3" #

jati_other[which(trimws(tolower(jati_other),which = "both")  =="atheist from brahmin family")]="(1) Brahmin 1" #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="don't know")]=NA #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalinga vysya")]="(3) Other Backward Castes (OBC) 3" # https://www.thehindu.com/news/national/andhra-pradesh/obc-status-for-kalinga-vysyas-sistakaranams/article7537421.ece
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lingayat")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lohana")]="(3) Other Backward Castes (OBC) 3" # http://entrance-exam.net/forum/general-discussion/does-lohana-caste-gujarati-come-under-obc-category-state-maharashtra-257223.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mistri")]= "(3) Other Backward Castes (OBC) 3"# https://en.wikipedia.org/wiki/Mistri_caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="momin ansar")]= "(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
jati_other[which(trimws(tolower(jati_other),which = "both")  =="shia's")]=NA

jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhumihaar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="forward / general")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hnidu")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshatriyas")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="naicker")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nayakar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="parayan")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Does-the-parayan-caste-come-under-SC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rawuthar")]="(3) Other Backward Castes (OBC) 3" # http://www.ncbc.nic.in/Writereaddata/1123.PDF
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sozhiya vellalar")]= "(3) Other Backward Castes (OBC) 3"# http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="upper caste hindu")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishnava")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="arya vyshya")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Under-which-category-does-Arya-Vysya-come
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bori")]="(3) Other Backward Castes (OBC) 3" # http://www.firstfoundation.in/Soc/List-OBC-Mah.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="harijans (businessman/trader of immoral goods e.g. alcohol)")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishya")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not

jati_other[which(trimws(tolower(jati_other),which = "both")  =="buddihist")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bunts")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/How-do-Bunts-belonged-to-OBC-or-general
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chakkiliar")]="(4) Scheduled Castes (SC) 4" #  https://en.wikipedia.org/wiki/Arunthathiyar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chittiyar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="devanga")]="(3) Other Backward Castes (OBC) 3" # these are chettiar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu-nadar")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karnam")]="(2) Forward/General (except Brahmin) 2" # http://pkpi4u.blogspot.com/2014/07/karanam-or-karana-is-caste-mostly.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kodava")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kodava_people
jati_other[which(trimws(tolower(jati_other),which = "both")  =="manipuri")]=NA # this is a region
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muthaliyar")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nair (upper caste)")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pattani")]="(3) Other Backward Castes (OBC) 3" # https://wikivisually.com/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="saini")]="(3) Other Backward Castes (OBC) 3" # https://punjabxp.com/list-backward-other-classes/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishyas")]="(2) Forward/General (except Brahmin) 2" 

jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward class")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu - nair")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kaikolar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="padmanayaka velamas")]="(2) Forward/General (except Brahmin) 2"  # https://en.wikipedia.org/wiki/List_of_Telugu_castes
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thiyya(obc)")]="(3) Other Backward Castes (OBC) 3" 


jati_other[which(trimws(tolower(jati_other),which = "both")  =="bouddh")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ganaka")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-kerala/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu salian")]="(3) Other Backward Castes (OBC) 3"  # https://sanjaykumarnishad.wordpress.com/category/obc/central-list-of-other-backward-castes-obcs/page/2/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maheshwari")]= "(2) Forward/General (except Brahmin) 2" #https://www.jeevansathi.com/rajasthani-maheshwari-matrimony-matrimonials
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pramalai kallar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="velar")]="(3) Other Backward Castes (OBC) 3" # https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes


jati_other[which(trimws(tolower(jati_other),which = "both")  =="chattiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chettyiar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nagker")]=NA#nagar? 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nayar")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Why-is-Nair-not-in-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thevar")]="(3) Other Backward Castes (OBC) 3" # https://www.thenewsminute.com/article/thevar-factor-who-real-aiadmk-dominant-obc-community-70804

jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishya (agrawal)")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="aggarwal")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="ambastha")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Ambashtha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="badhai")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="baidya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bengali zamindari kayastha")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhatia")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Bhatia_caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="brahmin by birth but married to  non brahmin")]="(1) Brahmin 1"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chambhar")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chamar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chowdaries")]="(2) Forward/General (except Brahmin) 2" # kamma https://en.wikipedia.org/wiki/Kamma_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dhakad")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/rajasthan.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="don't wish to mention")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gaund")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Shah_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="goldsmith")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="halba")]="(5) Scheduled Tribes (ST) 5" # http://www.ymnonline.com/data/stureg/caste.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="halbi")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jhojha")]="(3) Other Backward Castes (OBC) 3" # https://bloodlineage.wordpress.com/2012/05/13/list-of-muslim-backward-castes-in-uttar-pradesh/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="julaha (momin)")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khati")]="(3) Other Backward Castes (OBC) 3" # http://haryanascbc.gov.in/list-of-backward-classes
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khatris")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Khatri
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khatriya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kishwaha")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kushwaha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="koch")]="(3) Other Backward Castes (OBC) 3" # https://www.telegraphindia.com/states/north-east/koch-rajbongshis-on-obc-list/cid/474458
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kurmi")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/What-do-you-know-about-KURMI-Hindu-Caste-and-their-level-in-Indian-society
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ladshakhiya wani")]="(3) Other Backward Castes (OBC) 3" # https://timesofindia.indiatimes.com/city/nagpur/More-castes-included-in-backward-list/articleshow/29787773.cms
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lone")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mer rajput")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="multaani punjabi")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Jat_people
jati_other[which(trimws(tolower(jati_other),which = "both")  =="murai")]="(3) Other Backward Castes (OBC) 3" # http://www.delhi.gov.in/wps/wcm/connect/071ace004fc0691e9e8eff2402db5dfd/OBC_LIST.pdf?MOD=AJPERES
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nai")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Nai_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nayadu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Talk%3ANaidu
jati_other[which(trimws(tolower(jati_other),which = "both")  =="oc")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="patil")]="(2) Forward/General (except Brahmin) 2" # https://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Patil.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="scheduled caste")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sheikh")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Shaikh_of_Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sheikh muslim")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sikh")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sobti")]="(2) Forward/General (except Brahmin) 2" # potentially khatri https://www.google.com/search?client=ubuntu&hs=GEU&ei=3VqbXIiADZ6e1fAP_q6xaA&q=sobti+caste+in+india&oq=sobti+caste+in+india&gs_l=psy-ab.3...24741.24741..24903...0.0..0.53.53.1......0....2j1..gws-wiz.......0i71.yDwEPH1_CXU
jati_other[which(trimws(tolower(jati_other),which = "both")  =="tribal")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="urs")]="(3) Other Backward Castes (OBC) 3" # https://www.deccanherald.com/content/219115/urs-community-placed-bc-list.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaish")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="zoroastrian")]=NA

jati_other[which(trimws(tolower(jati_other),which = "both")  =="dawoodi bohra")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gujarati vaishnav")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jangra")]="(3) Other Backward Castes (OBC) 3" # http://www.delhi.gov.in/wps/wcm/connect/071ace004fc0691e9e8eff2402db5dfd/OBC_LIST.pdf?MOD=AJPERES
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kuduvakallige")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-the-3A-Vokkaliga-community-of-Karnataka-come-under-OBC-for-central-government-reservations
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kumawat")]="(3) Other Backward Castes (OBC) 3" # http://ipfs.io/ipfs/QmXoypizjW3WknFiJnKLwHCnL72vedxjQkDDP1mXWo6uco/wiki/Kumawat.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="madhava brahmin")]="(1) Brahmin 1"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="shilpkar")]="(4) Scheduled Castes (SC) 4" # http://bedupako.com/blog/?p=14042
jati_other[which(trimws(tolower(jati_other),which = "both")  =="traders")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishnavas")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="aachari")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Is-the-Vishwakarma-caste-above-below-or-equal-to-the-Brahmin-caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="barujibi")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bohra")]="(3) Other Backward Castes (OBC) 3"# https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="daivadhna")]="(2) Forward/General (except Brahmin) 2"  # https://www.quora.com/Is-Daivadnya-Brahmin-in-an-OBC-list
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khatri punjabi")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mahajan")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pathan")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="r.c")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="schedule tribe")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunni muslim")]=NA 


jati_other[which(trimws(tolower(jati_other),which = "both")  =="arya vysa")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Under-which-category-does-Arya-Vysya-come
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhandhari")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Bhandari_caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bodo kochari")]="(5) Scheduled Tribes (ST) 5" # https://www.telegraphindia.com/india/st-status-plan-for-hill-bodos/cid/1479700
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dont know")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ezhva")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gond")]="(3) Other Backward Castes (OBC) 3" # https://www.asianage.com/india/all-india/200418/yogi-government-set-to-include-17-obc-castes-in-sc-list.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gurjar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Gurjar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hajam")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jain swetamber")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kamma choudhary")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kashyap")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kashyap_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kaystha")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshtriya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kuduvakkalige")]="(3) Other Backward Castes (OBC) 3"  #https://en.wikipedia.org/wiki/Vokkaliga
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kumhar")]="(4) Scheduled Castes (SC) 4" #https://en.wikipedia.org/wiki/Kumhar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kuruba")]="(3) Other Backward Castes (OBC) 3" #https://www.deccanchronicle.com/nation/current-affairs/271216/karnataka-cms-poll-gamble-kurubas-in-st-list.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mali")]="(3) Other Backward Castes (OBC) 3" #https://peoplegroupsindia.com/profiles/mali/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="marathi")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="oswal")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pardesi")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="prefer not to say")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="roman catholic")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sali")]="(3) Other Backward Castes (OBC) 3" #http://www.ymnonline.com/data/stureg/caste.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sayed")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Syed-Muslims-in-India-belong-in-the-general-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="s.c.")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="soni")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-Saini-surname-comes-under-SC-caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="srivaishnava")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thakur")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vohra")]="(3) Other Backward Castes (OBC) 3"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="coorgi")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kodava_people
jati_other[which(trimws(tolower(jati_other),which = "both")  =="do not believe in caste system")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jatavas")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Chamar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kumbhar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kunbi")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kushwaha")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kushwaha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mohammad shaikh")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="nepali")]=NA

sort(trimws(unique(tolower(jati_other)),which = "both") )

# varier are potentially members of nair - https://en.wikipedia.org/wiki/Talk%3ANair_subcastes
# vadugar is obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Sunar is obc - https://www.quora.com/Do-the-%E2%80%9CPitale%E2%80%9D-and-%E2%80%9CSonar%E2%80%9D-surnames-fall-under-the-OBC-category
# Karnueegar are obc - https://en.wikipedia.org/wiki/Karuneegar
# rowther are shipping mechant obc - https://groups.google.com/forum/#!topic/soc.culture.indian/ezJIiSdvQB0
# reddy is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# parkavakulam are obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# padmasali are devanga (chettiar) - https://en.wikipedia.org/wiki/Padmashali
# velama are the same as Kapu - https://en.wikipedia.org/wiki/Kapu_(caste)
#mutharayar is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# "maruthuvar is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# mahar is scheduled chaste
# maratha are obc for all intents and purposes - https://en.wikipedia.org/wiki/Maratha
# khukrain are like khatri 
# khatri are like arora 
# kalinga are obc https://www.thehindu.com/news/national/andhra-pradesh/obc-status-for-kalinga-vysyas-sistakaranams/article7537421.ece
# Dakkhani is obc - http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
# Chamar is Dalit (scheduled caste)
# ballija same as Kapu -https://www.cs.odu.edu/~salam/wsdl/inforet/wikihtml/Balija.html
# Arunthathiyar is dalit - https://en.wikipedia.org/wiki/Arunthathiyar
# Arora is a forward caste - https://www.answers.com/Q/Does_arora_belongs_to_obc_category
# Sindhis we can classify as OBC - https://timesofindia.indiatimes.com/city/nagpur/Somes-Sindhis-demand-OBC-status/articleshow/36601089.cms
# Jat can be connsidered OBC - https://en.wikipedia.org/wiki/Jat_reservation_agitation
# Jogi can be considered OBC - https://en.wikipedia.org/wiki/Jogi
# Chamars are Dalit - https://en.wikipedia.org/wiki/Chamar
# Dhanak are scheduled casts - http://haryanascbc.gov.in/list-of-scheduled-castes and other states 
# Kori is a scheduled caste - https://en.wikipedia.org/wiki/Kori_caste
# Ashraf are upper class muslims - https://en.wikipedia.org/wiki/Caste_system_in_India
# Rajputs are a mix - upper caste in a few states in the north - assume FC - https://www.quora.com/Is-Rajput-come-in-forward-caste-or-OBC
# Kayashta are forward class -https://en.wikipedia.org/wiki/Kayastha
# Jath Sikh are sikhs
# Upper caste christians are christians 
# Kshatriya are other forward class
# Banya are other forward class - https://www.quora.com/Who-is-in-the-Teli-or-Baniya-castes 
# khatri are sikh banya - other forward class 
# Agamudiyar is OBC - https://en.wikipedia.org/wiki/Agamudayar
# andi pandaram - OBC
# badaga is OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# baishya is OBC - https://en.wikipedia.org/wiki/Baishya_Kapali
# bhandari is OBC - https://en.wikipedia.org/wiki/Bhandari_caste
# Chettiar is OBC - https://en.wikipedia.org/wiki/Chettiar
# Kamma are Forward - https://en.wikipedia.org/wiki/Kamma_(caste)
# CSI are largely dalits  https://www.bbc.co.uk/news/world-south-asia-11229170
# Devar are scheduled cast - no SC available set to OBC - fishermen - https://en.wikipedia.org/wiki/Dewar_(caste)
# ezhava are obc-  https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes
# gavara are obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Goundar are obc - https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf
# gowda are obc - https://economictimes.indiatimes.com/news/politics-and-nation/i-was-indias-first-obc-pm-not-narendra-modi-h-d-deve-gowda/articleshow/48047326.cms
# Kallar OBC- https://www.quora.com/Is-kallar-caste-is-a-OBC-category
# Kammalar OBC- http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# Kapu DMC in some places but OBC mostly - https://en.wikipedia.org/wiki/Kapu_(caste)
# Kasar are OBC - https://obccastegovtschemes.wordpress.com/2016/10/12/other-backward-classes-caste-list-obc-caste-in-maharashtra/
# labbai are OBC - https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
# Dheevara are OBC - https://en.wikipedia.org/wiki/Dheevara_(caste)
# Mappila are Muslim OBC 
# Maravar are OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# mbc is most backward caste - categorize as OBC 
# Nair is a forward class - https://www.quora.com/Why-is-Nair-not-in-OBC
# Mudaliar is a forward class - https://en.wikipedia.org/wiki/Mudaliar
# Nadar is a backward class - https://uk.answers.yahoo.com/question/index?qid=20121216193425AA9oL1q&guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAMvTFJWJEUspJhEEk9mEr0oM68L0lx1XPssFcVf4AThmGvlRg9kBm4TrrPUCiN1mdw3rbEzW2Xz5nAbABeAM30tTPf44OmXuOJMQwnIn4RbkfiviXWlPJoZEDOpM9axwelr8gc7J7--Ln1eWYPcUJNYRbBAyh_cdaZvYXGA60-ps
# Velama are forward class - https://www.vepachedu.org/castemore.htm
# Patel are SC/STand OBC - http://time.com/4011001/hardik-patel-protest-arrest-gujarat-obc/
# Pillai are forward caste - https://www.quora.com/Which-caste-category-is-Pillai-under
# Vellalar are OBC  - https://en.wikipedia.org/wiki/Vellalar
# Saliya are OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Bunt are OBC - http://mangaloretopnews.blogspot.com/2011/06/bunt-communitys-obc-status-in-central.html
# sourashtra OBC- http://www.bcmbcmw.tn.gov.in/bclist.htm
# Karuneegar OBC- http://www.bcmbcmw.tn.gov.in/bclist.htm
# Thachan OBC - https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes
# thuluva vellala OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# valaiyar are OBC - https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf
# vanniyar are OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# veera shaiva are OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# viswakarma OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Komati OBC - https://www.quora.com/Under-which-category-does-Arya-Vysya-come
# Yadhava OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Lohar are OBC (stated by people answering survey LOHAR(OBC))
# https://en.wikipedia.org/wiki/Adi_Dravida are Dalit
# Kallar and Agamudaiyar are part of the same caste
# Ambalakarar are OBC https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf

# now bring the standard casts into the fold (the targets)
jati[which(jati=="Brahmin")] = "(1) Brahmin 1"
jati[which(jati=="Sindhi")] = "(2) Forward/General (except Brahmin) 2"
jati[which(jati=="Jat")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Jogi (Mendicants and seek Alms)")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Chamar")] = "(4) Scheduled Castes (SC) 4"
jati[which(jati=="Dhanak")] = "(5) Scheduled Tribes (ST) 5"
jati[which(jati=="Kori")] = "(5) Scheduled Tribes (ST) 5"
jati[which(jati=="Ashrafs (Sayyad Shaikh)")] = "(2) Forward/General (except Brahmin) 2" # upper cast muslim
jati[which(jati=="Rajput (Peasant proprietors)")] = "(2) Forward/General (except Brahmin) 2"
jati[which(jati=="Peasants/Traders Kayastha")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Jath Sikh")] = "(2) Forward/General (except Brahmin) 2" # upper sikhs
jati[which(jati=="Upper Caste Christians")] = "(2) Forward/General (except Brahmin) 2"# upper christians
jati[which(jati=="Does not apply")] = "(6) Others 6"
jati[which(jati=="")] = NA
jati = ifelse(jati!="Other",jati,jati_other)

# Subject Pool Survey
SPurk_Survey = 
  data.frame(
    survey_date = as.character(unlist(survey_date)),
    survey_state = as.character(unlist(survey_state)),
    ID_GBIP = as.character(unlist(c(NA,NA,as.character(unlist(SP$IPAddress))[-c(1:2)],
                                    NA,NA,as.character(unlist(SP_side$IPAddress))[-c(1:2)],
                                    NA,NA,as.character(unlist(SP_second$IPAddress))[-c(1:2)]
    ))),
    gender = as.character(unlist(gender)), 
    age_cat = as.character(unlist(age_cat)),
    income_level = as.character(unlist(income_level)),
    religion = as.character(unlist(religion)),
    education_level = as.character(unlist(edu)), 
    jati = as.character(unlist(jati)),
    turnout = as.character(unlist(turnout)),
    turnout_14 = as.character(unlist(turnout_14)),
    PC_vote_choice = as.character(unlist(PC_vote_choice)),
    PC_vote_choice_14 = as.character(unlist(PC_vote_choice_14)),
    Source = as.character(unlist(rep("SubjectPool",length(PC_vote_choice))))
  )[-which(is.na( as.character(unlist(c(NA,NA,as.character(unlist(SP$IPAddress))[-c(1:2)],
                                        NA,NA,as.character(unlist(SP_side$IPAddress))[-c(1:2)],
                                        NA,NA,as.character(unlist(SP_second$IPAddress))[-c(1:2)]
  ))))),] 

write.csv(SPurk_Survey,"Generated Quantities/SPurk_Survey.csv",row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# b) Mechanical Turk Surveys 
AMT = read.csv("AMT Survey Data/cessonline-India-onboard-EXPOST -- AMTurk_May 28, 2019_03.22.csv")

# Survey date
survey_date = as.Date(substr(AMT$EndDate,1,10),"%Y-%m-%d")

# Survey state
survey_state = ifelse(c(NA,NA,as.character(unlist(AMT$Q97[-c(1,2)])))=="",NA,c(NA,NA,as.character(unlist(AMT$Q97[-c(1,2)]))))

# 2014 turnout
turnout_14 = c(NA,NA)

for(i in 3:dim(AMT)[1]){
  turnout_14 = c(turnout_14,
                 
                 ifelse(length(which(AMT[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                               as.character(unlist(AMT[1,])))]!=""))==0,NA,
                        as.character(unlist(
                          AMT[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                    as.character(unlist(AMT[1,])))][
                                      which(AMT[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                      as.character(unlist(AMT[1,])))]!="")]))) ) }

turnout_14 = 
  ifelse(turnout_14=="Did not vote",'No',
         ifelse(turnout_14=="Was not eligible",NA,
                ifelse(turnout_14=="Can' remember / Don't want to say",NA,
                       ifelse(turnout_14=="Can't remember / don't want to say",NA,
                              ifelse(turnout_14=="Can't remember / Don't want to say",NA,"Yes")))))
# 2014 Vote_Choice
PC_vote_choice_14_Other = c(NA,NA)
for(i in 3:dim(AMT)[1]){
  PC_vote_choice_14_Other = c(PC_vote_choice_14_Other, 
                              
                              ifelse(length(which(AMT[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                            as.character(unlist(AMT[1,])))]!=""))==0,NA,
                                     as.character(unlist(
                                       AMT[i,
                                          which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(AMT[1,]))) & 
                                                  grepl('Other',as.character(unlist(AMT[1,]))))][
                                                    which(AMT[i,
                                                             which(
                                                               grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(AMT[1,]))) & 
                                                                 grepl('Other',as.character(unlist(AMT[1,]))))
                                                             ]!="")]))) ) }

# Clean 'Other' response in vote choice question 2014
PC_vote_choice_14 = c(NA,NA)
for(i in 3:dim(AMT)[1]){
  PC_vote_choice_14 = c(PC_vote_choice_14, 
                        
                        ifelse(length(which(AMT[i,grep("If you were eligible to vote in 2014, who did you vote for",
                                                      as.character(unlist(AMT[1,])))]!=""))==0,NA,
                               as.character(unlist(
                                 AMT[i,
                                    which(grepl('If you were eligible to vote in 2014, who did you vote for',as.character(unlist(AMT[1,]))) & 
                                            !grepl('Other',as.character(unlist(AMT[1,]))))][
                                              which(AMT[i,
                                                       which(
                                                         grepl("If you were eligible to vote in 2014, who did you vote for",as.character(unlist(AMT[1,]))) & 
                                                           !grepl('Other',as.character(unlist(AMT[1,]))))
                                                       ]!="")]))) ) }


# CONVERT `OTHER` VOTES TO CORRECT CATEGORY
PC_vote_choice_14_Other = ifelse(grepl("\\bnational people's party\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bNPP\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bPMK\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bTMC\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\baap\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv admk\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjanasena\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnda\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnever vote\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnone\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnota\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot decided yet\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot sure\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot sure at this point in time\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bprefer not to say\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_14_Other)),"United Progressive Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bundecided\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bun sure\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bagp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcannot discolose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcant say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bchoose not to disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdepends on candidate from my constituency\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdo not wish to answer\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdo not wish to specify\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi don't know yet.\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bisn't it a secret ballot?\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi will not say but it's not a constant answer. \\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi won’t say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi would vote for a candidate rather than the party\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnata\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bndy\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bno comments\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot interested to reply\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnot yet decided\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnpp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bntk\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpolitical affiliation not to be revealed\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bprakash raj\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bregional party\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bstill making up my mind after party's evaluation\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btelugu desam party\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bthis information is confidential. i need not share such data.\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bxyz\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bno disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjammu and kashmir peoples movement\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnational democratic alliance\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcannot disclose\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi would not like to say\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bshivsena\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi will vote for the best candidate, not the party\\b",tolower(PC_vote_choice_14_Other)),"NDY",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bindependent candidate\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsecret\\b",tolower(PC_vote_choice_14_Other)),"NO DISCLOSE",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bbjp\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwill not vote\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjammu and kashmir people's movement\\b",tolower(PC_vote_choice_14_Other)),"Other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bdon't want to disclose\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnaga peoples front\\b",tolower(PC_vote_choice_14_Other)),"National Democratic Alliance",tolower(PC_vote_choice_14_Other))

PC_vote_choice_14_Other = tolower(PC_vote_choice_14_Other)

PC_vote_choice_14_Other = ifelse(grepl("\\bi don't vote\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi vote for the candidate and not the party\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bjsp\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bmahagathbandhan\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))

PC_vote_choice_14_Other = ifelse(grepl("\\bi did not have my name on voter's list\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsdf\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bi got married and found out that my name was struck off the voter list. it was too late to make amends. \\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bhad election duty\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\baiadmk\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwas not in india\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bsamajwadi party\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bwas on election duty\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))

PC_vote_choice_14_Other = ifelse(grepl("\\bnaam tamilar\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpmk\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpatali makal katchi\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnaam tamilar katchi\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnaam tamizhar katchi\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\btrs party\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bpattali makkal kattchi\\b",tolower(PC_vote_choice_14_Other)),"national democratic alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bmarumalarchi dravida munnetra kazhakam\\b",tolower(PC_vote_choice_14_Other)),"united progressive alliance",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bmakkal neethi mayam\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnam tamilar katchi\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnamtamilar\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bmakkal neethi mayyam\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bcombined parties\\b",tolower(PC_vote_choice_14_Other)),NA,tolower(PC_vote_choice_14_Other))
PC_vote_choice_14_Other = ifelse(grepl("\\bnaam thamizhar\\b",tolower(PC_vote_choice_14_Other)),"other",tolower(PC_vote_choice_14_Other))

# Turn into NDA, UPA, OTHER or NA
PC_vote_choice_14 = ifelse(grepl("\\bnational democratic alliance\\b",PC_vote_choice_14_Other),"National Democratic Alliance",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bunited progressive alliance\\b",PC_vote_choice_14_Other),"United Progressive Alliance",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bother\\b",PC_vote_choice_14_Other),"Other",PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bndy\\b",PC_vote_choice_14_Other),NA,PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bno disclose\\b",PC_vote_choice_14_Other),NA,PC_vote_choice_14)
PC_vote_choice_14 = ifelse(grepl("\\bHave not decided yet\\b",PC_vote_choice_14),NA,PC_vote_choice_14)
PC_vote_choice_14[which(PC_vote_choice_14=="Other" & is.na(PC_vote_choice_14_Other))]=NA

PC_vote_choice_14 = ifelse(PC_vote_choice_14=="Did not vote",NA,
                           ifelse(PC_vote_choice_14=="Was not eligible",NA,
                                  ifelse(PC_vote_choice_14=="Can' remember / Don't want to say",NA,
                                         ifelse(PC_vote_choice_14=="Can't remember / don't want to say",NA,
                                                ifelse(PC_vote_choice_14=="Can't remember / Don't want to say",NA,PC_vote_choice_14)))))

table(PC_vote_choice_14)






# Clean turnout question
turnout = c(NA,NA)
for(i in 3:dim(AMT)[1]){
  turnout = c(turnout,
              
              ifelse(length(which(AMT[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                             as.character(unlist(AMT[1,])))]!=""))==0,NA,
                     as.character(unlist(
                       AMT[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                  as.character(unlist(AMT[1,])))][
                                    which(AMT[i,grep("This year, the General Election for the Lok Sabha is expected to be held sometime between April and May",
                                                     as.character(unlist(AMT[1,])))]!="")]))))
}  

# Clean vote choice 'other' category
PC_vote_choice_Other = c(NA,NA)
for(i in 3:dim(AMT)[1]){
  PC_vote_choice_Other = c(PC_vote_choice_Other, 
                           
                           ifelse(length(which(AMT[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                          as.character(unlist(AMT[1,])))]!=""))==0,NA,
                                  as.character(unlist(
                                    AMT[i,
                                        which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(AMT[1,]))) & 
                                                grepl('Other',as.character(unlist(AMT[1,]))))][
                                                  which(AMT[i,
                                                            which(
                                                              grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(AMT[1,]))) & 
                                                                grepl('Other',as.character(unlist(AMT[1,]))))
                                                            ]!="")]))))
}

# Clean 'vote choice' question
PC_vote_choice = c(NA,NA)
for(i in 3:dim(AMT)[1]){
  PC_vote_choice = c(PC_vote_choice, 
                     
                     ifelse(length(which(AMT[i,grep("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",
                                                    as.character(unlist(AMT[1,])))]!=""))==0,NA,
                            as.character(unlist(
                              AMT[i,
                                  which(grepl('If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote',as.character(unlist(AMT[1,]))) & 
                                          !grepl('Other',as.character(unlist(AMT[1,]))))][
                                            which(AMT[i,
                                                      which(
                                                        grepl("If Lok Sabha elections were to be held tomorrow, which of the following parties would you vote",as.character(unlist(AMT[1,]))) & 
                                                          !grepl('Other',as.character(unlist(AMT[1,]))))
                                                      ]!="")]))))
}

# CONVERT `OTHER` VOTES TO CORRECT CATEGORY
PC_vote_choice_Other = ifelse(grepl("\\bagp\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcannot discolose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcant say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bchoose not to disclose\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdepends on candidate from my constituency\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdo not wish to answer\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdo not wish to specify\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi don't know yet.\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bisn't it a secret ballot?\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi will not say but it's not a constant answer. \\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi won’t say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bi would vote for a candidate rather than the party\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnata\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bndy\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bno comments\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot interested to reply\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot yet decided\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnpp\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bntk\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpolitical affiliation not to be revealed\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bprakash raj\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bregional party\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bstill making up my mind after party's evaluation\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btelugu desam party\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bthis information is confidential. i need not share such data.\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bxyz\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi mayyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnota\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattali makkal katchi\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnam tamilar katchi\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamilar\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam thamizhar\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam thamilar\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi maiyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal nidhi mayyem\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattali makkal katchi (pmk)\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal needhi maiam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv admk\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bun sure\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamilar katchi\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munnetra kazhakam- ttv\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnew candidate\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btrinamul congress\\b",tolower(PC_vote_choice_Other)),"United Progressive Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot sure at this point in time\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnever vote\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal needhi mayam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bundecided\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot decided yet\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi mandram\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi  maiyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkalneethi maiyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal munetra maiam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bprefer not to say\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot sure\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bjanasena\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\baap\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bit depends up on the candidate\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\b\\bna\\b\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bsocial democratic party of india\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmammal needhi mayyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btdp\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bvanchit bahujan aghadi\\b",tolower(PC_vote_choice_Other)),"NA",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot decided\\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamilar party\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamilar \\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal needhi maiyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neethi maiam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bkongunadu makkal desia katchi\\b",tolower(PC_vote_choice_Other)),"United Progressive Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal nethi maiyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(grepl("\\bnone  \\( i'm done with indian politics \\)\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnone \\( i'm done with political dramas\\)\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(grepl("\\bmakkal needhi mayyam\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bit depends on many factors. \\b",tolower(PC_vote_choice_Other)),"NDY",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmnm\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bna\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamizhar katchi\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnamm tamilar katchi\\b",tolower(PC_vote_choice_Other)),"Other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattalai makkal katchi \\(pmk\\)\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bwe should not disclose this voting information. sorry.\\b",tolower(PC_vote_choice_Other)),"NO DISCLOSE",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = tolower(PC_vote_choice_Other)
PC_vote_choice_Other = ifelse(grepl("\\bnaam tamilr katchi\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\btrs party\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(tolower(PC_vote_choice_Other)==" ",NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(tolower(PC_vote_choice_Other)=="dont want to tell",NA,
                              tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(tolower(PC_vote_choice_Other)==" kerala congress",NA,
                              tolower(PC_vote_choice_Other)) #impossible to tell which faction

PC_vote_choice_Other = ifelse(grepl("\\ aam aadmi party\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(grepl("\\bamma makkal munetra kalagam\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal neeti\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal nethi maiam\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bwill not vote\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))

PC_vote_choice_Other = ifelse(grepl("\\bnaam tamizhar\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnaam thamizar\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))


PC_vote_choice_Other = ifelse(grepl("\\jjp\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnamthamilar\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnone\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bnot dicided\\b",tolower(PC_vote_choice_Other)),NA,tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpattali makkal kattchi\\b",tolower(PC_vote_choice_Other)),"National Democratic Alliance",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bpeople justice party\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bvivasayi party\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bvivashai\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))


PC_vote_choice_Other = ifelse(grepl("\\bcan't say\\b",tolower(PC_vote_choice_Other)),"no disclose",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bcombined parties\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdont want to disclose\\b",tolower(PC_vote_choice_Other)),"no disclose",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bdont want to tel\\b",tolower(PC_vote_choice_Other)),"no disclose",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bmakkal needdhi maiam\\b",tolower(PC_vote_choice_Other)),"other",tolower(PC_vote_choice_Other))
PC_vote_choice_Other = ifelse(grepl("\\bwon't disclose\\b",tolower(PC_vote_choice_Other)),"no disclose",tolower(PC_vote_choice_Other))

PC_vote_choice = ifelse(grepl("\\bnational democratic alliance\\b",PC_vote_choice_Other),"National Democratic Alliance",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bunited progressive alliance\\b",PC_vote_choice_Other),"United Progressive Alliance",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bother\\b",PC_vote_choice_Other),"Other",PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bndy\\b",PC_vote_choice_Other),NA,PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bno disclose\\b",PC_vote_choice_Other),NA,PC_vote_choice)
PC_vote_choice = ifelse(is.na(PC_vote_choice_Other) & PC_vote_choice=="Other",NA,PC_vote_choice)
PC_vote_choice = ifelse(grepl("\\bHave not decided yet\\b",PC_vote_choice),NA,PC_vote_choice)

# gender
gender = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q2"])))
# age
age_cat = as.character(unlist(
  cut(
    round(as.numeric(difftime(
      Sys.Date(),
      as.Date(apply(
        data.frame(byear = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q67"]))),
                   bmonth = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q69"]))),
                   bdate = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q199"])))),
        1,FUN = function(x){paste0(x,collapse = " ")}),"%Y %B %d"),units = "day"))/365),
    breaks = c(-1,17,24,34,44,54,64,150),
    labels = c("[0-17]","(17-24]","(24-34]","(34-44]","(44-54]","(54-64]","(64-max]"))))
# income
income_level = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q80"])))
# religion
religion = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q83"])))
# edu
edu = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q22"])))
# relevant edu levels: 
#"(00) none 0" # No formal education
#"(03) 1-4 3" # Incomplete primary school
#"(05) primary 5" # Completed primary school
#"(08) 6-9 8" # Middle pass / Matric fail
#"(10) Secondary(&11) 10" # Matric pass / 10th pass - 11th pass, not completed intermediate
##"(12) Higher sec(&13,14) 12" # 12th pass / Intermediate 
#"(15) graduate 15" # Bachelor's degree - Undergraduate, still in college
#"(16) some post-grad 16" # Postgraduate degree
edu = 
  ifelse(edu=="No formal education","(01) No Formal Edu 01",
         ifelse(edu=="Incomplete primary school","(02) Primary or Lower 02" ,
                ifelse(edu=="Completed primary school","(02) Primary or Lower 02" ,
                       ifelse(edu=="Middle pass / Matric fail","(03) Middle or Secondary 03",
                              ifelse(edu=="Matric pass / 10th pass","(03) Middle or Secondary 03",
                                     ifelse(edu=="11th pass, not completed intermediate","(03) Middle or Secondary 03",
                                            ifelse(edu=="12th pass / Intermediate"," (04) Higher Secondary 04",
                                                   ifelse(edu=="Bachelor's degree","(05) Some Graduate or Higher 05",
                                                          ifelse(edu=="Undergraduate, still in college","(05) Some Graduate or Higher 05",
                                                                 ifelse(edu=="Postgraduate degree","(05) Some Graduate or Higher 05",NA))))))))))



# Jati
jati = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q91"])))
jati_other = c(NA,NA,as.character(unlist(AMT[-c(1:2),"Q91_13_TEXT"])))

jati_other = ifelse(trimws(tolower(jati_other),which = "both") =="pubjabi","Punjabi",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shatriya","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vanniya kula kshatriyar","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="agamudiyar","Agamudiyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="andi pandaram","Andipandaram",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="badaga","Badaga",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="baishya","Baishya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="banya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="baniya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bc-b","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="bhandari","Bhandari",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chettiyear","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chowdhary(kamma)","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="csi","Dalit",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="dalith","Dalit",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devangar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devar","Dewar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="fishermen","Dewar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ezhava","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gavara","Gavara",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="goundar","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gowda","Gowda",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gowdas","Gowda",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="humanity","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindhu","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindu","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="hindu vaniya settiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="india","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="jain","Jain",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kallar","Kallar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kamma","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kammalar","Kammalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kammavar","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kapu","Kapu",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kasar","Kasar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellala gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellalar gounder","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu veller","Goundar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kshatriya","Kshatriya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="labbai","Labbai",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="latin catholic","Upper Caste Christians",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mala arayan","Dheevara",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mannar","Other",jati_other) # I think this is a location..
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mappila","Mappila",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maravar","Maravar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mbc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="most backward","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="menon","Nair",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mpc","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mudaliyar","Mudaliar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muslim","Muslim",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="nadar","Nadar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="naadar","Nadar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="naidu","Kamma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="obc","OBC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="nair","Nair",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmanayaka velama","Velama",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="patel","Patel",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pattunulkarar","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillai","Pillai",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillamar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saiva vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliar","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliya","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="saliyar","Saliya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sc","SC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="schedule caste","SC",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="scheduled tribe","ST",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shahib",NA,jati_other) #couldn't find correspondent 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shetty","Bunt",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sourashtra","Sourashtra",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sourastra","Sourashtra",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhila vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhlia vellaler","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sri karuneegar","Karuneegar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sunni","Muslim",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="tamil","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="tamilan","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="telugu chettiar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thachan","Thachan",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thiya","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thiyya","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="thuluva vellala","Thuluva Vellala",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="valaiyar","Valaiyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniga chettiyar","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniya","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniyan","Banya",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vaniyar","Vanniyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vannier kula satisreyar","Other",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vanniyar","Vanniyar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="veera shaiva","Veerasaiva",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vellalar","Vellalar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="viswakarma","Viswakarma",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vysya","Komati",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadav","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadava","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadhav","Yadhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="lohar(obc)","Lohar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="",NA,jati_other)
# second wave of "other" checks
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="adi dravidar","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="agamudaiyar","Kallar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ambalakarar","Ambalakarar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ampalathar","Ampalathar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="arora","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="backwardclass","OBC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="ballija","Kapu",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="chamar","Chamar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="dakkani","Dakkhani",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="devang","Chettiar",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="eezhava","Ezhava",jati_other)
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="gounders","Goundar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kalinga","Kalinga",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="khatri","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="khukrain","Arora",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu gounder","Goundar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="kongu vellalar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mahar","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mapla","Mappila",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maratha","Maratha",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maravr","Maravar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="maruthuvar","Maruthuvar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="marwadi",NA,jati_other) # marwari is a regional belonging not cast system
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muslim mappila","Mappila",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="mutharayar","Mutharayar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="muthuraja","Mutharayar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="obc","OBC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmanayaka velams","Kapu",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="padmasali","Chettiar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="parkavakulam","Parkavakulam",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pbc","OBC",jati_other) # pbc means poor backeards caste
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="pillaymar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="reddy","Reddy",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="rowther","Rowther",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sc (adi dravida)","Dalit",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="shree karuneegar","Karuneegar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="siva pillai","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhilavellalor","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sozhia vellalar","Vellalar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="sonar","Sunar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="upper caste","OFC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="upper caste christians","OFC",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vadugar","Vadugar",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="varier","Nair",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwabrahmin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwa brahmin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwabramin","Brahmin",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwakarma","Viswakarma",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="vishwakarma","Viswakarma",jati_other) 
jati_other = ifelse(trimws(tolower(jati_other),which = "both")  =="yadavs","Yadhava",jati_other) 

jati_other[which(trimws(tolower(jati_other),which = "both")  =="agamudiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="andipandaram")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="badaga")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="baishya")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="banya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhandari")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bunt")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chettiar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dalit")]="(4) Scheduled Castes (SC) 4"#"Dalit"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dewar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dheevara")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ezhava")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gavara")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="goundar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gowda")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jain")]=NA#"Jain" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kallar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kamma")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kammalar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kapu")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karuneegar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kasar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="komati")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshatriya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="labbai")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mappila")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maravar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mudaliar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muslim")]=NA#"Muslim"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nadar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nair")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="other")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="patel")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pillai")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="saliya")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sc")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sourashtra")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thachan")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thuluva vellala")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="upper caste christians")]="Upper Caste Christians"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="valaiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vanniyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="veerasaiva")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="velama")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vellalar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="viswakarma")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="yadhava")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lohar")]="(3) Other Backward Castes (OBC) 3"
# second wave of checks 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="obc")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ofc")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="st")]="(5) Scheduled Tribes (ST) 5"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="reddy")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="arora")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="brahmin")]="(1) Brahmin 1"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maratha")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="parkavakulam")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalinga")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vadugar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="arunthathiyar")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rowther")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mutharayar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ambalakarar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dakkhani")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ampalathar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maruthuvar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chamar")]="(4) Scheduled Castes (SC) 4"
# wave three of edits
jati_other[which(trimws(tolower(jati_other),which = "both")  =="agarwal")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Does-the-Agarwal-community-come-under-the-OBC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ahir")]="(3) Other Backward Castes (OBC) 3" # https://timesofindia.indiatimes.com/city/nagpur/More-castes-included-in-backward-list/articleshow/29787773.cms
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ahl-e-quraish")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="assamese")]=NA # region not caste 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="baishnab")]="(3) Other Backward Castes (OBC) 3" #https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
jati_other[which(trimws(tolower(jati_other),which = "both")  =="balija")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bania")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="besthar")]="(5) Scheduled Tribes (ST) 5" # have asked to be part of ST https://en.wikipedia.org/wiki/Bestha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhat")]="(3) Other Backward Castes (OBC) 3" #http://www.rtifoundationofindia.com/appellant-why-caste-%E2%80%9Cbhat%E2%80%9D-was-included-obcs-while#.XIvhTx_njRY
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhumihar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="businessmen teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chatriya")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dawoodi bhora")]="(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ghanchi")]="(3) Other Backward Castes (OBC) 3" #https://en.wikipedia.org/wiki/Ghanchi
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gouda")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gujjar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Gurjar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="gupta")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not
jati_other[which(trimws(tolower(jati_other),which = "both")  =="harijans (merchants/traders of frowned products like alcohol)")]="(4) Scheduled Castes (SC) 4" # https://www.britannica.com/topic/untouchable
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu mangela")]="(3) Other Backward Castes (OBC) 3" # http://www.ymnonline.com/data/stureg/caste.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="islam")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jatav")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Jatav
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kahar")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kahar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kaisth")]="(2) Forward/General (except Brahmin) 2" #https://en.wikipedia.org/wiki/Kayastha
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalita")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kalita_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karmas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kashmiri pandit")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kayasth")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kayastha")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khan")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khandayat")]="(3) Other Backward Castes (OBC) 3" # https://www.telegraphindia.com/states/jharkhand/demand-to-be-on-obc-list/cid/717275
jati_other[which(trimws(tolower(jati_other),which = "both")  =="koeri")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/What-is-the-difference-between-Kurmi-Koeri-and-Kushwaha-castes-of-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kutchi visa oswal")]="(6) Others 6" # these are jain 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lingayath")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
jati_other[which(trimws(tolower(jati_other),which = "both")  =="malwadi")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="marwari")]= NA # this is a region https://www.quora.com/Are-marwaris-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maurya")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Which-caste-does-the-surname-Maurya-belong-to
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mudhaliyar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="naga")]="(5) Scheduled Tribes (ST) 5" # https://www.quora.com/What-is-the-exact-difference-between-various-reserved-categories-like-scheduled-caste-schedule-tribe-and-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="none")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="not known")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="not sure")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="others")]="(6) Others 6"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="paswan")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Paswan
jati_other[which(trimws(tolower(jati_other),which = "both")  =="phatan")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-all-Muslims-in-India-fall-under-OBC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="patidar")]="(4) Scheduled Castes (SC) 4"  # https://en.wikipedia.org/wiki/Patidar_reservation_agitation
jati_other[which(trimws(tolower(jati_other),which = "both")  =="phatans")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pathans")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="punjabi")]="(6) Others 6"  # regional denomination
jati_other[which(trimws(tolower(jati_other),which = "both")  =="punjabi khatri")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sahu")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sahu
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sanamahi")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-manipur/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sharma")]="(1) Brahmin 1" # https://www.quora.com/What-category-does-Anurag-Sharma-belong-to-general-or-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sud")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunri")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Sundhi
jati_other[which(trimws(tolower(jati_other),which = "both")  =="syed shia")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Syed-Muslims-in-India-belong-in-the-general-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="teli")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Is-Teli-an-OBC-caste-in-Bihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="unknown")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vatishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Why-are-Bairagi-Vaishnav-and-Swami-considered-OBC-even-though-they-are-upper-caste-Brahmins
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vokkaliga")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/Does-the-3A-Vokkaliga-community-of-Karnataka-come-under-OBC-for-central-government-reservations
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vyshyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vysyas")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya
jati_other[which(trimws(tolower(jati_other),which = "both")  =="yadavas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishnav")]="(2) Forward/General (except Brahmin) 2" # https://www.encyclopedia.com/social-sciences-and-law/sociology-and-social-reform/sociology-general-terms-and-concepts/vaisya

jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshyatriya")]="(2) Forward/General (except Brahmin) 2" # 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vannya")]="(3) Other Backward Castes (OBC) 3" #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ramgarhia")]="(3) Other Backward Castes (OBC) 3" #

jati_other[which(trimws(tolower(jati_other),which = "both")  =="atheist from brahmin family")]="(1) Brahmin 1" #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="don't know")]=NA #
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalinga vysya")]="(3) Other Backward Castes (OBC) 3" # https://www.thehindu.com/news/national/andhra-pradesh/obc-status-for-kalinga-vysyas-sistakaranams/article7537421.ece
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lingayat")]="(3) Other Backward Castes (OBC) 3" # https://www.jagranjosh.com/current-affairs/maharashtra-government-included-10-sub-caste-of-lingayat-community-in-obc-category-1409305003-1
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lohana")]="(3) Other Backward Castes (OBC) 3" # http://entrance-exam.net/forum/general-discussion/does-lohana-caste-gujarati-come-under-obc-category-state-maharashtra-257223.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mistri")]= "(3) Other Backward Castes (OBC) 3"# https://en.wikipedia.org/wiki/Mistri_caste
jati_other[which(trimws(tolower(jati_other),which = "both")  =="momin ansar")]= "(3) Other Backward Castes (OBC) 3" # http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
jati_other[which(trimws(tolower(jati_other),which = "both")  =="shia's")]=NA


jati_other[which(trimws(tolower(jati_other),which = "both")  =="bhumihaar")]="(1) Brahmin 1" # https://en.wikipedia.org/wiki/Bhumihar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="forward / general")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hnidu")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshatriyas")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="naicker")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nayakar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="parayan")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Does-the-parayan-caste-come-under-SC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rawuthar")]="(3) Other Backward Castes (OBC) 3" # http://www.ncbc.nic.in/Writereaddata/1123.PDF
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sozhiya vellalar")]= "(3) Other Backward Castes (OBC) 3"# http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="upper caste hindu")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishnava")]="(2) Forward/General (except Brahmin) 2"


jati_other[which(trimws(tolower(jati_other),which = "both")  =="arya vyshya")]="(4) Scheduled Castes (SC) 4" # https://www.quora.com/Under-which-category-does-Arya-Vysya-come
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bori")]="(3) Other Backward Castes (OBC) 3" # http://www.firstfoundation.in/Soc/List-OBC-Mah.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="harijans (businessman/trader of immoral goods e.g. alcohol)")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishya")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Did-Gupta-Vaish-come-in-the-OBC-category-or-not

jati_other[which(trimws(tolower(jati_other),which = "both")  =="buddihist")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bunts")]="(3) Other Backward Castes (OBC) 3" # https://www.quora.com/How-do-Bunts-belonged-to-OBC-or-general
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chakkiliar")]="(4) Scheduled Castes (SC) 4" #  https://en.wikipedia.org/wiki/Arunthathiyar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chittiyar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="devanga")]="(3) Other Backward Castes (OBC) 3" # these are chettiar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu-nadar")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="karnam")]="(2) Forward/General (except Brahmin) 2" # http://pkpi4u.blogspot.com/2014/07/karanam-or-karana-is-caste-mostly.html
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kodava")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Kodava_people
jati_other[which(trimws(tolower(jati_other),which = "both")  =="manipuri")]=NA # this is a region
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muthaliyar")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nair (upper caste)")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pattani")]="(3) Other Backward Castes (OBC) 3" # https://wikivisually.com/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="saini")]="(3) Other Backward Castes (OBC) 3" # https://punjabxp.com/list-backward-other-classes/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaishyas")]="(2) Forward/General (except Brahmin) 2" 

jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward class")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu - nair")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kaikolar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="padmanayaka velamas")]="(2) Forward/General (except Brahmin) 2"  # https://en.wikipedia.org/wiki/List_of_Telugu_castes
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thiyya(obc)")]="(3) Other Backward Castes (OBC) 3" 


jati_other[which(trimws(tolower(jati_other),which = "both")  =="bouddh")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ganaka")]="(3) Other Backward Castes (OBC) 3" # https://sanjaykumarnishad.wordpress.com/2016/11/02/central-list-of-other-backward-castes-obcs-kerala/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu salian")]="(3) Other Backward Castes (OBC) 3"  # https://sanjaykumarnishad.wordpress.com/category/obc/central-list-of-other-backward-castes-obcs/page/2/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="maheshwari")]= "(2) Forward/General (except Brahmin) 2" #https://www.jeevansathi.com/rajasthani-maheshwari-matrimony-matrimonials
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pramalai kallar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="velar")]="(3) Other Backward Castes (OBC) 3" # https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes


jati_other[which(trimws(tolower(jati_other),which = "both")  =="chattiyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chettyiar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nagker")]=NA#nagar? 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nayar")]="(2) Forward/General (except Brahmin) 2" #https://www.quora.com/Why-is-Nair-not-in-OBC
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thevar")]="(3) Other Backward Castes (OBC) 3" # https://www.thenewsminute.com/article/thevar-factor-who-real-aiadmk-dominant-obc-community-70804

jati_other[which(trimws(tolower(jati_other),which = "both")  =="bc nadar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bc - vadukar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="boyar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chettyar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu kaikolar muthaliyar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="labbi")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="manidha jathi")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nambiar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pillaimar")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="royals")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="valayar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="yadhavar")]="(3) Other Backward Castes (OBC) 3"


jati_other[which(trimws(tolower(jati_other),which = "both")  =="boyas")]="(3) Other Backward Castes (OBC) 3"# http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="khamma")]="(2) Forward/General (except Brahmin) 2" # https://en.wikipedia.org/wiki/Kamma_(caste)
jati_other[which(trimws(tolower(jati_other),which = "both")  =="koli")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Koli_people
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kumhar")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Kumhar
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kunbi")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="leppa")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muslim (shaik)")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="segathur")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sri vaishnava")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thuluva")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vanniar")]="(3) Other Backward Castes (OBC) 3"


jati_other[which(trimws(tolower(jati_other),which = "both")  =="backerd class")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="christian")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu sourashtra")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jangam")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Do-Jangam-come-under-the-SC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kongu vellaler")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kshtriya")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kuruma")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="lebbai")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mannadiar")]="(2) Forward/General (except Brahmin) 2"  # nair
jati_other[which(trimws(tolower(jati_other),which = "both")  =="meitei")]="(2) Forward/General (except Brahmin) 2" # http://www.thesangaiexpress.com/32497-how-did-the-meiteis-bag-the-general-category/
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muslims")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="paraiyan")]="(4) Scheduled Castes (SC) 4" # https://www.keralapsc.gov.in/index.php?option=com_content&id=338&Itemid=198
jati_other[which(trimws(tolower(jati_other),which = "both")  =="phathan")]="(3) Other Backward Castes (OBC) 3"  # https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rajak")]="(4) Scheduled Castes (SC) 4" # https://en.wikipedia.org/wiki/Rajaka
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sourshtra")]="(3) Other Backward Castes (OBC) 3" #http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="uppara")]="(3) Other Backward Castes (OBC) 3"  # https://en.wikipedia.org/wiki/Uppara
jati_other[which(trimws(tolower(jati_other),which = "both")  =="uppiliya")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="valayaar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vanniya")]="(3) Other Backward Castes (OBC) 3"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="mukkuvar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/bclist.htm
jati_other[which(trimws(tolower(jati_other),which = "both")  =="nadars")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="parayar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="piramalai kallar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sanni")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vannier")]="(3) Other Backward Castes (OBC) 3"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="chetti")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="thakur")]="(3) Other Backward Castes (OBC) 3" #https://en.wikipedia.org/wiki/Nai_(Nai_caste)

jati_other[which(trimws(tolower(jati_other),which = "both")  =="baiswya")]="(3) Other Backward Castes (OBC) 3" # https://en.wikipedia.org/wiki/Baishya_Kapali
jati_other[which(trimws(tolower(jati_other),which = "both")  =="broad cast")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dhalith")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu maravar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu maruthuvar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kongu")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muddaliar")]="(3) Other Backward Castes (OBC) 3"#https://www.thehansindia.com/posts/index/Andhra-Pradesh/2015-12-16/Include-Vaddera-Mudaliar-castes-in-OBCs-BC-leaders/193536
jati_other[which(trimws(tolower(jati_other),which = "both")  =="open")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="padmanayaka vlamas")]="(3) Other Backward Castes (OBC) 3" # valamas same as kammu
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sunna")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vadugan")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vanian chettiar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vellalakavundar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vishvabhramin")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="agamudayar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="devangas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="middle caste christianity")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="middle class christians")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vaaniya chettiyar")]="(3) Other Backward Castes (OBC) 3"


jati_other[which(trimws(tolower(jati_other),which = "both")  =="adi dravitar")]="(4) Scheduled Castes (SC) 4" # http://www.socialjustice.nic.in/writereaddata/UploadFile/SC%20ST%20LISTS%20(MODIFICATION)%20ORDER%201956.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ambalar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ausala")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward cast")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bc_yadhav")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chamer")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="cheramar")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="chetty")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dnc")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="hindu kallar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="jaat")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kongu vellallar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mdc")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="middle caste christians")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="mudiraj")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="muslim labbai")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ndr")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="padaiachi")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="porwal")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sedeule caste")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sengunthar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="upper")]="(2) Forward/General (except Brahmin) 2"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="variar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="verrakodi vellala")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vishwabhramin")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="aggarwal")]="(2) Forward/General (except Brahmin) 2" # https://www.quora.com/Does-the-Agarwal-community-come-under-the-OBC-category
jati_other[which(trimws(tolower(jati_other),which = "both")  =="ahamudaiyar")]="(3) Other Backward Castes (OBC) 3" # http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward caste")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="backward community")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="banik")]="(2) Forward/General (except Brahmin) 2" # http://en.banglapedia.org/index.php?title=Banik
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bc-yadhava")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="dakkini")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="general")]="(2) Forward/General (except Brahmin) 2" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kalar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="kurmi")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="madiga")]="(4) Scheduled Castes (SC) 4"# https://en.wikipedia.org/wiki/Madiga
jati_other[which(trimws(tolower(jati_other),which = "both")  =="naikkar")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="rawdar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="saiva vellalar pillai")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="sengunther")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="servaru")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="subarna banik")]="(2) Forward/General (except Brahmin) 2"

jati_other[which(trimws(tolower(jati_other),which = "both")  =="arya vysya")]="(4) Scheduled Castes (SC) 4"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="bc yalamas")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="konar")]="(4) Scheduled Castes (SC) 4" # http://adfdell.pstc.brown.edu/arisreds_data/annex-5.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="marath")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="pattunulkarrar")]=NA
jati_other[which(trimws(tolower(jati_other),which = "both")  =="udia")]="(3) Other Backward Castes (OBC) 3" # http://www.stscodisha.gov.in/Pdf/SEBC_List_Orissa.pdf
jati_other[which(trimws(tolower(jati_other),which = "both")  =="veerakodi vellala")]="(3) Other Backward Castes (OBC) 3"
jati_other[which(trimws(tolower(jati_other),which = "both")  =="vishvabrahmin")]="(2) Forward/General (except Brahmin) 2" 

jati_other[which(trimws(tolower(jati_other),which = "both")  =="ezahva")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="indian muslim")]=NA 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="obc mappila")]="(3) Other Backward Castes (OBC) 3" 
jati_other[which(trimws(tolower(jati_other),which = "both")  =="reddiar")]="(3) Other Backward Castes (OBC) 3"  # https://en.wikipedia.org/wiki/Reddiar


sort(trimws(unique(tolower(jati_other)),which = "both") )

# varier are potentially members of nair - https://en.wikipedia.org/wiki/Talk%3ANair_subcastes
# vadugar is obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Sunar is obc - https://www.quora.com/Do-the-%E2%80%9CPitale%E2%80%9D-and-%E2%80%9CSonar%E2%80%9D-surnames-fall-under-the-OBC-category
# Karnueegar are obc - https://en.wikipedia.org/wiki/Karuneegar
# rowther are shipping mechant obc - https://groups.google.com/forum/#!topic/soc.culture.indian/ezJIiSdvQB0
# reddy is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# parkavakulam are obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# padmasali are devanga (chettiar) - https://en.wikipedia.org/wiki/Padmashali
# velama are the same as Kapu - https://en.wikipedia.org/wiki/Kapu_(caste)
#mutharayar is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# "maruthuvar is OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# mahar is scheduled chaste
# maratha are obc for all intents and purposes - https://en.wikipedia.org/wiki/Maratha
# khukrain are like khatri 
# khatri are like arora 
# kalinga are obc https://www.thehindu.com/news/national/andhra-pradesh/obc-status-for-kalinga-vysyas-sistakaranams/article7537421.ece
# Dakkhani is obc - http://pib.nic.in/newsite/PrintRelease.aspx?relid=76106
# Chamar is Dalit (scheduled caste)
# ballija same as Kapu -https://www.cs.odu.edu/~salam/wsdl/inforet/wikihtml/Balija.html
# Arunthathiyar is dalit - https://en.wikipedia.org/wiki/Arunthathiyar
# Arora is a forward caste - https://www.answers.com/Q/Does_arora_belongs_to_obc_category
# Sindhis we can classify as OBC - https://timesofindia.indiatimes.com/city/nagpur/Somes-Sindhis-demand-OBC-status/articleshow/36601089.cms
# Jat can be connsidered OBC - https://en.wikipedia.org/wiki/Jat_reservation_agitation
# Jogi can be considered OBC - https://en.wikipedia.org/wiki/Jogi
# Chamars are Dalit - https://en.wikipedia.org/wiki/Chamar
# Dhanak are scheduled casts - http://haryanascbc.gov.in/list-of-scheduled-castes and other states 
# Kori is a scheduled caste - https://en.wikipedia.org/wiki/Kori_caste
# Ashraf are upper class muslims - https://en.wikipedia.org/wiki/Caste_system_in_India
# Rajputs are a mix - upper caste in a few states in the north - assume FC - https://www.quora.com/Is-Rajput-come-in-forward-caste-or-OBC
# Kayashta are forward class -https://en.wikipedia.org/wiki/Kayastha
# Jath Sikh are sikhs
# Upper caste christians are christians 
# Kshatriya are other forward class
# Banya are other forward class - https://www.quora.com/Who-is-in-the-Teli-or-Baniya-castes 
# khatri are sikh banya - other forward class 
# Agamudiyar is OBC - https://en.wikipedia.org/wiki/Agamudayar
# andi pandaram - OBC
# badaga is OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# baishya is OBC - https://en.wikipedia.org/wiki/Baishya_Kapali
# bhandari is OBC - https://en.wikipedia.org/wiki/Bhandari_caste
# Chettiar is OBC - https://en.wikipedia.org/wiki/Chettiar
# Kamma are Forward - https://en.wikipedia.org/wiki/Kamma_(caste)
# CSI are largely dalits  https://www.bbc.co.uk/news/world-south-asia-11229170
# Devar are scheduled cast - no SC available set to OBC - fishermen - https://en.wikipedia.org/wiki/Dewar_(caste)
# ezhava are obc-  https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes
# gavara are obc - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Goundar are obc - https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf
# gowda are obc - https://economictimes.indiatimes.com/news/politics-and-nation/i-was-indias-first-obc-pm-not-narendra-modi-h-d-deve-gowda/articleshow/48047326.cms
# Kallar OBC- https://www.quora.com/Is-kallar-caste-is-a-OBC-category
# Kammalar OBC- http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# Kapu DMC in some places but OBC mostly - https://en.wikipedia.org/wiki/Kapu_(caste)
# Kasar are OBC - https://obccastegovtschemes.wordpress.com/2016/10/12/other-backward-classes-caste-list-obc-caste-in-maharashtra/
# labbai are OBC - https://en.wikipedia.org/wiki/List_of_Muslim_Other_Backward_Classes_communities_in_India
# Dheevara are OBC - https://en.wikipedia.org/wiki/Dheevara_(caste)
# Mappila are Muslim OBC 
# Maravar are OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# mbc is most backward caste - categorize as OBC 
# Nair is a forward class - https://www.quora.com/Why-is-Nair-not-in-OBC
# Mudaliar is a forward class - https://en.wikipedia.org/wiki/Mudaliar
# Nadar is a backward class - https://uk.answers.yahoo.com/question/index?qid=20121216193425AA9oL1q&guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAMvTFJWJEUspJhEEk9mEr0oM68L0lx1XPssFcVf4AThmGvlRg9kBm4TrrPUCiN1mdw3rbEzW2Xz5nAbABeAM30tTPf44OmXuOJMQwnIn4RbkfiviXWlPJoZEDOpM9axwelr8gc7J7--Ln1eWYPcUJNYRbBAyh_cdaZvYXGA60-ps
# Velama are forward class - https://www.vepachedu.org/castemore.htm
# Patel are SC/STand OBC - http://time.com/4011001/hardik-patel-protest-arrest-gujarat-obc/
# Pillai are forward caste - https://www.quora.com/Which-caste-category-is-Pillai-under
# Vellalar are OBC  - https://en.wikipedia.org/wiki/Vellalar
# Saliya are OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Bunt are OBC - http://mangaloretopnews.blogspot.com/2011/06/bunt-communitys-obc-status-in-central.html
# sourashtra OBC- http://www.bcmbcmw.tn.gov.in/bclist.htm
# Karuneegar OBC- http://www.bcmbcmw.tn.gov.in/bclist.htm
# Thachan OBC - https://www.lopol.org/article/list-of-kerala-obc-other-backward-classes
# thuluva vellala OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# valaiyar are OBC - https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf
# vanniyar are OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# veera shaiva are OBC - http://www.bcmbcmw.tn.gov.in/obc/faq/tamilnadu.pdf
# viswakarma OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Komati OBC - https://www.quora.com/Under-which-category-does-Arya-Vysya-come
# Yadhava OBC - http://www.bcmbcmw.tn.gov.in/bclist.htm
# Lohar are OBC (stated by people answering survey LOHAR(OBC))
# https://en.wikipedia.org/wiki/Adi_Dravida are Dalit
# Kallar and Agamudaiyar are part of the same caste
# Ambalakarar are OBC https://www.natboard.edu.in/pdoof/pbnotice2018/cns/Caste%20List%20of%20MBC%20and%20DC%20of%20Tamilnadu%20State.pdf

# now bring the standard casts into the fold (the targets)
jati[which(jati=="Brahmin")] = "(1) Brahmin 1"
jati[which(jati=="Sindhi")] = "(2) Forward/General (except Brahmin) 2"
jati[which(jati=="Jat")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Jogi (Mendicants and seek Alms)")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Chamar")] = "(4) Scheduled Castes (SC) 4"
jati[which(jati=="Dhanak")] = "(5) Scheduled Tribes (ST) 5"
jati[which(jati=="Kori")] = "(5) Scheduled Tribes (ST) 5"
jati[which(jati=="Ashrafs (Sayyad Shaikh)")] = "(2) Forward/General (except Brahmin) 2" # upper cast muslim
jati[which(jati=="Rajput (Peasant proprietors)")] = "(2) Forward/General (except Brahmin) 2"
jati[which(jati=="Peasants/Traders Kayastha")] = "(3) Other Backward Castes (OBC) 3"
jati[which(jati=="Jath Sikh")] = "(2) Forward/General (except Brahmin) 2" # upper sikhs
jati[which(jati=="Upper Caste Christians")] = "(2) Forward/General (except Brahmin) 2"# upper christians
jati[which(jati=="Does not apply")] = "(6) Others 6"
jati[which(jati=="")] = NA
jati = ifelse(jati!="Other",jati,jati_other)

# Mechanical Turk Survey
AMTurk_Survey_temp = 
  data.frame(
    survey_date = as.character(unlist(survey_date)),
    survey_state = as.character(unlist(survey_state)),
    ID_GBIP =as.character(unlist(c(NA,NA,as.character(unlist(AMT$IPAddress))[-c(1:2)]))),
    gender = as.character(unlist(gender)), 
    age_cat = as.character(unlist(age_cat)),
    income_level = as.character(unlist(income_level)),
    religion = as.character(unlist(religion)),
    education_level= as.character(unlist(edu)), 
    jati = as.character(unlist(jati)),
    turnout = as.character(unlist(turnout)),
    turnout_14 = as.character(unlist(turnout_14)),
    PC_vote_choice = as.character(unlist(PC_vote_choice)),
    PC_vote_choice_14 = as.character(unlist(PC_vote_choice_14)),
    Source = as.character(unlist(rep("AMechTurk",length(PC_vote_choice))))
  )[-c(1:2),]

write.csv(AMTurk_Survey_temp,"Generated Quantities/AMTurk_Survey.csv",row.names = FALSE)

# Upload surveys
SPurk_Survey = read.csv("Generated Quantities/SPurk_Survey.csv",na.strings = c("",NA))
AMTurk_Survey_temp = read.csv("Generated Quantities/AMTurk_Survey.csv",na.strings = c("",NA))

# upload targets for helping with names and category consolidation 
IHDS_clean = read.csv("Generated Quantities/IHDS_clean.csv",na.strings = c("",NA))
#stack up surveys 
AMTurk_Survey  = rbind(SPurk_Survey,AMTurk_Survey_temp)

# clean surveys to be ready to use multiple imputation 
# consolidate levels to match targets 
AMTurk_Survey_temp = AMTurk_Survey
#
AMTurk_Survey_temp$gender = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$gender))=="",NA,
                                                                 as.character(unlist(AMTurk_Survey_temp$gender))))))
AMTurk_Survey_temp$gender = as.factor(ifelse(as.character(unlist(AMTurk_Survey_temp$gender))=="Other",NA,as.character(unlist(AMTurk_Survey_temp$gender))))
levels(AMTurk_Survey_temp$gender ) = levels(as.factor(IHDS_clean$gender))
AMTurk_Survey_temp$gender = as.factor(as.character(unlist(AMTurk_Survey_temp$gender)))
#
AMTurk_Survey_temp$age_cat = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$age_cat))=="",NA,
                                                                  as.character(unlist(AMTurk_Survey_temp$age_cat))))))
# remove under age people
AMTurk_Survey_temp = AMTurk_Survey_temp[-which(AMTurk_Survey_temp$age_cat=="[0-17]"),]
AMTurk_Survey_temp$age_cat = as.factor(as.character(unlist(AMTurk_Survey_temp$age_cat)))
levels(AMTurk_Survey_temp$age_cat ) = levels(as.factor(IHDS_clean$age_cat))
#
AMTurk_Survey_temp$income_level = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$income_level))=="",NA,
                                                                       as.character(unlist(AMTurk_Survey_temp$income_level))))))


levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="Less than \u20b960,000")] = "(01) Less than \u20b960,000 01"
levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="\u20b960,000 - \u20b989,999")] = "(02) \u20b960,000 - \u20b989,999 02"
levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="\u20b990,000 - \u20b91,19,999")] = "(03) \u20b990,000 - \u20b91,19,999 03"
levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="\u20b91,20,000 - \u20b92,39,999")] = "(04) \u20b91,20,000 - \u20b92,39,999 04"
levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="\u20b92,40,000 - \u20b95,99,999")] = "(05) \u20b92,40,000 - \u20b95,99,999 05"
levels(AMTurk_Survey_temp$income_level)[which(levels(AMTurk_Survey_temp$income_level)=="\u20b96,00,000 and over")] = "(06) \u20b96,00,000 and over 06"

AMTurk_Survey_temp$income_level = as.factor(as.character(unlist(AMTurk_Survey_temp$income_level)))
#
AMTurk_Survey_temp$religion = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$religion))=="",NA,
                                                                   as.character(unlist(AMTurk_Survey_temp$religion))))))

levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Buddhism")] = "(5) Buddhist 5"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Christianity")] = "(3) Christian 3"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Hinduism")] = "(1) Hindu 1"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Islam")] = "(2) Muslim 2"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Jainism")] = "(6) Jain 6"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Sikhism")] = "(4) Sikh 4"
levels(AMTurk_Survey_temp$religion)[which(levels(AMTurk_Survey_temp$religion)=="Other")] = "(7) Other 7"

AMTurk_Survey_temp$religion = as.factor(as.character(unlist(AMTurk_Survey_temp$religion)))
#
AMTurk_Survey_temp$education_level = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$education_level))=="",NA,
                                                              as.character(unlist(AMTurk_Survey_temp$education_level))))))
AMTurk_Survey_temp$education_level = as.factor(as.character(unlist(AMTurk_Survey_temp$education_level)))
#
AMTurk_Survey_temp$jati = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$jati))=="",NA,
                                                               as.character(unlist(AMTurk_Survey_temp$jati))))))
#
AMTurk_Survey_temp$survey_state = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$survey_state))=="",NA,
                                                                       as.character(unlist(AMTurk_Survey_temp$survey_state))))))
AMTurk_Survey_temp$survey_state = as.character(unlist(AMTurk_Survey_temp$survey_state ))
AMTurk_Survey_temp$survey_state  =  
         ifelse(AMTurk_Survey_temp$survey_state=="Andra Pradesh","(28) Andhra Pradesh 28",
                ifelse(AMTurk_Survey_temp$survey_state=="Mizoram","(15) Mizoram 15",
                       ifelse(AMTurk_Survey_temp$survey_state=="Andaman and Nicobar Islands","(35) Anadman/Nicobar 35",
                              ifelse(AMTurk_Survey_temp$survey_state=="Telangana","(28) Andhra Pradesh 28",
                                     ifelse(AMTurk_Survey_temp$survey_state=="Arunachal Pradesh","(12) Arunachal Pradesh 12",
                                            ifelse(AMTurk_Survey_temp$survey_state=="Assam","(18) Assam 18",
                                                   ifelse(AMTurk_Survey_temp$survey_state=="Bihar","(10) Bihar 10",
                                                          ifelse(AMTurk_Survey_temp$survey_state=="Chandigarh","(04) Chandigarh 04",
                                                                 ifelse(AMTurk_Survey_temp$survey_state=="Chhattisgarh","(22) Chhattisgarh 22",
                                                                        ifelse(AMTurk_Survey_temp$survey_state=="Delhi","(07) Delhi 07",
                                                                               ifelse(AMTurk_Survey_temp$survey_state=="Goa","(30) Goa 30",
                                                                                      ifelse(AMTurk_Survey_temp$survey_state=="Gujarat","(24) Gujarat 24",
                                                                                             ifelse(AMTurk_Survey_temp$survey_state=="Haryana","(06) Haryana 06",
                                                                                                    ifelse(AMTurk_Survey_temp$survey_state=="Himachal Pradesh","(02) Himachal Pradesh 02",
                                                                                                           ifelse(AMTurk_Survey_temp$survey_state=="Jammu and Kashmir","(01) Jammu & Kashmir 01",
                                                                                                                  ifelse(AMTurk_Survey_temp$survey_state=="Jharkhand","(20) Jharkhand 20",
                                                                                                                         ifelse(AMTurk_Survey_temp$survey_state=="Karnataka","(29) Karnataka 29",
                                                                                                                                ifelse(AMTurk_Survey_temp$survey_state=="Kerala","(32) Kerala 32",
                                                                                                                                       ifelse(AMTurk_Survey_temp$survey_state=="Madya Pradesh","(23) Madhya Pradesh 23",
                                                                                                                                              ifelse(AMTurk_Survey_temp$survey_state=="Maharashtra","(27) Maharashtra 27",
                                                                                                                                                     ifelse(AMTurk_Survey_temp$survey_state=="Manipur","(14) Manipur 14",
                                                                                                                                                            ifelse(AMTurk_Survey_temp$survey_state=="Meghalaya","(17) Meghalaya 17",
                                                                                                                                                                   ifelse(AMTurk_Survey_temp$survey_state=="Nagaland","(13) Nagaland 13",
                                                                                                                                                                          ifelse(AMTurk_Survey_temp$survey_state=="Orissa","(21) Orissa 21",
                                                                                                                                                                                 ifelse(AMTurk_Survey_temp$survey_state=="Pondicherry","(34) Pondicherry 34",
                                                                                                                                                                                        ifelse(AMTurk_Survey_temp$survey_state=="Punjab","(03) Punjab 03",
                                                                                                                                                                                               ifelse(AMTurk_Survey_temp$survey_state=="Rajasthan","(08) Rajasthan 08",
                                                                                                                                                                                                      ifelse(AMTurk_Survey_temp$survey_state=="Sikkim","(11) Sikkim 11",
                                                                                                                                                                                                             ifelse(AMTurk_Survey_temp$survey_state=="Tamil Nadu","(33) Tamil Nadu 33",
                                                                                                                                                                                                                    ifelse(AMTurk_Survey_temp$survey_state=="Tripura","(16) Tripura 16",
                                                                                                                                                                                                                           ifelse(AMTurk_Survey_temp$survey_state=="Uttar Pradesh","(09) Uttar Pradesh 09",
                                                                                                                                                                                                                                  ifelse(AMTurk_Survey_temp$survey_state=="West Bengal","(19) West Bengal 19",
                                                                                                                                                                                                                                         ifelse(AMTurk_Survey_temp$survey_state=="Daman and Diu","(25) Daman & Diu 25",
                                                                                                                                                                                                                                                ifelse(AMTurk_Survey_temp$survey_state=="Uttarakhand","(05) Uttarakhand 05",AMTurk_Survey_temp$survey_state
                                                                                                                                                                                                                                                ))))))))))))))))))))))))))))))))))
AMTurk_Survey_temp$survey_state = as.factor(as.character(unlist(AMTurk_Survey_temp$survey_state)))
# 
AMTurk_Survey_temp$turnout = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$turnout))=="",NA,
                                                                  as.character(unlist(AMTurk_Survey_temp$turnout))))))
AMTurk_Survey_temp$turnout = as.character(unlist(AMTurk_Survey_temp$turnout ))
AMTurk_Survey_temp$turnout  = 
    ifelse(AMTurk_Survey_temp$turnout=="Yes",1,
           ifelse(AMTurk_Survey_temp$turnout=="Not sure at this point in time",NA,
                  ifelse(AMTurk_Survey_temp$turnout=="Not heard of upcoming Lok Sabha election",0,
                         ifelse(AMTurk_Survey_temp$turnout=="No",0,NA
                         ))))

AMTurk_Survey_temp$turnout_14 = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$turnout_14))=="",NA,
                                                                  as.character(unlist(AMTurk_Survey_temp$turnout_14))))))
AMTurk_Survey_temp$turnout_14 = as.character(unlist(AMTurk_Survey_temp$turnout_14 ))
AMTurk_Survey_temp$turnout_14  = 
  ifelse(AMTurk_Survey_temp$turnout_14=="Yes",1,
         ifelse(AMTurk_Survey_temp$turnout_14=="Not sure at this point in time",NA,
                ifelse(AMTurk_Survey_temp$turnout_14=="Not heard of upcoming Lok Sabha election",0,
                       ifelse(AMTurk_Survey_temp$turnout_14=="No",0,NA
                       ))))
# to avoid people from the wrong state being assigned a wrong party - state can be useful for other characteristics - change to alliances 
# also there are some parties that we only observe once etc. - need to redc=uce number of categories.
# also we used alliances to clean up the 'other' category, so we should be consistent
AMTurk_Survey_temp$PC_vote_choice = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$PC_vote_choice))=="",NA,
                                                                         as.character(unlist(AMTurk_Survey_temp$PC_vote_choice))))))

# the following dataset is used to match parties to alliances
#alliance_parties = read.csv("~/DATA_AND_RESULTS/Alliance_parties.csv")

AMTurk_Survey_temp$PC_vote_choice = as.character(unlist(AMTurk_Survey_temp$PC_vote_choice))

AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Aam Aadmi Party")]                                      
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="All India Anna Dravida Munnetra Kazhagam")] ="NDA"
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="All India Majlis-e-Ittehad-ul-Muslimen")]                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="All India N R Congress")]="NDA"                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="All India Trinamool Congress")]                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="All India United Democratic Front")]                      
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Ambedkarwadi Republican Party of India")]                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Andhra Rastra Praja Samithi")]                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Apna Dal")] = "NDA"                                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bahujan Samaj Party")]                                    
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bahujan Sangharshh Dal")]                                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bahujan Vikas Aghadi")]="UPA"                                   
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bharatiya Ekta Dal")]                                      
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bharatiya Janata Party")]="NDA"                                 
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bharipa Bahujan Mahasangh")]                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bhartiya Shakti Chetna Party")]                           
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Biju Janata Dal")]                                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Bodoland People's Front")] = "NDA"                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Communist Party of India")] = "UPA"                             
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Communist Party of India (Marxist)")]  ="UPA"                   
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Communist Party of India (Marxist-Leninist) Liberation")]  ="UPA"

AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Desiya Murpokku Dravida Kazhagam")] ="NDA"                      
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Dravida Munnetra Kazhagam")] ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Gondwana Gantantra Party")]                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Hindusthan Praja Paksh Maharashtra")]                      
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Independent Candidate")]  

AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="India National Congress" )]  ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Indian National Congress"  )] ="UPA"                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Indian National Lok Dal" )]                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Indian Union Muslim League"    )]="UPA"                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jai Samaikyandhra Party"      )]                          
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jammu and Kashmir Peoples Democratic Party"    )]       
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jammu & Kashmir National Conference"    )]="UPA"                 
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jammu & Kashmir National Panthers Party"   )]             
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jammu & Kashmir People's Conference"      )]               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Janata Dal (Secular)"       )]="UPA"                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Janata Dal (United)"     )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jharkhand Mukti Morcha"     )]="UPA"                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Jharkhand Vikas Morcha (Prajatantrik)"   )]="UPA"                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Karunaadu Party"      )]                                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Kerala Congress (M)"    )] ="NDA"                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Lok Janshakti Party"   )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Lok Satta"                )]                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Loktantrik Janata Dal"   )] ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Maharashtra Navnirman Sena"    )]                          
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Manithaneya Makkal Katchi"     )]                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Naga People's Front"         )]="NDA"                            
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="National Democratic Alliance" )] ="NDA"                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Nationalist Congress Party")] ="UPA"                             
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Paattali Makkal Katchi")] = "NDA"                                 
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Peace Party"       )] ="UPA"                                    
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Peasants and Workers Party of India"    )]               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Prem Janata Dal"     )]                                   
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Puthiya Tamilagam"   )] ="NDA"                                   
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Rashtriya Janata Dal"   )] ="UPA"                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Rashtriya Lok Samata Party" )]  ="UPA"                           
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Republican Paksha (Khoripa)"  )]                          
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Republican Party of India"   )] ="NDA"                           
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Revolutionary Socialist Party"  )] ="UPA"                       
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Revolutionary Socialist Party of Kerala (Bolshevik)")] 
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Samaikya Telugu Rajyam"  )]                              
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Samta Party"    )]                                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Samta Samadhan Party" )]                                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Sanyukt Samajwadi Dal" )]                                  
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Sarva Janata Party" )]                                    
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Shiv Sena"    )]  ="NDA"                                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Sikkim Democratic Front"    )] ="NDA"                           
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Social Democratic Party of India"   )]                     
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Socialist Unity Centre of India (Communist)"  )]          
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Swabhimani Paksha"      )]                                 
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Tamil Nadu Makkal Congress"  )]                           
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Telangana Rashtra Samithi"  )]                             
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Telugu Desam Party"      )]                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="United Democratic Party" )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="United Progressive Alliance"  )] ="UPA"                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Viduthalai Chiruthaigal Katchi" )]                         
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="Welfare Party of India"  )]                               
AMTurk_Survey_temp$PC_vote_choice[which(AMTurk_Survey_temp$PC_vote_choice=="YSR Congress Party" )] 

# and PC_vote_choice 14
AMTurk_Survey_temp$PC_vote_choice_14 = as.factor(as.character(unlist(ifelse(as.character(unlist(AMTurk_Survey_temp$PC_vote_choice_14))=="",NA,
                                                                         as.character(unlist(AMTurk_Survey_temp$PC_vote_choice_14))))))

# the following dataset is used to match parties to alliances
#alliance_parties = read.csv("~/DATA_AND_RESULTS/Alliance_parties.csv")

AMTurk_Survey_temp$PC_vote_choice_14 = as.character(unlist(AMTurk_Survey_temp$PC_vote_choice_14))

AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Aam Aadmi Party")]                                      
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="All India Anna Dravida Munnetra Kazhagam")] ="NDA"
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="All India Majlis-e-Ittehad-ul-Muslimen")]                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="All India N R Congress")]="NDA"                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="All India Trinamool Congress")]                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="All India United Democratic Front")]                      
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Ambedkarwadi Republican Party of India")]                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Andhra Rastra Praja Samithi")]                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Apna Dal")] = "NDA"                                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bahujan Samaj Party")]                                    
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bahujan Sangharshh Dal")]                                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bahujan Vikas Aghadi")]="UPA"                                   
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bharatiya Ekta Dal")]                                      
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bharatiya Janata Party")]="NDA"                                 
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bharipa Bahujan Mahasangh")]                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bhartiya Shakti Chetna Party")]                           
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Biju Janata Dal")]                                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Bodoland People's Front")] = "NDA"                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Communist Party of India")] = "UPA"                             
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Communist Party of India (Marxist)")]  ="UPA"                   
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Communist Party of India (Marxist-Leninist) Liberation")]  ="UPA"

AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Desiya Murpokku Dravida Kazhagam")] ="NDA"                      
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Dravida Munnetra Kazhagam")] ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Gondwana Gantantra Party")]                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Hindusthan Praja Paksh Maharashtra")]                      
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Independent Candidate")]  

AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="India National Congress" )]  ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Indian National Congress"  )] ="UPA"                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Indian National Lok Dal" )]                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Indian Union Muslim League"    )]="UPA"                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jai Samaikyandhra Party"      )]                          
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jammu and Kashmir Peoples Democratic Party"    )]       
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jammu & Kashmir National Conference"    )]="UPA"                 
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jammu & Kashmir National Panthers Party"   )]             
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jammu & Kashmir People's Conference"      )]               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Janata Dal (Secular)"       )]="UPA"                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Janata Dal (United)"     )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jharkhand Mukti Morcha"     )]="UPA"                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Jharkhand Vikas Morcha (Prajatantrik)"   )]="UPA"                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Karunaadu Party"      )]                                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Kerala Congress (M)"    )] ="NDA"                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Lok Janshakti Party"   )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Lok Satta"                )]                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Loktantrik Janata Dal"   )] ="UPA"                              
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Maharashtra Navnirman Sena"    )]                          
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Manithaneya Makkal Katchi"     )]                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Naga People's Front"         )]="NDA"                            
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="National Democratic Alliance" )] ="NDA"                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Nationalist Congress Party")] ="UPA"                             
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Paattali Makkal Katchi")] = "NDA"                                 
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Peace Party"       )] ="UPA"                                    
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Peasants and Workers Party of India"    )]               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Prem Janata Dal"     )]                                   
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Puthiya Tamilagam"   )] ="NDA"                                   
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Rashtriya Janata Dal"   )] ="UPA"                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Rashtriya Lok Samata Party" )]  ="UPA"                           
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Republican Paksha (Khoripa)"  )]                          
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Republican Party of India"   )] ="NDA"                           
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Revolutionary Socialist Party"  )] ="UPA"                       
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Revolutionary Socialist Party of Kerala (Bolshevik)")] 
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Samaikya Telugu Rajyam"  )]                              
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Samta Party"    )]                                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Samta Samadhan Party" )]                                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Sanyukt Samajwadi Dal" )]                                  
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Sarva Janata Party" )]                                    
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Shiv Sena"    )]  ="NDA"                                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Sikkim Democratic Front"    )] ="NDA"                           
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Social Democratic Party of India"   )]                     
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Socialist Unity Centre of India (Communist)"  )]          
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Swabhimani Paksha"      )]                                 
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Tamil Nadu Makkal Congress"  )]                           
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Telangana Rashtra Samithi"  )]                             
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Telugu Desam Party"      )]                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="United Democratic Party" )]="NDA"                                
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="United Progressive Alliance"  )] ="UPA"                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Viduthalai Chiruthaigal Katchi" )]                         
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="Welfare Party of India"  )]                               
AMTurk_Survey_temp$PC_vote_choice_14[which(AMTurk_Survey_temp$PC_vote_choice_14=="YSR Congress Party" )] 

# remove weekly doubles
AMTurk_Survey_temp$weeks = floor(as.numeric(difftime(as.Date("19/05/2019","%d/%m/%Y"),AMTurk_Survey_temp$survey_date)/7)) + 1

ID_GBIP_week = paste(AMTurk_Survey_temp$ID_GBIP,AMTurk_Survey_temp$weeks)
AMTurk_Survey_temp = AMTurk_Survey_temp[order(ID_GBIP_week),]
AMTurk_Survey_temp = AMTurk_Survey_temp[!duplicated(ID_GBIP_week),]

write.csv(AMTurk_Survey_temp,file = "Generated Quantities/Online_Surveys.csv",row.names = FALSE)
