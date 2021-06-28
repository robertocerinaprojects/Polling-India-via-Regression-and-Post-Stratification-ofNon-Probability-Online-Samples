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
                                                        colnames(stratification_frame) %in% colnames(cbind(survey_Z,P=P,WtE=WtE))]),
                                                      .SDcols = c("weights")
]
# augment strat frame for each `source'
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
# Vote
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
save(V_model_stupid,file = 'Generated Quantities/V_model_stupid_NoT.RData',compress = TRUE)
load(file = 'Generated Quantities/V_model_stupid_NoT.RData')
# # # # #
# # # # #
# # # # #
# # # # #
n.sims = 1000
library(parallel)
# Select relevant variables from strat fram
dim(stratification_frame_augmented)
X.test = stratification_frame_augmented[, colnames(stratification_frame_augmented) %in% colnames( cbind(survey_Z_data,WtE = WtE,P = P))|
                                          colnames(stratification_frame_augmented) == "weights",with=FALSE]
# aggregate over same variables 
X.test = X.test[,lapply(.SD,sum),by = c(colnames(X.test)[-which(colnames(X.test)=="weights")]),.SDcols = c("weights")]
# predict turnout on X.Test
X.test$T_preds = 1
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
save(X.test,file = 'Generated Quantities/Prediction_Frame_noT.RData',compress = TRUE)
gc()
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

# Aggregate 
library(mc2d)
# # # # #
# # # # #
# # # # #
# # # # #
# identify redundant cells for vote-choice
V.redundant.cells =  which(duplicated(X.test[,colnames(X.test) %in% c("weights",V_model_stupid[[1]]$forest$independent.variable.names),with=FALSE]))
V.redundant.cells_list = c()
for(q in 1:length(unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames( X.test)[grep("pred.week",colnames( X.test))]))))){
  V.redundant.cells_list = c(V.redundant.cells_list,V.redundant.cells + (q-1)*dim(X.test)[1] )
}
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
    
    PREDS_temp_WtE = as.data.table(cbind(temp,
                                         WtE =  unique(as.numeric(gsub(".*?([0-9]+).*", "\\1", colnames( X.test)[grep("pred.week",colnames( X.test))])))[w_id]))
    PREDS_temp = rbind(PREDS_temp,PREDS_temp_WtE)
  }
  # # # # #
  RESULTS = PREDS_temp
  # # # AGGREGATE # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # sto
  
  # for vote choice 
  RESULTS[,levels(V)] = as.data.table(t(apply(RESULTS [,levels(V),with =FALSE],1,function(x){x/sum(x)})))
  # first aggregate
  
  # # # CHALLENGE: aggregating over redundant cells will shrink variance of simulations by shrinking each sim. toward the overall mean.
  
  if(length(V.redundant.cells_list)!=0){ temp = X.test[-V.redundant.cells_list,1:which(colnames(X.test)=="weights"),with=FALSE]; 
                                         tmp = RESULTS[-V.redundant.cells_list,]}else{
                                         temp = X.test[,1:which(colnames(X.test)=="weights"),with=FALSE]; 
                                         tmp = RESULTS}
  
  # Vote-Choice Aggregation
  RESULTS = cbind( foreach(w = max(WtE):min(WtE) ,.combine = 'rbind') %do% (temp), 
                   tmp)

  # vote choice sims incorporate uncertainty about conditional mean; let's simulate from it 
  TEMP = rbinom(n = dim( RESULTS)[1],size = round(RESULTS$weights),prob = as.numeric(unlist(RESULTS[,levels(V)[1],with=FALSE])))
  TEMP = cbind(TEMP, rbinom(n = dim( RESULTS)[1],size = round(RESULTS$weights)-TEMP,prob = as.numeric(unlist(RESULTS[,levels(V)[2],with=FALSE]))))
  TEMP = cbind(TEMP,  round(RESULTS$weights)-rowSums(TEMP))
  colnames(TEMP) = paste(levels(V),"W",sep="_")
  RESULTS = cbind(RESULTS,TEMP)
  
  gc()
  # STATE-LEVEL
  STATE_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                       by= c("WtE","states","zones"),
                       .SDcols = c("weights",colnames(TEMP))]
  STATE_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% STATE_TEMP[,colnames(TEMP)[j],with=FALSE]/STATE_TEMP$weights)
  STATE_RESULTS = append(STATE_RESULTS,list(STATE_TEMP))
  save(STATE_RESULTS,file = 'Generated Quantities/STATE_RESULTS_NoT.RData',compress = TRUE)
  gc()
  #
  #
  #
  # ZONE-LEVEL
  ZONE_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                      by= c("WtE","zones"),
                      .SDcols = c("weights",colnames(TEMP))]
  ZONE_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% ZONE_TEMP[,colnames(TEMP)[j],with=FALSE]/ZONE_TEMP$weights)
  # append and save results 
  ZONE_RESULTS = append(ZONE_RESULTS,list( ZONE_TEMP))
  save(ZONE_RESULTS,file = 'Generated Quantities/ZONE_RESULTS_NoT.RData',compress = TRUE)
  #
  #
  #
  gc()
  # NAT-LEVEL
  NAT_TEMP = RESULTS[,lapply(.SD,function(x){sum(x)}),
                     by= c("WtE"),
                     .SDcols = c("weights",colnames(TEMP))]
  NAT_TEMP[,colnames(TEMP)] = (foreach(j = 1:nlevels(V),.combine = 'cbind') %do% NAT_TEMP[,colnames(TEMP)[j],with=FALSE]/NAT_TEMP$weights)
  

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
          TEMP2)
      )
    
  }
  gc()
  # append and save results 
  NAT_RESULTS = append(NAT_RESULTS,list( RTEMP))
  save(NAT_RESULTS,file = 'Generated Quantities/NAT_RESULTS_NoT.RData',compress = TRUE)
  gc()
  #
  #
  #
  end.time.draw = Sys.time()
  print(paste('time taken for this draw:',difftime(end.time.draw,start.time.draw,units = 'mins'),"mins"))
}

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

pdf(file = 'Plots/NAT_vote_share_over_time_NoT.pdf',height = 7.5,width = 7.5)
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
STATE_RES_TABLE = STATE_RES_TABLE[,lapply(.SD,function(x){mean(x)}),by = c("states","zones"),.SDcols = c("OTHER_W","NDA_W","UPA_W")]
#
pdf(file = 'Plots/State_Results_NoT.pdf',width = 10,height = 10)
par(mfrow = c(2,2))
for(i in c("NDA_pred","UPA_pred","OTHER_pred")){
  x = cbind(NDA_pred = STATE_RES_TABLE$NDA_W,
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
ZONE_RES_TABLE = ZONE_RES_TABLE[,lapply(.SD,function(x){mean(x)}),by = c("zones"),.SDcols = c("OTHER_W","NDA_W","UPA_W")]
#
pdf(file = 'Plots/Zone_Results_NoT.pdf',width = 10,height = 10)
par(mfrow = c(2,2))
for(i in c("NDA_pred","UPA_pred","OTHER_pred")){
  x = cbind(NDA_pred = ZONE_RES_TABLE$NDA_W,
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
  x = cbind(V_NDA_pred =  NAT_RES_TABLE$NDA_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6],
            V_OTHER_pred =  NAT_RES_TABLE$OTHER_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6],
            V_UPA_pred =  NAT_RES_TABLE$UPA_W[NAT_RES_TABLE$draw==i &  NAT_RES_TABLE$WtE == 6] )
  y = as.numeric(as.character(unlist(nat_res_obs)))[match(colnames(x),colnames(nat_res_obs))]
  x = as.numeric(as.character(unlist(x)))
  
  delta = 100*x-unique(HIST[which(HIST$Year==2014),c("NAT_Percentage.Votes.NDA","NAT_Percentage.Votes.UPA","NAT_Percentage.Votes.OTHER")])
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
                    table(c("NDA","UPA","OTHER")[apply(HIST[HIST$Year==2014,c("Vote_Share_Percentage.NDA","Vote_Share_Percentage.UPA","Vote_Share_Percentage.OTHER")],1,function(x){which(x==max(x))})])["OTHER"],89 - temp19$OTHER),
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
pdf(file = 'Plots/uswing_seats_distribution_NoT.pdf',width = 11.5,height = 8.5)
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
