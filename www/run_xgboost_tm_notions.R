library(data.table)
library(stringi)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(xgboost)
library(Matrix)

# library(devtools)
# options(devtools.install.args = "--no-multiarch") # if you have 64-bit R only, you can skip this
# install_github("Microsoft/LightGBM", subdir = "R-package")


# load("../RData/indicateurs_tags_pages.RData")#from data prep.R
load("RData/tagged_triplet.RData")#from data prep.R
load("RData/data2_prep.RData")

set.seed(2018L)
cardinality=sapply(data2,function(x)length(unique(x)))
data2=data2[,cardinality>1]


data2$Indicateur_enriched=paste(data2$Indicateur,data2$Famille,data2$`Classement producteur Niveau 3 (le plus détaillé)`,data2$Source,data2$Producteur)

tagged_triplet <- merge(tagged_triplet,
                        data2[,c("Base","Indicateur","Indicateur_enriched")],
                        by=c("Base","Indicateur"))

# tolower(df_man_tags$Indicateur[1])%in%tagged_triplet$Indicateur




tagged_triplet$id=1:nrow(tagged_triplet)
tagged_triplet$text=paste(tagged_triplet$Indicateur_enriched,tagged_triplet$variable)

all_ids = tagged_triplet$id
indicateurs=tagged_triplet$Indicateur%>%unique
length(indicateurs)

train_ind=sample(indicateurs,size = round(.75*length(indicateurs)))
train_sample=tagged_triplet$Indicateur%in%train_ind
train_ids=tagged_triplet[train_sample,]$id
test_ids = setdiff(all_ids, train_ids)


tagged_triplet <- tagged_triplet%>%dplyr::select(id,text,Indicateur_enriched,variable,value)
setDT(tagged_triplet)
setkey(tagged_triplet, id)

train = tagged_triplet[J(train_ids)]
test = tagged_triplet[J(test_ids)]

train$id_loc=1:nrow(train)
# with TRAIN

##################################
#### Extract ngrams from indic ###
##################################

train <- train%>%mutate(Indicateur_enriched=tolower(Indicateur_enriched))

limit=2*36
train_1gram_indic <- train%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=1)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)
train_1gram_stats <- train_1gram_indic %>%
  count(ngram,sort = F)%>%
  filter(n>limit)

train_2gram_indic <- train%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=2)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)

train_2gram_stats <- train_2gram_indic %>%
  count(ngram,sort = F)%>%
  filter(n>limit)

train_3gram_indic <- train%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=3)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)

train_3gram_stats <- train_3gram_indic %>%
  count(ngram,sort = F)%>%
  filter(n>limit)

ngrams_from_indic <- c(train_1gram_stats$ngram,train_2gram_stats$ngram,train_3gram_stats$ngram)


##################################
#### Extract ngrams from tags ####
##################################


train <- train%>%mutate(variable=tolower(variable))

train_1gram_tags <- train%>%
  unnest_tokens(ngram,variable,token="ngrams",n=1)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()
train_1gram_stats <- train_1gram_tags %>%
  count(ngram,sort = F)
train_2gram_tags <- train%>%
  unnest_tokens(ngram,variable,token="ngrams",n=2)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()

train_2gram_stats <- train_2gram_tags %>%
  count(ngram,sort = F)

train_3gram_tags <- train%>%
  unnest_tokens(ngram,variable,token="ngrams",n=3)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()

train_3gram_stats <- train_3gram_tags %>%
  count(ngram,sort = F)

# train_1gram_stats%>%filter(n>687)

ngrams_from_vars <- c(train_1gram_stats$ngram,train_2gram_stats$ngram,train_3gram_stats$ngram)

# save(list = c("ngrams_from_indic","ngrams_from_vars"),file = "RData/trigram_tidytext.RData")
nb_ngrams=length(ngrams_from_indic)
keep_ngrams <- c(ngrams_from_indic,ngrams_from_vars)
length(keep_ngrams)

train_ngrams_indic <- rbind(train_1gram_indic,train_2gram_indic,train_3gram_indic)
train_ngrams_indic <- train_ngrams_indic[train_ngrams_indic$ngram%in%ngrams_from_indic,]
train_ngrams_indic$ngram_fac <- factor(train_ngrams_indic$ngram)
train_ngrams_indic$ngram_num <- as.numeric(train_ngrams_indic$ngram_fac)

train_ngrams_tags <- rbind(train_1gram_tags,train_2gram_tags,train_3gram_tags)
train_ngrams_tags$ngram_fac <- factor(train_ngrams_tags$ngram)
train_ngrams_tags$ngram_num <- as.numeric(train_ngrams_tags$ngram_fac)+nb_ngrams

labels_ngrams=c(paste0("i_",levels(train_ngrams_indic$ngram_fac)),paste0("t_",levels(train_ngrams_tags$ngram_fac)))

# ngrams_lab_id=data.frame(ngram=c(levels(train_ngrams_indic$ngram_fac),levels(train_ngrams_tags$ngram_fac)),
#                          id=1:(length(levels(train_ngrams_indic$ngram_fac))+length(levels(train_ngrams_tags$ngram_fac))),
#                          lab=labels_ngrams)





# with VALIDATION


test$id_loc=1:nrow(test)
# with test

##################################
#### Extract ngrams from indic ###
##################################

test <- test%>%mutate(Indicateur_enriched=tolower(Indicateur_enriched))

limit=2*36
test_1gram_indic <- test%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=1)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)
test_2gram_indic <- test%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=2)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)
test_3gram_indic <- test%>%
  unnest_tokens(ngram,Indicateur_enriched,token="ngrams",n=3)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)


##################################
#### Extract ngrams from tags ####
##################################


test <- test%>%mutate(variable=tolower(variable))

test_1gram_tags <- test%>%
  unnest_tokens(ngram,variable,token="ngrams",n=1)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()
test_2gram_tags <- test%>%
  unnest_tokens(ngram,variable,token="ngrams",n=2)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()
test_3gram_tags <- test%>%
  unnest_tokens(ngram,variable,token="ngrams",n=3)%>%
  group_by(ngram,id_loc)%>%
  summarise(value=1)%>%na.omit()

match_ngram_id_indic<- train_ngrams_indic%>%dplyr::select(ngram,ngram_num)%>%distinct()
test_ngrams_indic <- rbind(test_1gram_indic,test_2gram_indic,test_3gram_indic)
test_ngrams_indic <- test_ngrams_indic[test_ngrams_indic$ngram%in%ngrams_from_indic,]
test_ngrams_indic <- merge(test_ngrams_indic,match_ngram_id_indic,by="ngram")

match_ngram_id_tags<- train_ngrams_tags%>%dplyr::select(ngram,ngram_num)%>%distinct()
test_ngrams_tags <- rbind(test_1gram_tags,test_2gram_tags,test_3gram_tags)
test_ngrams_tags <- merge(test_ngrams_tags,match_ngram_id_tags,by="ngram")

save(list = c("match_ngram_id_indic","match_ngram_id_tags","labels_ngrams"),file = "RData/match_ngram_id.RData")






# DUMMY

# test_weights=merge(test%>%dplyr::select(variable),tags_stats,by="variable")
# test_weights$weight=1-test_weights$freq


###################################################################
############### AJOUT HEURISTIQUE A PARTIR DES NOTIONS ############
###################################################################
library(readxl)
notions <- readxl::read_xlsx("data_original/20180426_Dictionnaire des notions edited.xlsx")

notions <- lapply(1:ncol(notions),function(i){
  data.frame(notion=names(notions)[i],termes=unname(notions[,i]))%>%
    na.omit%>%
    mutate_all(as.character)
}
)%>%
  do.call(what = "rbind")%>%
  mutate(termes=tolower(termes))

library(stringr)
nb_features=max(train_ngrams_tags$ngram_num)


############################## TRAIN ##############################

match_notions_indic <- do.call("rbind",sapply(1:nrow(notions),
                                              function(i){
                                                matching=str_which(pattern = paste0("(^|( ))(",notions$termes[i],")(( )|($))"),
                                                                   string = train$Indicateur_enriched)
                                                potential_match=which(train$variable==notions$notion[i])
                                                matching=matching[matching%in%potential_match]
                                                if(length(matching)>0){
                                                  data.frame(i= matching,j=nb_features+1)
                                                } else NULL
                                              }
))
match_notions_indic <- unique(match_notions_indic)

train_ngrams <- rbind(train_ngrams_indic[,c("id_loc","ngram_num")],
                      train_ngrams_tags[,c("id_loc","ngram_num")],
                      match_notions_indic%>%rename(id_loc=i,ngram_num=j))

labels_ngrams <- c(labels_ngrams,"notions")

dtm_train <- sparseMatrix(i=train_ngrams$id_loc,
                          j=train_ngrams$ngram_num,
                          x=1,
                          dimnames = list(train$id_loc,
                                          labels_ngrams))

dtrain <- xgb.DMatrix(dtm_train, label = train[['value']])


############################## TEST ##############################



match_notions_indic_test <- do.call("rbind",sapply(1:nrow(notions),
                                                   function(i){
                                                     matching=str_which(pattern = paste0("(^|( ))(",notions$termes[i],")(( )|($))"),
                                                                        string = test$Indicateur_enriched)
                                                     potential_match=which(test$variable==notions$notion[i])
                                                     matching=matching[matching%in%potential_match]
                                                     if(length(matching)>0){
                                                       data.frame(i= matching,j=nb_features+1)
                                                     } else NULL
                                                   }
))
match_notions_indic_test <- unique(match_notions_indic_test)

test_ngrams <- rbind(test_ngrams_indic[,c("id_loc","ngram_num")],
                     test_ngrams_tags[,c("id_loc","ngram_num")],
                     match_notions_indic_test%>%rename(id_loc=i,ngram_num=j))


# resize_ngrams_vec=max(test_ngrams$ngram_num)
dtm_test <- sparseMatrix(i=test_ngrams$id_loc,
                         j=test_ngrams$ngram_num,
                         x=1,
                         dimnames = list(test$id_loc,
                                         labels_ngrams))


dtest <- xgb.DMatrix(dtm_test, label = test[['value']])


###################################################################
############################## RUN XGBOOST ########################
###################################################################

params=list(eta=.01,
            max_depth=15,
            min_child_weight=1,
            subsample=.2,
            colsample_bytree=.1,
            objective="binary:logistic",
            eval_metric="auc",
            gamma=.01)

watchlist <- list(eval = dtest, train = dtrain)




system.time(xgbmodel <- xgb.train(params = params,dtrain,
                                  verbose = 1,print_every_n = 100,
                                  nrounds = 10000,watchlist,
                                  early_stopping_rounds=50))



save(list="xgbmodel",file="www/xgbmodel_tidytext_notions.RData")
rdrop2::drop_upload(file = "www/xgbmodel_tidytext_notions.RData",dtoken = token,path = "www", mode = "overwrite")

################################################################
# rel_imp=xgb.importance(feature_names = labels_ngrams,model = xgbmodel)
# 
# rel_imp <- rel_imp%>%mutate_at(.vars = c("Gain","Cover","Frequency"),function(x)x/min(x))
# rel_imp%>%filter(Gain>1000)
# 



################################################################