
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


#### FINALEMENT ON NE STEM PAS, CA PREND PLUS DE TEMPS ET ON NE GAGNE PAS EN PERFORMANCE
# library(SnowballC)
# 
# stemmed_text <- tagged_triplet%>%
#   select(text,id)%>%
#   unnest_tokens(text_w,text)%>%
#   mutate(text_w=wordStem(text_w,language="french"))%>%
#   group_by(id)%>%
#   summarise(text_stemmed=paste(text_w,collapse = " "))
# 
# tagged_triplet=merge(tagged_triplet,stemmed_text,by="id")
# 
# # ATTENTION ICI ON REMPLACE LE TEXT PAR SA VERSION STEMMED
# tagged_triplet <- tagged_triplet%>%mutate(text_original=text,text=text_stemmed)


all_ids = tagged_triplet$id
indicateurs=tagged_triplet$Indicateur%>%unique
length(indicateurs)


# split(indicateurs,1:10)

train_ind=sample(indicateurs,size = round(.75*length(indicateurs)))
train_sample=tagged_triplet$Indicateur%in%train_ind
train_ids=tagged_triplet[train_sample,]$id
test_ids = setdiff(all_ids, train_ids)






tagged_triplet <- tagged_triplet%>%select(id,text,Indicateur_enriched,variable,value)
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


train_ngrams <- rbind(train_ngrams_indic%>%select(-ngram_fac),train_ngrams_tags%>%select(-ngram_fac))

dtm_train <- sparseMatrix(i=train_ngrams$id_loc,
                             j=train_ngrams$ngram_num,
                              x=1,
                             dimnames = list(train$id_loc,
                                             labels_ngrams))


# save(list="ngrams_lab_id",file="RData/ngrams_lab_id.RData")
dtrain <- xgb.DMatrix(dtm_train, label = train[['value']])


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

match_ngram_id_indic<- train_ngrams_indic%>%select(ngram,ngram_num)%>%distinct()
test_ngrams_indic <- rbind(test_1gram_indic,test_2gram_indic,test_3gram_indic)
test_ngrams_indic <- test_ngrams_indic[test_ngrams_indic$ngram%in%ngrams_from_indic,]
test_ngrams_indic <- merge(test_ngrams_indic,match_ngram_id_indic,by="ngram")

match_ngram_id_tags<- train_ngrams_tags%>%select(ngram,ngram_num)%>%distinct()
test_ngrams_tags <- rbind(test_1gram_tags,test_2gram_tags,test_3gram_tags)
test_ngrams_tags <- merge(test_ngrams_tags,match_ngram_id_tags,by="ngram")

save(list = c("match_ngram_id_indic","match_ngram_id_tags","labels_ngrams"),file = "RData/match_ngram_id.RData")


test_ngrams <- rbind(test_ngrams_indic,test_ngrams_tags)

resize_ngrams_vec=max(test_ngrams$ngram_num)
dtm_test <- sparseMatrix(i=test_ngrams$id_loc,
                          j=test_ngrams$ngram_num,
                          x=1,
                          dimnames = list(test$id_loc,
                                          labels_ngrams[1:resize_ngrams_vec]))



params=list(eta=.02,
            max_depth=15,
            min_child_weight=1,
            subsample=.3,
            colsample_bytree=.1,
            objective="binary:logistic",
            eval_metric="auc",
            gamma=.01)

  # DUMMY
dtest <- xgb.DMatrix(dtm_test, label = test[['value']])

watchlist <- list(eval = dtest, train = dtrain)

system.time(xgbmodel <- xgb.train(params = params,dtrain,
                                  verbose = 1,print_every_n = 100,
                                  nrounds = 2000,watchlist,
                                  early_stopping_rounds=50))

save(list="xgbmodel",file="www/xgbmodel_tidytext.RData")
rdrop2::drop_upload(file = "www/xgbmodel_tidytext.RData",dtoken = token,path = "www", mode = "overwrite")

################################################################
##########LE MODELE DECONNE SUR DES CHOSES SIMPLES #############
##########PARCE QUE NGRAM DUMMY NE FONCTIONNE PAS SI ###########
##########LE MOT EST COMMUN A L'INDICATEUR ET AU TAG############
# Ne marche pas pour : handicap, âge, accessibilité, cancer, femmes dépenses...
# Il faut prendre create_dtm_non_tagged, en faire une fonction, 
# l'appliquer à train, test, untagg et dissociant les features 
# dummy ngram indic et 
# dummy ngram tag
################################################################