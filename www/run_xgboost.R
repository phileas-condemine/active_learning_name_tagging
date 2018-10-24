start_time=Sys.time()

timings=c("slow","fast")
timing=timings[1]
# timing=timings[2]

library(data.table)
library(stringi)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)
library(xgboost)
library(Matrix)
set.seed(2018L)


load("RData/data2_prep.RData")
load("RData/tagged_triplet.RData")
load("RData/dtm_prep.RData")
system.time({
nb_ngram_indic <- max(j)
match_notions_indic <- match_notions_indic%>%mutate(id=(tag_id-1)*20000+i)%>%select(-i,-tag_id)
i2 <- expand.grid(tag_id=1:length(tags),ind_id=i)%>%mutate(id=(tag_id-1)*20000+ind_id)%>%.$id
j2 <- expand.grid(tag_id=1:length(tags),j=j)%>%.$j

i_tags2 <- expand.grid(tag_id=i_tags,ind_id=1:length(text))%>%mutate(id=(tag_id-1)*20000+ind_id)%>%.$id
j_tags2 <- expand.grid(j_tags=j_tags,ind_id=1:length(text))%>%.$j_tags

j_tags2 <- j_tags2+nb_ngram_indic

i_dimname <- data.frame(i2)%>%distinct%>%.$i2
# sum(!(unique(i_tags2)%in%i_dimname))
# sum(!(unique(match_notions_indic$id)%in%i_dimname))
# sum(!(unique(i2)%in%i_dimname))
dtm_sp <- sparseMatrix(i=c(i2,i_tags2,
                           match_notions_indic$id),
                       j=c(j2,j_tags2,match_notions_indic$j),
                       dimnames = list(1:max(i_dimname)%>%as.character,
                                       c(dimnm[[2]],
                                         paste0("__",dimnm_tags[[2]]),
                                         "notions")))
})





# CHECK COHERENCE DU DTM
# tmp <- dtm_sp[40100,]
# names(tmp)[tmp]#LES NGRAMS ASSOCIES
# data2[100,]$Indicateur_enriched
# tags[3-1]


tagged_triplet=data.table(tagged_triplet)[,.SD[1],by="i"]

tagged_ids = unique(tagged_triplet$id)
# check correspondance des id de tagged_triplet et de dtm_sp
# random_row=tagged_triplet[sample(nrow(tagged_triplet),1),]
# random_row$Indicateur
# tmp <- dtm_sp[random_row$id,]
# names(tmp)[tmp]#LES NGRAMS ASSOCIES ATTENTION c'est l'indicateur + famille, prod, source, tag...


train_smp=sample(tagged_ids,size = round(.75*length(tagged_ids)))
test_smp = setdiff(tagged_ids, train_smp)
train_ind=rowSums(expand.grid((1:length(tags)-1)*20000,train_smp))
test_ind=rowSums(expand.grid((1:length(tags)-1)*20000,test_smp))
###### NGRAMS a garder #######
# ngrams_from_indic <- c(train_1gram_stats$ngram,train_2gram_stats$ngram,train_3gram_stats$ngram)
# nb_indic_a_priori=length(ngrams_from_indic)
# if("ngrams_non_pertinents.RData"%in%list.files("RData")){
# load("RData/ngrams_non_pertinents.RData")
# }else{
#   ngrams_non_pertinents=NULL
# }
# 
# indic_non_pertinent=ngrams_non_pertinents[grep("i_",ngrams_non_pertinents)]
# indic_non_pertinent=gsub("i_","",indic_non_pertinent)
# 
# ngrams_from_indic <- setdiff(ngrams_from_indic,indic_non_pertinent)
#######

# SPLIT xgb.DMatrix en TEST et TRAIN

system.time({
ngrams_tagged <- which(colSums(dtm_sp[c(train_ind,test_ind),])>0)
knowns_ind <- tagged_triplet[tagged_triplet$id%in%train_ind,][['i']] ## On avait un pb parce que dans tagged_id il y a parfois 34, parfois 36 tags !
train_ind <- train_ind[train_ind%in%knowns_ind]
train_ind <- sort(train_ind)
train_labels <- tagged_triplet[tagged_triplet$id%in%train_ind,]%>%
  arrange(i)%>%.[['value']]
train_dtm <- dtm_sp[train_ind,ngrams_tagged]
dtrain <- xgb.DMatrix(data = as(train_dtm,"dgCMatrix"), 
                      label = train_labels)

knowns_ind <- tagged_triplet[tagged_triplet$id%in%test_ind,][['i']] ## On avait un pb parce que dans tagged_id il y a parfois 34, parfois 36 tags !
test_ind <- test_ind[test_ind%in%knowns_ind]
test_ind <- sort(test_ind)
test_labels <- tagged_triplet[tagged_triplet$id%in%test_ind,]%>%
  arrange(i)%>%.[['value']]
test_dtm <- dtm_sp[test_ind,ngrams_tagged]
dtest <- xgb.DMatrix(data = as(test_dtm,"dgCMatrix"), 
                      label = test_labels)
watchlist <- list(train = dtrain,eval = dtest)
})


###################################################################
############################## RUN XGBOOST ########################
###################################################################
# 
# params=list(eta=.1,
#             max_depth=18,
#             min_child_weight=1,
#             subsample=.2,
#             colsample_bytree=100/length(ngrams_tagged),#100 colonnes à la fois
#             objective="binary:logistic",
#             eval_metric="auc",
#             gamma=.01)

if (timing=="slow"){
params=list(eta=.1,
            max_depth=8,
            min_child_weight=1,
            subsample=.35,
            colsample_bytree=300/length(ngrams_tagged),#100 colonnes à la fois
            objective="binary:logistic",
            eval_metric="auc",
            gamma=.01)
nrounds=1E+5
} else if (timing=="fast"){
  params=list(eta=.2,
              max_depth=8,
              min_child_weight=1,
              subsample=.35,
              colsample_bytree=300/length(ngrams_tagged),#100 colonnes à la fois
              objective="binary:logistic",
              eval_metric="auc",
              gamma=.01)
  nrounds=1E+4
}

system.time(xgbmodel <- xgb.train(params = params,dtrain,
                                  verbose = 1,print_every_n = 40,
                                  nrounds = nrounds,watchlist,
                                  early_stopping_rounds=500))



print(Sys.time()-start_time)


system.time(save(list="xgbmodel",file="www/xgbmodel_tidytext_notions.RData"))
system.time(rdrop2::drop_upload(file = "www/xgbmodel_tidytext_notions.RData",dtoken = token,path = "www", mode = "overwrite"))

imp=xgb.importance(feature_names = NULL,xgbmodel)
imp$feat_name=dimnames(dtrain)[[2]][1+as.numeric(imp$Feature)]
head(imp,100)

# PREDICT ON UNTAGGED


untagged_dtm=dtm_sp[-c(test_ind,train_ind),ngrams_tagged]

# system.time(pred <- predict(object=xgbmodel,
#                       newdata = as(untagged_dtm,"dgCMatrix"))
#               )

system.time(pred <- 
              data.frame(id=1:max(summary(untagged_dtm)$i),
                   pred=predict(object=xgbmodel,
                        newdata = as(untagged_dtm,"dgCMatrix"))
            ))


pred$tag_id=as.numeric(as.character(pred$id))%/%20000
pred$indic_id=as.numeric(as.character(pred$id))%%20000

pred$tags=as.factor(pred$tag_id)
levels(pred$tags) <- tags

rm(untagged_dtm,xgbmodel)
gc()
pred=data.table(pred)
setorder(pred,-pred,indic_id)

indicateurs_pred=pred[,list(tag1=tags[1],
                            tag2=tags[2],
                            tag3=tags[3],
                            max_prob=max(pred),
                            min_prob=min(pred),
                            sum_prob=sum(pred)),
                      by=c("indic_id")]


indicateurs_pred=merge(indicateurs_pred,data2,by.x="indic_id",by.y="index")

min_max_prob=indicateurs_pred[which.min(max_prob)]
min_sum_prob=indicateurs_pred[which.min(sum_prob)]


system.time(save(list="indicateurs_pred",file="RData/indicateurs_a_tagger.RData"))
system.time(rdrop2::drop_upload("RData/indicateurs_a_tagger.RData",dtoken = token, mode = "overwrite",path = "RData"))
