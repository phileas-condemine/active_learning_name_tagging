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
library(purrr)
library(purrrlyr)

options("scipen"=100, "digits"=10) # GESTION DE LA CONVERSION DES NUMERICS EN CHARACTER SANS UTILISER LA NOTATION SCIENTIFIQUE !!!
set.seed(2018L)
load("RData/data2_prep.RData")
load("RData/tagged_triplet.RData")
load("RData/dtm_prep.RData")
system.time({
nb_ngram_indic <- max(j)
match_notions_indic <- match_notions_indic%>%mutate(id=(tag_id-1)*nb_indicateurs+i)%>%select(-i,-tag_id)

i2 <- expand.grid(tag_id=1:length(tags),ind_id=i)%>%mutate(id=(tag_id-1)*nb_indicateurs+ind_id)%>%.$id
j2 <- expand.grid(tag_id=1:length(tags),j=j)%>%.$j

i_tags2 <- expand.grid(tag_id=i_tags,ind_id=1:length(text))%>%mutate(id=(tag_id-1)*nb_indicateurs+ind_id)%>%.$id
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

# VERIFICATION de l'indicateur 10403 part décès prématurés cancer pancréas selon sexe
# on devrait  le trouver avec le tag cancer !!!

index_test=7116#10403
i_variantes=as.character(0:35*nb_indicateurs+index_test)

ngrams=dtm_sp[index_test,]
ngrams=names(ngrams[ngrams])
ngrams

dtm_sp[index_test,"notions"]

dtm_sp[i_variantes,"notions"]

# Les indicateurs associés à des notions
with_notions=dtm_sp[,"notions"]
with_notions=names(with_notions[with_notions])
with_notions
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
train_ind=rowSums(expand.grid((1:length(tags)-1)*nb_indicateurs,train_smp))
test_ind=rowSums(expand.grid((1:length(tags)-1)*nb_indicateurs,test_smp))
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
            colsample_bytree=300/length(ngrams_tagged),# 300 colonnes à la fois
            objective="binary:logistic",
            eval_metric="auc",
            gamma=.01)
nrounds=1E+4
} else if (timing=="fast"){
  params=list(eta=.2,
              max_depth=8,
              min_child_weight=1,
              subsample=.35,
              colsample_bytree=300/length(ngrams_tagged), # 300 colonnes à la fois
              objective="binary:logistic",
              eval_metric="auc",
              gamma=.01)
  nrounds=1E+3
}

system.time(xgbmodel <- xgb.train(params = params,dtrain,
                                  verbose = 1,print_every_n = 40,
                                  nrounds = nrounds,watchlist,
                                  early_stopping_rounds=nrounds/20))



print(Sys.time()-start_time)


system.time(save(list="xgbmodel",file="www/xgbmodel_tidytext_notions.RData"))
system.time(rdrop2::drop_upload(file = "www/xgbmodel_tidytext_notions.RData",dtoken = token,path = "www", mode = "overwrite"))

# load("www/xgbmodel_tidytext_notions.RData")

imp=xgb.importance(feature_names = NULL,xgbmodel)
head(imp,100)

# GET MOST INFLUENT NGRAMS IN THE INDIC
# imp$Feature=gsub("__","",imp$Feature)
get_imp_ngrams=data.table(i=i,j=j)
dico_ngrams=data.frame(j=1:max(j),ngram=dimnm[[2]])
get_imp_ngrams=merge(get_imp_ngrams,dico_ngrams,by="j")

get_imp_ngrams=merge(get_imp_ngrams,imp,by.x="ngram",by.y="Feature")

setorder(get_imp_ngrams,-Gain)
get_imp_ngrams=get_imp_ngrams[,list(ngram1=ngram[1],
                     ngram2=ngram[2],
                     ngram3=ngram[3],
                     ngram4=ngram[4],
                     ngram5=ngram[5]),by="i"]

# PREDICT ON UNTAGGED

done_id=unique(c(test_ind,train_ind)%%nb_indicateurs)
max(done_id)
done_id=expand.grid((0:35*nb_indicateurs),done_id)%>%{.$Var1+.$Var2}
done_id%>%{.[.%%nb_indicateurs==8990]}
untagged_dtm=dtm_sp[setdiff(dimnames(dtm_sp)[[1]],as.character(done_id)),
                    ngrams_tagged]


table((as.numeric(dimnames(untagged_dtm)[[1]])-1)%/%nb_indicateurs)
# empty_rows=rowSums(untagged_dtm)
# empty_rows=names(empty_rows[empty_rows==0])
# empty_rows=as.numeric(empty_rows)
# empty_rows
# untagged_dtm=untagged_dtm[-empty_rows,]


system.time(prediction <- predict(object=xgbmodel,
                        newdata = as(untagged_dtm,"dgCMatrix")))
pred <- data.frame(id=as.numeric(dimnames(untagged_dtm)[[1]]),
                   pred=prediction,notion=untagged_dtm[,"notions"])
pred=data.table(pred)

#### ETAPE CRUCIALE, ON HACKE LA PREDICTION AVEC LES NOTIONS ???
pred$pred=pmax(pred$pred,pred$notion)


pred$tag_id=as.numeric(as.character(pred$id))%/%nb_indicateurs
pred$indic_id=as.numeric(as.character(pred$id))%%nb_indicateurs
pred$tags=as.factor(pred$tag_id)
levels(pred$tags) <- tags[as.numeric(as.character(levels(pred$tags)))+1]
setorder(pred,-pred,indic_id)

pred$tags=as.character(pred$tags)

indicateurs_pred=pred[,list(tag1=tags[1],
                            tag2=tags[2],
                            tag3=tags[3],
                            tag4=tags[4],
                            tag5=tags[5],
                            max_prob=max(pred),
                            min_prob=min(pred),
                            sum_prob=sum(pred)),
                      by=c("indic_id")]
sum(is.na(indicateurs_pred$tag3))
indicateurs_pred=merge(indicateurs_pred,data2,by.x="indic_id",by.y="index")
sum(is.na(indicateurs_pred$tag3))


consistency_imp_word=merge(indicateurs_pred,get_imp_ngrams,by.x="indic_id",by.y="i")
setorder(consistency_imp_word,max_prob)
View(consistency_imp_word)
table(c(as.character(indicateurs_pred$tag1),
        as.character(indicateurs_pred$tag2),
        as.character(indicateurs_pred$tag3)))



min_max_prob=indicateurs_pred[which.min(max_prob)]
min_sum_prob=indicateurs_pred[which.min(sum_prob)]


system.time(save(list="indicateurs_pred",file="RData/indicateurs_a_tagger.RData"))
system.time(rdrop2::drop_upload("RData/indicateurs_a_tagger.RData",dtoken = token, mode = "overwrite",path = "RData"))
# names(indicateurs_pred) <- rlang::set_chr_encoding(encoding = "UTF-8",x = names(indicateurs_pred))
nms <- iconv(names(indicateurs_pred), from = "latin1", to = "UTF-8")
names(indicateurs_pred)<-NULL
names(indicateurs_pred) <- nms
Encoding(indicateurs_pred$Indicateur)

fwrite(indicateurs_pred,file="RData/indicateurs_a_tagger.csv",sep="|")

