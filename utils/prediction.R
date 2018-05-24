library(xml2)
library(data.table)
library(rvest)
library(stringi)
library(dplyr)
library(httr)
library(stringr)
library(text2vec)
library(glmnet)
library(xgboost)

load("RData/tagged_triplet.RData")

load("RData/dtm_non_tagged.RData")




if("xgbmodel_enriched_3gram_eta01_depth8_dummy.RData"%in%list.files("modeles")){
system.time(load("modeles/xgbmodel_enriched_3gram_eta01_depth8_dummy.RData"))
}else{
  # rdrop2::drop_download("modeles/xgbmodel_enriched_3gram_eta01_depth8_dummy.RData",dtoken = token,overwrite = T,local_path = "modeles")
  system.time(load("modeles/xgbmodel_enriched_3gram_eta01_depth8_dummy.RData"))
}

system.time(pred <- predict(object=xgbmodel,newdata = dtm))

untagged$prediction=pred
rm(dtm,xgbmodel)
gc()
untagged=data.table(untagged)
setorder(untagged,-prediction,Indicateur)

indicateurs_pred=untagged[,list(tag1=tag[1],
                                              tag2=tag[2],
                                              tag3=tag[3],
                                              max_prob=max(prediction),
                                              min_prob=min(prediction)),
                                        by=c("Indicateur","Base")]
stats_tags
untagged%>%
  filter(tag=="Enfants, adolescents, jeunes adultes")%>%
  arrange(-prediction)%>%
  select(text,prediction)%>%
  head

load("RData/data2_prep.RData")

indicateurs_pred=merge(indicateurs_pred,data2,by=c("Indicateur","Base"))


sum(indicateurs_pred$max_prob==indicateurs_pred$min_prob)

quantile(indicateurs_pred$max_prob-indicateurs_pred$min_prob)

table(indicateurs_pred$tag1)
table(indicateurs_pred$tag2)
table(indicateurs_pred$tag3)

table(indicateurs_pred$tag1)

min_max_prob=indicateurs_pred[which.min(max_prob)]

indicateurs_pred[which.min(max_prob-min_prob)]

indicateurs_pred[which.min(max_prob/min_prob)]

save(list="indicateurs_pred",file="RData/indicateurs_a_tagger.RData")

# rdrop2::drop_upload("RData/indicateurs_a_tagger.RData",dtoken = token, mode = "overwrite",path = "RData")

