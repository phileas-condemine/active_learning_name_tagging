library(data.table)
library(stringi)
library(dplyr)
library(stringr)
library(xgboost)
library(Matrix)

load("RData/dtm_non_tagged.RData")



# load("www/xgbmodel_tidytext_notions.RData")
# 
# 
# system.time(pred <- predict(object=xgbmodel,newdata = untagged_dtm))


if("xgbmodel_tidytext_notions.RData"%in%list.files("www")){
system.time(load("www/xgbmodel_tidytext_notions.RData"))
}else{
  rdrop2::drop_download(path = "www/xgbmodel_tidytext_notions.RData",
                        local_path="www/xgbmodel_tidytext_notions.RData",
                        dtoken = token,overwrite = T)
  system.time(load("www/xgbmodel_tidytext_notions.RData"))
}

system.time(pred <- data.frame(id=untagged_dtm@Dimnames[[1]],pred=predict(object=xgbmodel,newdata = untagged_dtm)))

pred$tag_id=as.numeric(as.character(pred$id))%/%1E+5
pred$indic_id=as.numeric(as.character(pred$id))%%1E+5

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
                                              min_prob=min(pred)),
                                        by=c("indic_id")]


indicateurs_pred=merge(indicateurs_pred,data2,by=c("indic_id"))

min_max_prob=indicateurs_pred[which.min(max_prob)]


save(list="indicateurs_pred",file="RData/indicateurs_a_tagger.RData")

rdrop2::drop_upload("RData/indicateurs_a_tagger.RData",dtoken = token, mode = "overwrite",path = "RData")




