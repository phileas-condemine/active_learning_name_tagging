library(data.table)
library(dplyr)
library(tidytext)
library(tm)
library(Matrix)


load("RData/tagged_triplet.RData")
load("RData/data2_prep.RData")

# stats_tags=tagged_triplet[,list("frequence"=sum(value)/.N),by="variable"]
# setorder(stats_tags,frequence)

data2$id=paste(data2$Indicateur,data2$Base,sep=" ___ ")
tagged_triplet$id=paste(tagged_triplet$Indicateur,tagged_triplet$Base,sep=" ___ ")

set.seed(2018L)
# remove the data already tagged
data2=data2[!data2$id%in%tagged_triplet$id,]
data2=data.table(data2)
data2=data2[,.SD[1],by="id"]

tags=unique(tagged_triplet$variable)
tags=tags%>%tolower%>%gsub(pattern = ",",replacement = "")


# load("RData/trigram_tidytext.RData")
# load("RData/ngrams_lab_id.RData")
load("RData/match_ngram_id.RData")

library(stringi)
library(stringr)
library("future.apply")
plan(multiprocess) ## Run in parallel on local computer
# keep_ngrams=train_ngrams%>%select(ngram,ngram_num)%>%distinct()%>%arrange(ngram_num)

data2$indic_id=1:nrow(data2)

system.time(
  matching_indic_ngrams <- do.call("rbind",future_sapply(1:nrow(match_ngram_id_indic),
  function(i){
    matching=str_which(pattern = paste0("(^| )(",match_ngram_id_indic$ngram[i],")( |$)"),
                     string = data2$Indicateur_enriched)
    if(length(matching)>0){
      data.frame(i= matching,j=match_ngram_id_indic$ngram_num[i])
    } else NULL
}
))
)


system.time(
  matching_tags_ngrams <- do.call("rbind",sapply(1:nrow(match_ngram_id_tags),
   function(i){
     matching=str_which(pattern = paste0("(^|( ))(",match_ngram_id_tags$ngram[i],")(( )|($))"),
                        string = tags)
     if(length(matching)>0){
       data.frame(i= matching,j=match_ngram_id_tags$ngram_num[i])
     } else NULL
   }
  ))
)


system.time(matching_indic_ngrams_dup <- do.call("rbind",lapply(1:36,function(tag_id){
                                data.frame(matching_indic_ngrams,"tag_id"=tag_id)})))

tags_ngrams_to_add <- matching_indic_ngrams_dup%>%
  select(i,tag_id)%>%distinct()%>%
  merge(matching_tags_ngrams,by.x="tag_id",by.y="i")

matching_indic_ngrams_dup <- rbind(matching_indic_ngrams_dup,tags_ngrams_to_add)

matching_indic_ngrams_dup <- matching_indic_ngrams_dup%>%
  mutate(clef=factor(tag_id*1E+5 +i),id=as.numeric(clef))
#C'est un peu long mais la longueur de id joue beaucoup dans la vitesse de predict xgboost apparemment

resize_ngrams_vec=max(matching_indic_ngrams_dup$j)

system.time(untagged_dtm <- sparseMatrix(i = matching_indic_ngrams_dup$id,
                             j = matching_indic_ngrams_dup$j,
                             x=1,
                             dimnames = list(levels(matching_indic_ngrams_dup$clef),
                                             labels_ngrams[1:resize_ngrams_vec])))



system.time(save(list=c("untagged_dtm","data2","tags"),file="RData/dtm_non_tagged.RData"))

# ON POURRAIT FORTEMENT ACCELERER LE PROCESS EN AJOUTANT "SEULEMENT" LES 'i,j,x=1' pour les nouveaux j apportés par les nouveaux tags !