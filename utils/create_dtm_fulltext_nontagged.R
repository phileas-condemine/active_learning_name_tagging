library(data.table)
library(dplyr)
library(text2vec)


load("RData/tagged_triplet.RData")
load("RData/data2_prep.RData")

stats_tags=tagged_triplet[,list("frequence"=sum(value)/.N),by="variable"]
setorder(stats_tags,frequence)

data2$id=paste(data2$Indicateur,data2$Base,sep=" ___ ")
tagged_triplet$id=paste(tagged_triplet$Indicateur,tagged_triplet$Base,sep=" ___ ")

set.seed(2018L)
# remove the data already tagged
data2=data2[!data2$id%in%tagged_triplet$id,]
data2=data.table(data2)
data2=data2[,.SD[1],by="id"]

tags=unique(tagged_triplet$variable)


untagged=expand.grid(id=paste(data2$Indicateur,data2$Base,sep=" ___ "),tag=tags)
untagged <- mutate_all(untagged,as.character)
untagged=cbind(untagged,do.call("rbind",strsplit(untagged$id,split = " ___ ")))
untagged <- untagged%>%rename(Indicateur="1",Base="2")%>%select(-id)%>%mutate_if(is.factor,as.character)
untagged=merge(untagged%>%data.table,
               data2%>%select(Indicateur,Base,Indicateur_enriched)%>%data.table,
               by=c("Indicateur","Base"))#,allow.cartesian=TRUE
untagged$text=paste(untagged$Indicateur_enriched,untagged$tag)

load("RData/trigram_vectorize.RData")
untagged$ind=1:nrow(untagged)
# prep_fun = tolower
# tok_fun = word_tokenizer

print("run tolower tokenize and trigram vectorizer")

# N_WORKERS = 4
# if(require(doParallel)) registerDoParallel(N_WORKERS)
# # splits = split_into(untagged$text, N_WORKERS)
# it = itoken_parallel(iterable =  untagged$text,preprocessor = tolower,ids = untagged$ind, tokenizer = word_tokenizer, n_chunks = N_WORKERS)
# print(system.time(dtm <-  create_dtm(it, trigram_vectorizer, type = 'dgTMatrix')))

print(system.time(dtm <- untagged$text %>%
  prep_fun %>%
  tok_fun%>%
  itoken(ids = untagged$ind,progressbar = F)%>%
  create_dtm(trigram_vectorizer)
  ))
print("save the dtm")
system.time(save(list=c("untagged","dtm"),file="RData/dtm_non_tagged.RData"))

