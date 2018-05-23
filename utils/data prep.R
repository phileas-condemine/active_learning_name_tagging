# encoding="windows-1252"
# encoding="iso-8859-1"

library(xml2)
library(data.table)
library(rvest)
library(stringi)
library(dplyr)
library(httr)
library(stringr)



##### Liste des indicateurs et tags #####

#sauvegarder en CSV séparé par point-virgule
# print("fread")
data2 <- fread("data_original/29032018_Index2.csv",encoding="Latin-1")
# print(names(data2))
data2$V27=NULL
data2[c(!data2[,1]==data2[,21]),c(1,21)]
data2[,21] <- NULL
# print("extraction de l'acronyme de la source")
data2$source_acronyme=data2$Source%>%
  stri_extract_all(regex = "(\\()([A-z]+)(\\))")%>%
  lapply(function(x)paste(x,collapse=" "))%>%
  unlist
data2[,c("Source","source_acronyme")]

# print("extraction de l'acronyme du producteur")
data2$producteur_acronyme=data2$Producteur%>%
  stri_extract_all(regex = "(\\()([A-z]+)(\\))")%>%
  lapply(function(x)paste(x,collapse=" "))%>%
  unlist
data2[,c("Producteur","producteur_acronyme")]



# print("modif des caractères spéciaux et suppression des stopwords")
fix=c(#" - "=" ",
  # "\\("="",
  # "\\)"="",
  # "- "=" ",
  # " -"=" ",
  # "-"=" ",
  # "\\\\"="",
  # "/"="",
  # "'"="",
  # ","="",
  # "\\."="",
  "%"="")

fr_stopwords=paste0(" ",tm::stopwords(kind="fr")," ")
fr_stopwords=c(fr_stopwords," actifs part entière APE")
fix_stop=rep(" ",length(fr_stopwords))
names(fix_stop) <- fr_stopwords
# print("application des remplacements/suppressions")
data2 <- data2%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.character,function(x)str_replace_all(x,fix))%>%#suppression manuelle de caractères spéciaux
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,function(x)
    gsub(x=x,pattern = "[[:punct:]]",replacement = ""))%>%#suppresion de la ponctuation
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces
# print("really ?")
cardinality=sapply(data2,function(x)length(unique(x)))
data2=data2[,cardinality>1]
# print("bug ici à cause de l'encodage ?")

data2$Indicateur_enriched=paste(data2$Indicateur,
                                data2$Famille,
                                data2$`Classement producteur Niveau 1 (le moins détaillé)`,
                                data2$`Classement producteur Niveau 2`,
                                data2$`Classement producteur Niveau 3 (le plus détaillé)`,
                                data2$Source,data2$Producteur)
# print("non")

save(list="data2",file="RData/data2_prep.RData")
# print(list.files())
print(list.files("RData"))

sapply(data2,function(x)length(unique(x)))

indicateurs=unique(data2$Indicateur)
nchar(indicateurs)%>%hist

tags=fread("data_original/nomenclatures.csv",encoding="Latin-1")

indicateurs_tags_pages=expand.grid(indicateur=indicateurs,famille=tags$famille,pages=1)%>%data.table%>%
  merge(tags%>%select(-`domaines ou thèmes`)%>%data.table,by="famille")
indicateurs_tags_pages$ind=1:nrow(indicateurs_tags_pages)


save(list="indicateurs_tags_pages",file="RData/indicateurs_tags_pages.RData")



##### Données taggées #####

tagged <- fread("data_original/03042018_prediction.csv",encoding="Latin-1")

tagged <- tagged%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.character,function(x)str_replace_all(x,fix))%>%#suppression manuelle de caractères spéciaux
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,function(x)
    gsub(x=x,pattern = "[[:punct:]]",replacement = ""))%>%#suppresion de la ponctuation
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces

tagged <- tagged%>%mutate(id=paste(Indicateur,Base,sep=" ___ "))

tagged <- tagged%>%select(-index,-label,-Base,-Indicateur)

# write.csv(x = names(tagged),file = "data_original/nom_tags_dans_pred.csv")
#manipulation à la main pour matcher les noms des tags
  
matching_tag_names <- fread("data_original/tags_vs_rename_invenis.csv",encoding="Latin-1")
new_names=matching_tag_names$Indicateur_original
new_names=new_names[2:length(new_names)]
new_names=c(new_names,"id")

names(tagged) <- new_names

tagged_triplet=reshape::melt(tagged)
tagged_triplet=cbind(do.call("rbind",strsplit(tagged_triplet$id,split=" ___ ")),tagged_triplet%>%select(-id))%>%rename(Indicateur="1",Base="2")
tagged_triplet=data.table(tagged_triplet)


#Récupération des indicateurs déjà taggés à la main
man_tagged_files=drop_dir("man_labelled_data")
if (sum(dim(man_tagged_files))>0){
  man_tagged_files$ind=unlist(stringr::str_extract_all(pattern = "(ind_)([:digit:]*)(_time)",string = man_tagged_files$name))
  man_tagged_files%>%
    data.table()->man_tagged_files
  man_tagged_files <- man_tagged_files[,list(path=sort(path_display,decreasing = T)[1]),by="ind"]
  path <- man_tagged_files$path[1]
  man_tagged_files$local_path=substr(man_tagged_files$path,2,str_length(man_tagged_files$path))
  man_tagged_files=man_tagged_files[!man_tagged_files$local_path%in%
                                      paste0("man_labelled_data/",list.files("man_labelled_data/"))]
  if(nrow(man_tagged_files)>0){
  df_man_tags=do.call("rbind",apply(man_tagged_files[,c("path","local_path")],1,function(path){
    drop_download(path[1],local_path = "man_labelled_data/",overwrite = T)
    load(path[2])
    df_tag
  }))
  df_man_tags <- df_man_tags%>%rename(variable=tag)
  tagged_triplet=rbind(tagged_triplet,df_man_tags)
  }
}


save(list="tagged_triplet",file="RData/tagged_triplet.RData")
rdrop2::drop_upload("RData/tagged_triplet.RData", mode = "overwrite",path = "RData")

##### Selection indice des indicateurs taggés ##### 
#il faut enrichir (scraping) en priorité les observations qui ont été taggées !
# indicateurs_tags_index <- indicateurs_tags_pages%>%
#   select("famille","indicateur","ind")%>%mutate_if(is.factor,as.character)%>%data.table
# tagged_triplet <- tagged_triplet%>%mutate_if(is.factor,as.character)
# 
# 
# id_prioritaires <- merge(indicateurs_tags_index%>%data.frame,tagged_triplet%>%data.frame,
#                          by.x=c("famille","indicateur"),by.y=c("variable","Indicateur"),
#                          all.x=F,all.y=F)$ind
# 
# # id_prioritaires=setdiff(id_prioritaires,res_dt$ind)
# save(list="id_prioritaires",file="RData/id_prioritaires.RData")
