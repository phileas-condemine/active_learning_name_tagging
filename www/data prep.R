# encoding="windows-1252"
# encoding="iso-8859-1"


library(data.table)
library(stringi)
library(dplyr)
library(stringr)

# library(rdrop2)
# token <- readRDS("droptoken.rds")
# print(drop_acc(dtoken = token))
# outputDir="man_labelled_data"

##### Liste des indicateurs et tags #####

#sauvegarder en CSV séparé par point-virgule
data2 <- fread("data_original/29032018_Index2.csv",encoding="Latin-1")





data2$V27=NULL
data2[c(!data2[,1]==data2[,21]),c(1,21)]
data2[,21] <- NULL
# data2$source_acronyme=data2$Source%>%
#   stri_extract_all(regex = "(\\()([A-z]+)(\\))")%>%
#   lapply(function(x)paste(x,collapse=" "))%>%
#   unlist
# data2[,c("Source","source_acronyme")]
# 
# data2$producteur_acronyme=data2$Producteur%>%
#   stri_extract_all(regex = "(\\()([A-z]+)(\\))")%>%
#   lapply(function(x)paste(x,collapse=" "))%>%
#   unlist
# data2[,c("Producteur","producteur_acronyme")]

# stop_words = tm::stopwords(kind="fr")
# save(list="stop_words",file="RData/stop_words.RData")
load("RData/stop_words.RData")
stop_words=c(stop_words,"actifs part entière APE")
stop_words=paste0(" ",stop_words," ")
stop_words=c(stop_words," c'"," l'"," d'"," j'"," t'"," m'"," s'")
fix_stop=rep(" ",length(stop_words))
names(fix_stop) <- stop_words





  

data2 <- data2%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.character,tolower)%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces






cardinality=sapply(data2,function(x)length(unique(x)))
data2=data2[,cardinality>1]

data2$Indicateur_enriched=paste(data2$Indicateur,
                                data2$Famille,
                                data2$`Classement producteur Niveau 1 (le moins détaillé)`,
                                data2$`Classement producteur Niveau 2`,
                                data2$`Classement producteur Niveau 3 (le plus détaillé)`,
                                data2$Source,data2$Producteur)

save(list="data2",file="RData/data2_prep.RData")

sapply(data2,function(x)length(unique(x)))

indicateurs=unique(data2$Indicateur)
nchar(indicateurs)%>%hist

tags=fread("data_original/nomenclatures.csv",encoding="Latin-1")
names(tags) <- c("famille","famille_reduced","domaines")
indicateurs_tags_pages=expand.grid(indicateur=indicateurs,famille=tags$famille,pages=1)%>%data.table%>%
  merge(tags%>%dplyr::select(-domaines)%>%data.table,by="famille")
indicateurs_tags_pages$ind=1:nrow(indicateurs_tags_pages)


save(list="indicateurs_tags_pages",file="RData/indicateurs_tags_pages.RData")



##### Données taggées #####

# tagged_old <- fread("data_original/03042018_prediction.csv",encoding="Latin-1")

tagged <- fread("data_original/3105_Index des indicateurs tagges.csv",encoding="Latin-1")

tagged=merge(tagged,data2%>%dplyr::select(index,Base),by="index")


tagged <- tagged%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces

tagged <- tagged%>%mutate(id=paste(Indicateur,Base,sep=" ___ "))

tagged <- tagged%>%dplyr::select(-index,-label,-Base,-Indicateur)

# write.csv(x = names(tagged),file = "data_original/nom_tags_dans_pred.csv")
#manipulation à la main pour matcher les noms des tags
  
matching_tag_names <- fread("data_original/tags_vs_rename_invenis2.csv",encoding="Latin-1")
new_names=matching_tag_names$Indicateur_original
# new_names=new_names[2:length(new_names)]
new_names=c(new_names,"id")
# new_names=new_names[!new_names%in%c("Efficience","Pertinence des soins")]
names(tagged) <- new_names

tagged_triplet=reshape::melt(tagged)
tagged_triplet=cbind(do.call("rbind",strsplit(tagged_triplet$id,split=" ___ ")),tagged_triplet%>%dplyr::select(-id))%>%rename(Indicateur="1",Base="2")
tagged_triplet=data.table(tagged_triplet)

#Récupération des indicateurs déjà taggés à la main

my_files=paste0("man_labelled_data/",list.files("man_labelled_data"))
my_files=my_files[grep(pattern = ".RData",x = my_files,ignore.case = T)]






if ("aggreg_man_lab.RData"%in%list.files("man_labelled_data/")){
  load("man_labelled_data/aggreg_man_lab.RData")
}else{
  rdrop2::drop_download("man_labelled_data/aggreg_man_lab.RData",overwrite = T,
                        dtoken = token,local_path = "man_labelled_data/aggreg_man_lab.RData")
  load("man_labelled_data/aggreg_man_lab.RData")
}






my_files=paste0("man_labelled_data/",list.files("man_labelled_data"))
my_files=my_files[grep(pattern = ".RData",x = my_files,ignore.case = T)]

man_tagged_files=drop_dir("man_labelled_data",dtoken = token)
man_tagged_files=man_tagged_files[grep(pattern = "ind_",x=man_tagged_files$name),]
man_tagged_files$ind <- unlist(str_extract_all(pattern = "(ind_)([:digit:]*)(_time)",
                                               string = man_tagged_files$name))

man_tagged_files$id <- man_tagged_files$ind%>%
  gsub(pattern = "ind_",replacement = "")%>%
  gsub(pattern = "_time",replacement = "")%>%
  as.numeric

man_tagged_files <- man_tagged_files%>%filter(!id%in%aggreg_man_lab$ind)%>%dplyr::select(-id)

if (sum(dim(man_tagged_files))>0){
  man_tagged_files%>%
    data.table()->man_tagged_files
  man_tagged_files <- man_tagged_files[,list(path=sort(path_display,decreasing = T)[1]),by="ind"]
  man_tagged_files$local_path=substr(man_tagged_files$path,2,str_length(man_tagged_files$path))
  man_tagged_files <- man_tagged_files[grep(pattern = ".RData",x = man_tagged_files$local_path,ignore.case = T),]
  
  
  if(nrow(man_tagged_files)>0){
    df_man_tags=do.call("rbind",apply(man_tagged_files[,c("path","local_path","ind")],1,function(path){
      if(!path[2]%in%my_files){
        drop_download(path = path[2],
                      local_path = path[2],
                      verbose = T,overwrite = T,
                      dtoken = token)
      }
      load(path[2])
      df_tag$ind=path[3]
      df_tag
    }))
    df_man_tags <- df_man_tags%>%rename(variable=tag)
    df_man_tags$ind <- df_man_tags$ind%>%
      gsub(pattern = "ind_",replacement = "")%>%
      gsub(pattern = "_time",replacement = "")%>%
      as.numeric
    aggreg_man_lab <- rbind(aggreg_man_lab,df_man_tags)
    save(list="aggreg_man_lab",file="man_labelled_data/aggreg_man_lab.RData")
    rdrop2::drop_upload("man_labelled_data/aggreg_man_lab.RData", 
                        mode = "overwrite",dtoken = token,
                        path = "man_labelled_data")
  }
}


aggreg_man_lab <- aggreg_man_lab%>%filter(!variable%in%c("efficience","pertinence soins"))

tagged_triplet=rbind(tagged_triplet,aggreg_man_lab%>%dplyr::select(-ind))






tagged_triplet <- tagged_triplet%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.factor,as.character)%>%
  mutate_if(is.character,tolower)%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces



save(list="tagged_triplet",file="RData/tagged_triplet.RData")
rdrop2::drop_upload("RData/tagged_triplet.RData", mode = "overwrite",path = "RData",dtoken = token)

