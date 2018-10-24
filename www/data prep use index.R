# encoding="windows-1252"
# encoding="iso-8859-1"
# A RELANCER EN ENTIER SI ON AJOUTE DES NOTIONS OU DES TAGS OU DES INDICATEURS TAGGED AVEC DES NOUVEAUX NGRAMS
# devtools::install_github("mattflor/chorddiag")
library(data.table)
library(stringi)
library(dplyr)
library(stringr)
library(text2vec)
library(readxl)
library(Matrix)
library(text2vec)
library(rdrop2)
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
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
  mutate_if(is.character,tolower)%>%#en minuscules
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
mat_cor=cor(tagged[,3:36])

# tags=names(tagged)[3:36]
# plotly::plot_ly(type="heatmap",z=~mat_cor,x=~tags,y=~tags,source="heatplot")
# devtools::install_github("mattflor/chorddiag")
# library(chorddiag)
# mat_cor_nodiag=as(mat_cor-Matrix::Diagonal(n = 34),"matrix")
# mat_cor_nodiag=abs(mat_cor_nodiag)
# th=.1
# mat_cor_nodiag[mat_cor_nodiag<th] <- 0
# chrdiag <- chorddiag::chorddiag(mat_cor_nodiag
#                      #,type="bipartite"
#                      ,showTicks=F
#                      )
# htmlwidgets::saveWidget(widget = chrdiag,
#                         file="correlation_valeurabsolue_chorddiag_seuil10pct.html",
#                         selfcontained = T,title = "chord-diag corr tags")

tagged=merge(tagged,data2%>%dplyr::select(index,Base),by="index")


tagged <- tagged%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces

# tagged <- tagged%>%mutate(id=paste(Indicateur,Base,sep=" ___ "))
tagged$index=as.character(tagged$index)
mapping=tagged%>%select(index,Base,Indicateur)

tagged <- tagged%>%dplyr::select(-label,-Base,-Indicateur)
# write.csv(x = names(tagged),file = "data_original/nom_tags_dans_pred.csv")
#manipulation à la main pour matcher les noms des tags

matching_tag_names <- fread("data_original/tags_vs_rename_invenis2.csv",encoding="Latin-1")
new_names=matching_tag_names$Indicateur_original
# new_names=new_names[2:length(new_names)]
# new_names=c(new_names,"id")
new_names=c("id",new_names)

# new_names=new_names[!new_names%in%c("Efficience","Pertinence des soins")]
names(tagged) <- new_names

tagged_triplet=reshape::melt(tagged)
# tagged_triplet=cbind(do.call("rbind",strsplit(tagged_triplet$id,split=" ___ ")),tagged_triplet%>%dplyr::select(-id))%>%rename(Indicateur="1",Base="2")
tagged_triplet=merge(tagged_triplet,mapping,by.x="id",by.y="index")
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

tagged_triplet=rbind(tagged_triplet,aggreg_man_lab%>%rename(id=ind))

tagged_triplet$id=as.numeric(tagged_triplet$id)

tagged_triplet <- tagged_triplet%>%
  mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.factor,as.character)%>%
  mutate_if(is.character,tolower)%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces

tags <- unique(tagged_triplet$variable)
tagged_triplet=merge(tagged_triplet,data.frame(tags=tags,tag_id=1:length(tags)),by.x="variable",by.y="tags")

tagged_triplet <- tagged_triplet%>%mutate(i=(tag_id-1)*20000+id)%>%select(-tag_id)


save(list="tagged_triplet",file="RData/tagged_triplet.RData")
rdrop2::drop_upload("RData/tagged_triplet.RData", mode = "overwrite",path = "RData",dtoken = token)


tags_dtm <- tags%>%
  word_tokenizer()%>%
  itoken()%>%{create_dtm(vocab_vectorizer(create_vocabulary(.,ngram=c(1L,3L))),it=.)}


#### STEMMING AVEC SNOWBALLC
# library(SnowballC)
# library(tidytext)
# system.time(stemmed_text <- data2%>%
#   select(Indicateur_enriched,index)%>%
#   unnest_tokens(text_w,Indicateur_enriched)%>%
#   mutate(text_w=wordStem(text_w,language="french"))%>%
#   group_by(index)%>%
#   summarise(text_stemmed=paste(text_w,collapse = " ")))
# data2=merge(data2,stemmed_text,by="index")
# 
# # ATTENTION ICI ON REMPLACE LE TEXT PAR SA VERSION STEMMED
# data2 <- data2%>%mutate(text_original=Indicateur_enriched,
#                         Indicateur_enriched=text_stemmed)


# text <- stemmed_text$text_stemmed

# STEMMING => mauvais résultats, on va partir sur de la lemmatisation
# http://www.bernhardlearns.com/2017/04/cleaning-words-with-r-stemming.html
# install.packages("koRpus")

text <- data2$Indicateur_enriched
# text <- iconv(text, "latin1", "UTF-8")


# ICI ON ENLEVE LES ACCENTS ?
# Encoding(text)
text=iconv(text,from="UTF-8",to="ASCII//TRANSLIT")


# on ajoute "azerty" pour servir de séparateur d'indicateurs 
# les caractères spéciaux même rares comme | sont déjà présents dans le txt 
fwrite(x = list(paste(text,"azerty")),"indicateurs.txt")
# enregistrement UTF8 avec blocnote ?
readLines("indicateurs.txt",10)
# AU PARAVANT IL FAUT AVOIR INSTALLED TREETAGGER ET AJOUTE LES VARS D'ENVIRONNEMENT
# system.time(tagged <- system("tag-french indicateurs.txt",intern = T))
# system.time(tagged <- system("tag-french indicateurs.txt",intern = T)) # CA PREND 2 MINUTES !?
# fwrite(list(tagged),"indicateurs_tagtreed.txt")
system.time(system("tag-french indicateurs.txt > indicateurs_tagtreed2.txt")) # CA PREND 12 SECONDES !

tagged_parsed=fread("indicateurs_tagtreed2.txt",fill=T,header=F)

# tagged_parsed=fread(input = "indicateurs_tagtreed.txt",sep="\t",fill=T,header=F)
names(tagged_parsed) <- c("mot_original","type_de_mot","mot_lemme")
head(tagged_parsed,100)
tail(tagged_parsed,100)

tagged_parsed=tagged_parsed[-nrow(tagged_parsed),]# on supprime la dernière ligne qui est bizarre.

# classification des séparateurs "azerty" 
tagged_parsed[grep("azerty",tagged_parsed$mot_original),]$type_de_mot%>%table

tagged_parsed[grep("card",tagged_parsed$mot_original),]
tagged_parsed%>%filter(mot_lemme=="@card@")

table(tagged_parsed$type_de_mot)

data.table(tagged_parsed)[,.SD[1:10],by="type_de_mot"]%>%View

# IL FAUT VERIFIER CES DIFFERENTES CLASSIFICATIONS, APPAREMMENT <NA> et SENT peuvent sauter.

# on enlève la ponctuation

ajustement_text=tagged_parsed%>%
  filter(!type_de_mot%in%c("PUN","SENT"))%>%
  mutate(text_ajuste=ifelse(type_de_mot=="NUM",mot_original,mot_lemme))

big_txt=paste(ajustement_text$text_ajuste,collapse=" ")
split_txt=strsplit(big_txt,split = "azerty")
# head(split_txt)
split_txt=iconv(unlist(split_txt),'UTF-8','latin1')
# head(split_txt)
tokens = word_tokenizer(split_txt)
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it, ngram = c(1L, 3L))
vocab = prune_vocabulary(vocab, term_count_min = 3L)
# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)
dtm=create_dtm(it,vectorizer,type = "dgCMatrix")
dtm=as(dtm, "ngCMatrix")
# on veut retrouver j plutôt que fucking p
i=summary(dtm)$i
j=summary(dtm)$j
dimnm=dimnames(dtm)

i_tags=summary(tags_dtm)$i
j_tags=summary(tags_dtm)$j
dimnm_tags=dimnames(tags_dtm)

###################################################################
############### AJOUT HEURISTIQUE A PARTIR DES NOTIONS ############
###################################################################
notions <- readxl::read_xlsx("data_original/20180426_Dictionnaire des notions edited.xlsx")
names(notions)[!names(notions)%in%tags]
notions <- lapply(1:ncol(notions),function(i){
  data.frame(notion=names(notions)[i],termes=unname(notions[,i]))%>%
    na.omit%>%
    mutate_all(as.character)
}
)%>%
  do.call(what = "rbind")%>%
  mutate(termes=tolower(termes))

notions_txt=notions$termes
notions_txt=iconv(notions_txt,from="UTF-8",to="ASCII//TRANSLIT")

fwrite(x = list(paste(notions_txt,"azerty")),"notions_2B_lem.txt")
# enregistrement UTF8 avec blocnote ?
readLines("notions_2B_lem.txt",10,encoding = "utf8")
system.time(notions_lemmed <- system("tag-french notions_2B_lem.txt",intern = T))
fwrite(list(notions_lemmed),"notions_lemmatized.txt")
notions_lemmatized=fread(input = "notions_lemmatized.txt",sep="\t",fill=T,header=F)
names(notions_lemmatized) <- c("mot_original","type_de_mot","mot_lemme")
notions_lemmatized=notions_lemmatized[1:(nrow(notions_lemmatized)-3)]
notions_lemmatized=notions_lemmatized%>%
  filter(!type_de_mot%in%c("PUN","SENT"))%>%
  mutate(text_ajuste=ifelse(type_de_mot=="NUM",mot_original,mot_lemme))

notions_lemmatized=paste(notions_lemmatized$text_ajuste,collapse=" ")
notions_lemmatized=unlist(strsplit(notions_lemmatized,split = " azerty "))
notions_lemmatized=gsub(" azerty","",notions_lemmatized)
# head(split_txt)
# notions_lemmatized=iconv(unlist(notions_lemmatized),'UTF-8','latin1')

# notions_nchar=nchar(notions_lemmatized)
# notions_lemmatized=substr(x = notions_lemmatized,start = 2,stop = notions_nchar-1)

notions$termes=notions_lemmatized
notions=unique(notions)


notions$tag_id=sapply(notions$notion,function(x)which(tags==x))
nb_ngram_indic=max(j)
nb_features=max(j)+max(j_tags)

system.time(match_notions_indic <- do.call("rbind",pbapply::pbsapply(1:nrow(notions),
         function(index){
           matching=str_which(pattern = paste0("(^|( ))(",notions$termes[index],")(( )|($))"),
                              string = split_txt)#text
           if(length(matching)>0){
             data.frame(i= matching,
                        tag_id=notions$tag_id[index],
                        j=nb_features+1)
           } else NULL
         }
)))
# i => ligne = numéro de l'indicateur récup avec split_txt
# j => colonne = numéro du token dans la sparse matrix
# tag_id => tag associé
# n'oubliez pas qu'on fera un modèle binomial !

match_notions_indic <- unique(match_notions_indic)
head(match_notions_indic)


save(list=c("i","j","dimnm","i_tags","j_tags","dimnm_tags","match_notions_indic","tags","text"),file="RData/dtm_prep.RData")
