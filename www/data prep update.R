library(data.table)
library(stringi)
library(dplyr)
library(stringr)
options("scipen"=100, "digits"=10) # GESTION DE LA CONVERSION DES NUMERICS EN CHARACTER SANS UTILISER LA NOTATION SCIENTIFIQUE !!!


load("RData/tagged_triplet.RData")
my_files=paste0("man_labelled_data/",list.files("man_labelled_data"))
my_files=my_files[grep(pattern = ".RData",x = my_files,ignore.case = T)]



if ("aggreg_man_lab.RData"%in%list.files("man_labelled_data/")){
  load("man_labelled_data/aggreg_man_lab.RData")
}else{
  rdrop2::drop_download(path="man_labelled_data/aggreg_man_lab.RData",
                        overwrite = T,
                        dtoken = token,
                        local_path = "man_labelled_data/aggreg_man_lab.RData")
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

man_tagged_files <- man_tagged_files%>%
  filter(!id%in%aggreg_man_lab$ind)%>%
  dplyr::select(-id)
if (sum(nrow(man_tagged_files))>0){
  man_tagged_files%>%
    data.table()->man_tagged_files
  man_tagged_files <- man_tagged_files[,list(path=sort(path_display,decreasing = T)[1]),by="ind"]
  man_tagged_files$local_path=substr(man_tagged_files$path,2,str_length(man_tagged_files$path))
  man_tagged_files <- man_tagged_files[grep(pattern = ".RData",x = man_tagged_files$local_path,ignore.case = T),]
  
  
  if(nrow(man_tagged_files)>0){
    
    df_man_tags=do.call("rbind",
      apply(man_tagged_files[,c("path","local_path","ind")],
            1,
            function(path){
              # message(path)
      if(!path[2]%in%my_files){
        drop_download(path = path[2],
                      local_path = path[2],
                      verbose = T,overwrite = T,
                      dtoken = token)
      }
      print(path[2])
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


aggreg_man_lab <- aggreg_man_lab%>%
  rename(id=ind)%>%
  filter(!variable%in%c("efficience","pertinence soins"))%>%
  filter(!id%in%tagged_triplet$id)

# aggreg_man_lab$id %in% tagged_triplet$id

tags <- unique(tagged_triplet$variable)
aggreg_man_lab=merge(aggreg_man_lab,
                     data.frame(tags=tags,tag_id=1:length(tags)),
                     by.x="variable",by.y="tags")
aggreg_man_lab <- aggreg_man_lab%>%mutate(i=(tag_id-1)*20000+id)%>%select(-tag_id)

tagged_triplet=rbind(tagged_triplet,aggreg_man_lab)

tagged_triplet$id=as.numeric(tagged_triplet$id)




tagged_triplet <- tagged_triplet%>%
  # mutate(Indicateur=as.character(Indicateur))%>%#passage en char
  mutate_if(is.factor,as.character)%>%
  # mutate_if(is.character,tolower)%>%#suppression de stopwords génériques et spécifiques
  # mutate_if(is.character,function(x)str_replace_all(x,fix_stop))%>%#suppression de stopwords génériques et spécifiques
  # mutate_if(is.character,tm::stripWhitespace)#suppression des doubles espaces



save(list="tagged_triplet",file="RData/tagged_triplet.RData")
rdrop2::drop_upload("RData/tagged_triplet.RData", mode = "overwrite",path = "RData",dtoken = token)

