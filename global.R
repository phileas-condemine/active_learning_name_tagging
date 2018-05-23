options("httr_oauth_cache" = TRUE)

library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(shinythemes)
library(shinycssloaders)
library(stringr)
library(shinyWidgets)
library(rdrop2)
library(utf8)
library(rlang)
# Sys.setlocale(category = "LC_ALL", locale = "fr_FR.utf8")
# https://github.com/karthik/rdrop2#accessing-dropbox-on-shiny-and-remote-servers
# token <- drop_auth()
# saveRDS(token, "droptoken.rds")

# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
print("start here")
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
print("opened rdrop acc")
outputDir="man_labelled_data"

source("utils/dropdownButton.R")
source("utils/themeSelector.R")


# if(!"man_labelled_data"%in%list.files()){
#   shell("mkdir man_labelled_data")
# }

# print("re-run data prep with new tagged data ~60sec")
# print(system.time(source("utils/data prep.R",encoding = "utf-8")))
# print("run gbm ~60sec")
# print(system.time(source("utils/run_xgboost.R",encoding = "utf-8")))
# print("create full dtm with new vocab ~170sec")
# print(system.time(source("utils/create_dtm_fulltext_nontagged.R",encoding = "utf-8")))
# print("run prediction on full dtm ~60sec")
# print(system.time(source("utils/prediction.R",encoding = "utf-8")))



if ("tagged_triplet.RData"%in%list.files("RData/")){
  load("RData/tagged_triplet.RData")
}else{
  print("dl tagged triplet")
  rdrop2::drop_download("RData/tagged_triplet.RData",overwrite = T,
                        dtoken = token,local_path = "RData")
  print("done")
  print(list.files("RData"))
  load("RData/tagged_triplet.RData")
}
tags_list=unique(tagged_triplet$variable)

if ("indicateurs_a_tagger.RData"%in%list.files("RData/")){
  load("RData/indicateurs_a_tagger.RData")
}else{
  print("dl indicateurs à tagger")
  rdrop2::drop_download("RData/indicateurs_a_tagger.RData",overwrite = T,local_path = "RData")
  print("done")
  load("RData/indicateurs_a_tagger.RData")
}

print("déclaration de variable avec accent")
vars=c("Base","Indicateur","tag1","tag2","tag3","Famille",
       "Classement producteur Niveau 1 (le moins détaillé)",
       "Classement producteur Niveau 2",
       "Classement producteur Niveau 3 (le plus détaillé)","Source","Producteur")
print(vars)
clean_date_to_save=c(" "="_",":"="-")
