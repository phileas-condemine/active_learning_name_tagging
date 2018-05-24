options("httr_oauth_cache" = TRUE)
options(shiny.trace = TRUE)
library(httpuv)
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
# https://shiny.rstudio.com/articles/debugging.html
# cat(file=stderr(), ..., "\n")
# print(Sys.getlocale())
Sys.setlocale(category = "LC_ALL",locale = "en_US.UTF-8")
# print(Sys.getlocale())


print("test httpuv")
print(httpuv::decodeURIComponent("https://github.com/rstudio/shiny/issues/1971"))
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
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
# print(drop_acc(dtoken = token))
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
  # rdrop2::drop_download("RData/tagged_triplet.RData",overwrite = T,
                        # dtoken = token,local_path = "RData")
  load("RData/tagged_triplet.RData")
}
tags_list=unique(tagged_triplet$variable)

if ("indicateurs_a_tagger.RData"%in%list.files("RData/")){
  load("RData/indicateurs_a_tagger.RData")
}else{
  # rdrop2::drop_download("RData/indicateurs_a_tagger.RData",
                        # overwrite = T,local_path = "RData",dtoken = token)
  load("RData/indicateurs_a_tagger.RData")
}

# print(head(indicateurs_pred))


vars=c("Base","Indicateur","tag1","tag2","tag3","Famille",
       "Classement producteur Niveau 1 (le moins détaillé)",
       "Classement producteur Niveau 2",
       "Classement producteur Niveau 3 (le plus détaillé)","Source","Producteur")

print(vars)

# print(head(indicateurs_pred[,vars,with=F]))

clean_date_to_save=c(" "="_",":"="-")

# session_ <- devtools::session_info()[[2]]

