library(xml2)
library(data.table)
library(rvest)
library(stringi)
library(dplyr)
library(httr)
library(stringr)
library(lightgbm)
library(text2vec)

# library(devtools)
# options(devtools.install.args = "--no-multiarch") # if you have 64-bit R only, you can skip this
# install_github("Microsoft/LightGBM", subdir = "R-package")


# load("RData/indicateurs_tags_pages.RData")#from data prep.R
load("RData/tagged_triplet.RData")#from data prep.R
load("RData/data2_prep.RData")

# set.seed(2018L)

tagged_triplet <- merge(tagged_triplet,data2[,c("Indicateur","Indicateur_enriched","Base")],by=c("Indicateur","Base"))


tagged_triplet$id=1:nrow(tagged_triplet)
tagged_triplet$text=paste(tagged_triplet$Indicateur_enriched,tagged_triplet$variable)
all_ids = tagged_triplet$id
indicateurs=tagged_triplet$Indicateur%>%unique
train_ind=sample(indicateurs,size = round(.7*length(indicateurs)))
train_sample=tagged_triplet$Indicateur%in%train_ind
train_ids=tagged_triplet[train_sample,]$id
test_ids = setdiff(all_ids, train_ids)

tagged_triplet <- tagged_triplet%>%select(id,text,value)
setDT(tagged_triplet)
setkey(tagged_triplet, id)

train = tagged_triplet[J(train_ids)]
test = tagged_triplet[J(test_ids)]

# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_train = train$text %>% 
  prep_fun %>% 
  tok_fun %>%
  itoken(ids = train$id,
         progressbar = FALSE)# turn off progressbar because it won't look nice in rmd

stop_words = tm::stopwords(kind="fr")
vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 3L))
pruned_vocab = prune_vocabulary(vocab,doc_count_min = 34*10)
trigram_vectorizer = vocab_vectorizer(vocab)
# create dtm_train with new pruned vocabulary vectorizer
dtm_train  = create_dtm(it_train, trigram_vectorizer)

it_test = test$text %>% 
  prep_fun %>% 
  tok_fun %>% 
  itoken(ids = test$id, 
         progressbar = FALSE)
dtm_test = create_dtm(it_test, trigram_vectorizer)

save(list="trigram_vectorizer",file="RData/trigram_vectorize.RData")
NFOLDS=4

dtrain <- lgb.Dataset(dtm_train, label=train[['value']])
dtest <- lgb.Dataset(dtm_test, label=test[['value']])
# http://lightgbm.readthedocs.io/en/latest/Parameters.html
params <- list(objective="binary", metric="auc")
# lgbmodel <- lgb.cv(params = params, data = dtrain, nrounds = 200, 
                # nfold=NFOLDS, min_data=1, learning_rate=.1, 
                # early_stopping_rounds=10)

system.time(lgbmodel <- lgb.train(params = params, data = dtrain, nrounds = 500, 
                      valids = list("validation"=dtest), min_data=1, learning_rate=.1, 
                   early_stopping_rounds=20,verbose=10,num_leaves = 127))


system.time(lgb.save(booster = lgbmodel,filename = "modeles/lgbmodel_enriched_3gram_eta01_dummy_lgbsave.txt"))

rdrop2::drop_upload("modeles/lgbmodel_enriched_3gram_eta01_dummy_lgbsave.txt")
