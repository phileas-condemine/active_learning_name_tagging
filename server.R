function(input, output, session){
  # indicateurs_pred_react <- reactiveVal()#init reactive value
  # indicateurs_pred_react(indicateurs_pred)#update value
  # ind_to_tag <- reactive(indicateurs_pred_react()[which.min(max_prob)])
  my_reactives <- reactiveValues(ind_done=c(),indicateurs_pred_react=indicateurs_pred)
  ind_to_tag <- reactive(my_reactives$indicateurs_pred_react[which.min(max_prob)])
  output$to_tag <- renderDT(data.table(ind_to_tag()[,vars,with=F]),
                            options=list(searching = FALSE,paging = FALSE),rownames= FALSE)

  observeEvent(input$save_tags,{
    ind_tagged=ind_to_tag()
    given_tags=input$tags
    vec_tag=tags_list%in%given_tags*1
    names(vec_tag) <- tags_list
    df_tag <- data.frame(vec_tag)
    df_tag$tag=rownames(df_tag)
    df_tag <- df_tag%>%rename(value=vec_tag)
    df_tag$Base=ind_tagged$Base
    df_tag$Indicateur=ind_tagged$Indicateur
    rownames(df_tag) <- NULL
    # https://shiny.rstudio.com/articles/persistent-data-storage.html#local
    filePath <- paste0("man_labelled_data/","ind_",ind_tagged$ind,"_time_",
                       Sys.time()%>%as.character%>%stringr::str_replace_all(clean_date_to_save),".RData")
    save(df_tag,file=filePath)
    rdrop2::drop_upload(filePath, path = outputDir, mode = "overwrite")
    my_reactives$ind_done=c(my_reactives$ind_done,ind_tagged$ind)
    print(paste("just tagged ind :",my_reactives$ind_done,collapse = " "))
    print(nrow(my_reactives$indicateurs_pred_react))
    my_reactives$indicateurs_pred_react=indicateurs_pred[!indicateurs_pred$ind%in%my_reactives$ind_done]#update value
    updateSelectInput(session = session,inputId="tags",selected = "")
  })
  
  observeEvent(input$re_run_lgb,{
    # rerun des modeles
    print("re-run data prep with new tagged data ~60sec")
    print(system.time(source("utils/data prep.R",encoding = "utf-8")))
    print("run gbm ~60sec")
    print(system.time(source("utils/run_xgboost.R",encoding = "utf-8")))
    print("create full dtm with new vocab ~170sec")
    print(system.time(source("utils/create_dtm_fulltext_nontagged.R",encoding = "utf-8")))
    print("run prediction on full dtm ~60sec")
    print(system.time(source("utils/prediction.R",encoding = "utf-8")))
    print("done")
    
    
    
    })
  }