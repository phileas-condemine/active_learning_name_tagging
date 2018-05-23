bootstrapPage(
  theme = shinytheme("cerulean"),
  # includeCSS("style.css"),
  # dropdownButton(
  #   tooltip = tooltipOptions(title = "Choix du theme"), icon = icon("gear"),
  #                                          themeSelector()),
  div(DTOutput("to_tag")),
  selectInput(inputId="tags",label= "tags de l'indicateur",choices = tags_list,multiple = T),
  actionButton("save_tags","cliquer pour enregistrer"),
  actionButton("re_run_lgb","relancer le modèle d'apprentissage")
  # https://dreamrs.github.io/shinyWidgets/index.html
  # selectInput(inputId = "toggle_method",label = "Méthode de labellisation", 
  #             choices = c("Vérification des meilleures prédictions ?",
  #                         "Labellisation des indicateurs mal compris ?")),
  # conditionalPanel(condition='input.toggle_method == "Vérification des meilleures prédictions ?"',
  #             renderDataTable("dt"))
)