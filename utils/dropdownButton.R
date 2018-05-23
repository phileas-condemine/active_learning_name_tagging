dropdownButton <- function (..., circle = TRUE, status = "default", size = "default", 
                            icon = NULL, label = NULL, tooltip = FALSE, right = FALSE, 
                            up = FALSE, width = NULL, inputId = NULL) 
{
  size <- match.arg(arg = size, choices = c("default", "lg", 
                                            "sm", "xs"))
  if (is.null(inputId)) {
    inputId <- paste0("drop", sample.int(1e+09, 1))
  }
  html_ul <- list(class = paste("dropdown-menu", ifelse(right, 
                                                        "dropdown-menu-right", "")), class = "dropdown-shinyWidgets", 
                  id = paste("dropdown-menu", inputId, sep = "-"), style = if (!is.null(width)) paste0("width: ", 
                                                                                                       htmltools::validateCssUnit(width), ";"), `aria-labelledby` = inputId, 
                  lapply(X = list(...), FUN = htmltools::tags$li, style = "margin-left: 10px; margin-right: 10px;"))
  if (circle) {
    html_button <- circleButton(inputId = inputId, icon = icon, 
                                status = status, size = size, class = "dropdown-toggle", 
                                `data-toggle` = "dropdown")
  }
  else {
    html_button <- list(class = paste0("btn btn-", status, 
                                       " action-button dropdown-toggle "), class = if (size != 
                                                                                       "default") paste0("btn-", size), type = "button", 
                        id = inputId, `data-toggle` = "dropdown", `aria-haspopup` = "true", 
                        `aria-expanded` = "true", list(icon, label), tags$span(class = "caret"))
    html_button <- do.call(htmltools::tags$button, html_button)
  }
  if (identical(tooltip, TRUE)) 
    tooltip <- tooltipOptions(title = label)
  if (!is.null(tooltip) && !identical(tooltip, FALSE)) {
    tooltip <- lapply(tooltip, function(x) {
      if (identical(x, TRUE)) 
        "true"
      else if (identical(x, FALSE)) 
        "false"
      else x
    })
    tooltipJs <- htmltools::tags$script(sprintf("$('#%s').tooltip({ placement: '%s', title: '%s', html: %s, trigger: 'hover' });", 
                                                inputId, tooltip$placement, tooltip$title, tooltip$html))
  }
  else {
    tooltipJs <- ""
  }
  dropdownTag <- htmltools::tags$div(class = ifelse(up, "dropup", 
                                                    "dropdown"), html_button, id = paste("dropdown", inputId, 
                                                                                         sep = "-"), do.call(htmltools::tags$ul, html_ul), tooltipJs, 
                                     tags$script(sprintf("dropBtn('#%s', %s);", paste("dropdown", 
                                                                                      inputId, sep = "-"), "true")))
  attachShinyWidgetsDep(dropdownTag, "dropdown")
}
environment(dropdownButton) <- asNamespace('shinyWidgets')
