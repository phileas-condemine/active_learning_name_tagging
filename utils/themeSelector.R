themeSelector <- function () 
{
  shiny::fixedPanel(top = "40px", right = "40px", draggable = TRUE, 
                    style = "width: 250px; z-index: 100000;", div(class = "panel panel-danger", 
                                                                  style = "box-shadow: 5px 5px 15px -5px rgba(0, 0, 0, 0.3);", 
                                                                  div(class = "panel-heading", "Select theme:"), div(class = "panel-body", 
                                                                                                                     selectInput("shinytheme-selector", NULL, c("default", 
                                                                                                                                                                allThemes()), selectize = FALSE))), tags$script("$('#shinytheme-selector')\n  .on('change', function(el) {\n    var allThemes = $(this).find('option').map(function() {\n      if ($(this).val() === 'default')\n        return 'bootstrap';\n      else\n        return $(this).val();\n    });\n\n    // Find the current theme\n    var curTheme = el.target.value;\n    if (curTheme === 'default') {\n      curTheme = 'bootstrap';\n      curThemePath = 'shared/bootstrap/css/bootstrap.min.css';\n    } else {\n      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';\n    }\n\n    // Find the <link> element with that has the bootstrap.css\n    var $link = $('link').filter(function() {\n      var theme = $(this).attr('href');\n      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');\n      return $.inArray(theme, allThemes) !== -1;\n    });\n\n    // Set it to the correct path\n    $link.attr('href', curThemePath);\n  });"))
}

environment(themeSelector) <- asNamespace('shinythemes')
