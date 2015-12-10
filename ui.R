# ui.R
#
# End of study project
# Markowitz allocation based on simulated annealing algorithm
# @author : Arthur Magnien, INSA Lyon
# last updated: June 19, 2014 by Arthur Magnien

library(shiny)

Sys.setenv(TZ="Europe/London")
zones <- attr(as.POSIXlt(Sys.time()), "tzone")
zone <- if (zones[[1]] == "") {
  paste(zones[-1], collapse="/")
} else zones[[1]]

shinyUI(pageWithSidebar(
  headerPanel("PFE"),
  sidebarPanel(
    p(strong(paste0("Current time (", zone, "):")),
      textOutput("currentTime")
    ),
    p(strong(paste0("TRPT :")),
      textOutput("TRPT")
    ),
    p(strong(paste0("Expected return :")),
      textOutput("ExpR")
    ),
    p(strong(paste0("Deviation standard :")),
      textOutput("standDev")
    ),
    p(strong(paste0("Beta :")),
      textOutput("beta")
    ),
    
    #################### Options ###########
    ########################################
    helpText("Calculate the weight of the portfolio based on the expected return chosen by the user"),
    uiOutput("ui"),
    uiOutput("dateUi"),
    textInput("maxit", "Maximum of iteration (1000 - 5000 is generally enough)", "2000"),
    #textInput("numasset", "Number of assets to search in (not working yet)", "4"),
    uiOutput("realTime"),
    uiOutput("realTime2"),
    uiOutput("optimUi"),
    checkboxInput("log", "Plot y axis on log scale", 
                  value = FALSE),
    checkboxInput("autonom", "Let me find the best portfolio myself !", 
                  value = TRUE),
    checkboxInput("realtime", "Let the magic play ! (go real-time)", 
                  value = TRUE),
    checkboxInput("pause", "Emergency checkbox", FALSE),
    actionButton("get", "Calculate allocation")
  ),
  mainPanel(div(class="span12",
    #################### Results ###########
    ########################################
            uiOutput("ploting"),
            uiOutput("markow"),
            textOutput("text1"),
            textOutput("text2"),
            textOutput("text3"),
            textOutput("text4"),
            textOutput("text5"),
            textOutput("text6")
            )
            )
))