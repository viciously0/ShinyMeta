#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(metafor)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Shiny R Meta"),
  dashboardSidebar(sidebarMenu(
    menuItem("Data Selection", tabName = "dataSel", icon = icon("file")),
    menuItem(
      "Data Overview",
      tabName = "dataOvr",
      icon = icon("list-alt", lib = "glyphicon")
    ),
    menuItem(
      "Meta Analysis",
      tabName = "MetaAna",
      icon = icon("bar-chart")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "dataSel",
            fluidRow(
              box(
                title = "Data Selection",
                radioButtons(
                  "dataChoice",
                  label = h3("Radio buttons"),
                  choices = list(
                    "Example Data" = 1,
                    "Upload Data" = 2,
                    "Simulate Data" = 3
                  ),
                  selected = 1
                ),
                uiOutput("dataChoiceUI"),
                actionButton("refresh", "Load Data Set")
              )
            ),
            fluidRow(box(tags$div(class="header", checked=NA,
                                  list(
                                    tags$p("Information on publication bias and how it can be assessed:", tags$a(href = "http://datacolada.org/58", "datacolada")),
                                    tags$p("A convenient tool for learning about p-curves:", tags$a(href = "http://rpsychologist.com/d3/pdist/", "p-curve tool"))
                                    
                                  )
            ),
            
            width = 12, title = "A collection of resources on meta analyses and publication bias"
            ))
            
            
            ),
    
    tabItem(
      tabName = "dataOvr",
      # fluidRow(
      # box(title = "Data Overview",
      #     uiOutput("dataUI"),
      #   width = NULL))
      div(style = 'overflow-x: scroll', DT::dataTableOutput("dataUI"))
    ),
    tabItem(tabName = "MetaAna",
            fluidRow(
              box(
                width = 12,
                title = "Forest plot using random effects model",
                plotOutput(outputId = "forest")
              )
              
              
            ))
  ))
)