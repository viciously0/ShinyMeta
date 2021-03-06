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
    menuItem("Introduction & Information", tabName = "II", icon = icon("info")),
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
    tabItem(
      tabName = "II",
      fluidRow(box(tags$div(class="header", checked=NA,
                            list(
                              tags$p("This App prvodies a visual interface to the metafor package for conducting meta-analyses. 
                                     So far, only the meta-analysis of odds ratio has been implemented."),
                              tags$p("The App can use example data, has a tool to generate data and also allows you to upload your own data. 
                                     It provides an overview of the data and calculates a forest plot."),
                              tags$p("If you want to upload your own data, make sure it has the following format: one row per study, one column each for the positive and negative outcomes of the experimental and controle group, as well as one for the author names and one for the year of publication. You will be able to specify the columns after you uploaded your data set.")
                            )
      ),
      
      width = 12, title = "Using this App"
      )),
    
      fluidRow(box(tags$div(class="header", checked=NA,
                            list(
                              tags$p("Information on publication bias and how it can be assessed:", tags$a(href = "http://datacolada.org/58", "datacolada")),
                              tags$p("A convenient tool for learning about p-curves:", tags$a(href = "http://rpsychologist.com/d3/pdist/", "p-curve tool"))
                              
                            )
      ),
      
      width = 12, title = "A collection of resources on meta analyses and publication bias"
      )),
      fluidRow(box(tags$div(class="header", checked=NA,
                            list(
                              tags$article("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. J Stat Softw, 36(3), 1-48."),
                              tags$article("Chang, W. (2015). shinydashboard: Create Dashboards with ‘Shiny’. R package version 0.5, 1."),
                              tags$article("Team, R. (2015). RStudio: integrated development for R. RStudio, Inc., Boston, MA"),
                              tags$article("Team, R. C. (2013). R: A language and environment for statistical computing.")
                              
                            )
      ),
      
      width = 12, title = "Citation"
      ))
    ),
    tabItem(tabName = "dataSel",
            fluidRow(
              column(width = 12,
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
                     ),
                     
                     uiOutput("dataSpec"),
                     
                     if (exists("errorhandling")){
                       uiOutput("errorhandling")
                     }
              )

    )
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
