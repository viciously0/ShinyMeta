library(shiny)
ui <- fluidPage(
  fluidRow(
    column(4,
           wellPanel(
             selectizeInput(inputId= "invar", label= "Select Variable", 
                            choices= names(iris), 
                            selected= names(iris)[1],
                            multiple=T),
             uiOutput("moreControls"))),
    
    
    mainPanel(
      tableOutput("tab")
    )
  ))
server <- function(input, output) {
  
  sorted <-  reactive({
    data <- iris[ ,c(input$invar)]
    #print(input$invar)
    data})
  
  output$moreControls <- renderUI({
    numvar<- length(input$invar)
    lapply(1:numvar, function(i) {
      tagList(
        selectInput("inv",paste0("Please Select Probability Distribution of ", input$invar[i]),
                    choices = c("Normal","Uniform","Triangular","Log Normal")),
        conditionalPanel(condition = "input.inv=='Normal'",
                         textInput("invarpdfmean","Please Select Input Variable Mean:",0.25),
                         textInput("invarpdfsd","Please Select Input Variable Standard Deviation", 0.02)),
        conditionalPanel(condition = "input.inv=='Uniform'",
                         textInput("invarpdfmin","Please Select Minimum Input Variable Value:",0.18),
                         textInput("invarpdfmax","Please Select Maximum Input Variable Value", 0.3)),
        conditionalPanel(condition = "input.inv=='Triangular'",
                         textInput("invarpdfmin","Please Select Minimum Input Variable Value:",0.18),
                         textInput("invarpdfmax","Please Select Maximum Input Variable Value:", 0.3)),
        conditionalPanel(condition = "input.inv=='Log Normal'",
                         textInput("invarpdfmeanlog","Please Select Mean Log of Input Variable:",0.18),
                         textInput("invarpdfsdlog","Please Select Standard Deviation Log of Input Variable:", 0.3))
        
      )})})
}





shinyApp(ui, server)

