# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(metafor)
library(DT)


# Define server logic
shinyServer(function(input, output) {
  simulate_data = function(n, m, sd, or) {
    # Generate random meta analysis data
    # n: total number of studies to simulate
    # m: mean sample size of studies
    # sd: standard deviation of sample size of studies
    # or:  odds ratio
    
    random_freqs = function(n, M1, M2, OR) {
      # Return a vector of bivariate binary data as a function of odds ratio
      # Implements the inverse function of the calculation of odds ratios from cell probabilities
      # n: total number of observations
      # M1: row marginal probabiliy
      # M2: column marginal probability
      # OR: odds ratio
      # adapted from: https://stats.stackexchange.com/questions/13193/generating-data-with-a-pre-specified-odds-ratio
      
      # find value for p11
      g = function(prob) {
        log(prob) + log(1 - M1 - M2 + prob) - log(M1 - prob) - log(M2 - prob) - log(OR)
      }
      br = c(max(0, M1 + M2 - 1), min(M1, M2))
      p11 = uniroot(g, br)$root # convenient numiercal solution
      
      # fill in other cell probabilities
      p10 = M1 - p11
      p01 = M2 - p11
      p00 = 1 - p11 - p10 - p01
      
      # generate random numbers with corresponding cell probabilities
      x = runif(n)
      n11 = sum(x < p11)
      n10 = sum(x < (p11 + p10)) - n11
      n01 = sum(x < (p11 + p10 + p01)) - n11 - n10
      n00 = n - (n11 + n10 + n01)
      
      freqs = c(n11, n10, n01, n00)
      return(freqs)
    }
    
    # Generate random studies
    # Note: We're assuming a log-normal distribution of the sample sizes here,
    # other distributions would also be possible.
    
    n_studies <- n # total number of studies to simulate
    sample_size_mean <- floor(log(m))  # mean sample size of studies
    sample_size_sd <-
      floor(log(sd)) # standard deviation of sample size of studies
    odds_ratio <- exp(or) # odds ratio
    
    
    # generate sample sizes
    sample_sizes <-
      round(rlnorm(n_studies, meanlog = sample_size_mean, sdlog = sample_size_sd),
            0)
    
    # create emtpy data frame and fill with data
    data <-
      data.frame(
        tpos = NA,
        tneg = NA,
        cpos = NA,
        cneg = NA,
        N = NA
      )
    
    # loopediloop
    for (i in 1:n_studies) {
      data[i, ] <-
        c(random_freqs(sample_sizes[i], 0.5, 0.5, odds_ratio),
          sample_sizes[i])
    }
    
    # add random publication date and author string
    curr_year <-
      as.numeric(format(Sys.Date(), "%Y"))
    data$year <-
      floor(runif(n_studies, min = 1900, max = curr_year))
    data$author <- rep("Random et al.", n_studies)
    return(data)
    
  }
  
  # reactive value and UI for data selection tab
  dataC <- reactive({
    input$dataChoice
  })
  
  output$dataChoiceUI <-  renderUI({
    if (dataC() == "1") {
      # load example data
    } else if (dataC() == "2") {
      div(
        # Input: Select a file ----
        fileInput(
          "file1",
          "Choose CSV File",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(
          "sep",
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),
        
        # Input: Select quotes ----
        radioButtons(
          "quote",
          "Quote",
          choices = c(
            None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = '"'
        )
        
      )
      
      
    } else if (dataC() == "3") {
      div(
        sliderInput("n_studies", "Number of studies:", 4, 20, 12),
        sliderInput(
          "sample_size_mean",
          "Mean sample size of studies:",
          20,
          2000,
          990
        ),
        sliderInput("sample_size_sd", "SD of sample size of studies:", 0, 10, 5),
        sliderInput("odds_ratio", "True odds ratio:", -3, 3, 0, step = 0.1)
        
      )
      
    }
    
  })
  
  
  # generate reactive data set with uploaded file in it / with example data, if not file has been uploaded
  examp.data <- metafor::dat.bcg
  examp.data <- subset(examp.data, alloc == "random")
  examp.data$N <- rowSums(examp.data[, c(4, 5, 6, 7)])
  examp.data <- as.data.frame(examp.data)
  
  values <- reactiveValues(dta.original = examp.data)
  
  observeEvent(input$file1, {
    values$dta.original <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote)
  })
  
  # UI for selection of variables out of the uploaded data set
   output$dataSpec <- renderUI({
      data2 = values$dta.original
      if (dataC() == "2") {
        box(
          title = "Data Specification",
          
          tags$p("Please select the variables from your data set"),
          selectInput("v.ai",
                      "Positive count of treatment group",
                      choices = as.list(names(data2)),
                      selected = "tpos"
          ),
          selectInput("v.bi",
                      "Negative count of treatment group",
                      choices = as.list(names(data2)),
                      selected = "tneg"
          ),
          selectInput("v.ci",
                      "Positive count of controle group",
                      choices = as.list(names(data2)),
                      selected = "cpos"
          ),
          selectInput("v.di",
                      "Negative count of controle group",
                      choices = as.list(names(data2)),
                      selected = "cneg"
          ),
          selectInput("v.author",
                      "Names of the authors",
                      choices = as.list(names(data2)),
                      selected = "author"
          ),
          selectInput("v.year",
                      "Year of publication",
                      choices = as.list(names(data2)),
                      selected = "year"
          )
        )
        
      }
     
    })
 
  # creation of the reactive data set that will be analysed / listens to "load data set" button to refresh reactive data set
   dta <- reactive({
     input$refresh
     isolate({
       if (dataC() == "1") {
         # load example data set and select relevant rows and columns
         data <- metafor::dat.bcg
         data <- subset(data, alloc == "random")
         data$N <- rowSums(data[, c(4, 5, 6, 7)])
         data <- as.data.frame(data)
       } else if (dataC() == "2") {
         
           data <- cbind(values$dta.original[input$v.ai],
                         values$dta.original[input$v.bi],
                         values$dta.original[input$v.ci],
                         values$dta.original[input$v.di],
                         values$dta.original[input$v.author],
                         values$dta.original[input$v.year])
           names(data) <- c("tpos", "tneg", "cpos", "cneg", "author", "year")
           data$N <- rowSums(data[, 1:4])
           data <- as.data.frame(data)
         
         
       } else if (dataC() == "3") {
         simulate_data(
           n = input$n_studies,
           m = input$sample_size_mean,
           sd = input$sample_size_sd,
           or = input$odds_ratio
         )
       } else {
         get(metafor::dat.bcg)
       }
     })
   })
  
  # reactive data table shown in "Data Overview"
  output$dataUI <- DT::renderDataTable({
    dta()
  })
  
  # update message of "load data set" button
  observeEvent(input$refresh, {
    showNotification("Data was updated!", type = "error", duration = 8)
  })

  # calculation and rendering of the forest plot
  output$forest <- renderPlot({

    data = dta()
      
    # Calculate random-effects model
    res <- rma(
      measure = "OR",
      ai = tpos,
      bi = tneg,
      ci = cpos,
      di = cneg,
      data = data,
      slab = paste(author, year, sep = ", ")
    )
    
    # parameter to scale point sizes in regards to sample size
    size <-
      ((data$N - min(data$N)) / (max(data$N) - min(data$N)) * 2) + 1
    
    # plot basic forest plot with random effects model
    forest(
      res,
      xlim = c(-8, 5),
      order = "fit",
      cex = .8,
      ilab = data$N,
      ilab.xpos = -4,
      col = "#D3D3D3",
      psize = size,
      addcred = TRUE
    )
    
    # switch to bold font for headers
    op <- par(font = 2)
    
    # brute force method for size scaling to avoid overlapping of headers
    # (for different number of studies)
    if (nrow(data) <= 13) {
      if (nrow(data) <= 6) {
        n <- (nrow(data) * 1.3)
        
      } else{
        n <- (nrow(data) * 1.2)
      }
    } else{
      n <- (nrow(data) * 1.1)
    }
    
    # add headers
    text(-8, n  , "Author(s) and Year", pos = 4, cex = .8)
    text(5, n , "Observed OR [95% CI]",  pos = 2, cex = .8)
    text(-4, n , "N", cex = .8)
  })
})
