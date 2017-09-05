library(shiny)
library(RWeka)
library(tm) 
library(stringi)
library(dplyr)
library(stringr)
library(qdap)
library(SnowballC)
library(RColorBrewer)
library(data.table)
library(RSQLite)
library(sqldf)

# import the necessary data, database connection and functions
source("utils.R")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        tags$div(class="navbar-header", tags$h1("Next Word Prediction App", style = "color: green")),
        
        textInput("userText", "Please enter the text for predicting the next word", value = ''),
        
        
        sliderInput("maxWords", "Choose the number of possible next words to display", min = 1, max = 10, value = 5),
        helpText("This app predicts the next possible words using methods of Natural Language Processing 
                 using a database of publicly available text data. The predictions are made using databases of 
                 N-grams and the Stupid Backoff model for calculating the scores. In brief this model takes from
                 one to three last words from the input and matches 4-grams followed by 3-grams and 2-grams depending
                 on how many words were submitted as input. The next words predicted by a higher N-gram have a 
                 higher score and priority for output. This feature stems from the intuition that longer N-grams tend to
                 capture the potential meaning of the text.")
        ),
      
      # Show predicted words
      mainPanel(
        
        fluidRow(column(12,
                        tags$div(
                        tags$h1(href="#", class="btn btn-primary btn-lg btn-block", "Next Word predictions for your input using Natural Language Processing models:")              
                        ))),
        tags$br(),
        
        fluidRow(
          
          
          column(12, 
                 
          tags$div(class="panel panel-primary", tags$div(class="panel-heading", tags$h1("Text input", class="panel-title")),
                          tags$div(class="panel-body", 
                                   h2(textOutput("value"))
                          ))
          )),
        
         fluidRow(
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", 
                                                                tags$h1("Next Word with Stupid Back-Off model - small database", class="panel-title")),
                          tags$p("This panel contains predictions based on 200000 lines of text (40000 from blogs, 40000 from news and 
120000 from Twitter) without filtering words and N-grams occurring once. The swear words and those lines containing non-English characters were removed.", class="panel-body"),
                          
                          
                          uiOutput("nbest")
                          )
                 
                 
          
                          
          
          ),
          
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", 
                          tags$h1("Next Word with Stupid Back-Off model - large database", class="panel-title")),
                          tags$p("This panel contains predictions based on 600000 lines of text (120000 from blogs, 120000 from news and 
360000 from Twitter) with words and N-grams occurring once filtered out. The swear words and those lines containing non-English characters were removed.", class="panel-body"),
                          
                          uiOutput("nbest2")
                          )
                 )

          )
        
        
        )
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output the user input
  output$value <- renderText({input$userText})
  
  # perform predictions
  


  output$predictions2 <- renderTable({
    if(input$userText == ""){
      return(NULL)}
    
    data <- data.frame(Next_Word = predictNextWord2(input$userText, input$maxWords))
    data
  })

  # smaller database predictions   
  output$nbest <- renderUI({
             if(input$userText == ""){return(NULL)}
    
             nbest <- predictNextWord(input$userText, input$maxWords)
            
             
            if(input$maxWords == 1){
              
              tags$div(class="panel-body", align = "center", actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"))
              
            } else if(input$maxWords == 2){
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success")
              )
              
              
            } else if(input$maxWords == 3){
              
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success")
              )
              
            } else if(input$maxWords == 4){

              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success")
              )
              
              
            } else if(input$maxWords == 5){
            
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success")
              )
              
            } else if(input$maxWords == 6){
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success"),
                       actionButton("nbest6", label=nbest[6], class="btn btn-success")
              )
              
            } else if(input$maxWords == 7){
              
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success"),
                       actionButton("nbest6", label=nbest[6], class="btn btn-success"),
                       actionButton("nbest7", label=nbest[7], class="btn btn-success")
              )
            } else if(input$maxWords == 8){

              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success"),
                       actionButton("nbest6", label=nbest[6], class="btn btn-success"),
                       actionButton("nbest7", label=nbest[7], class="btn btn-success"),
                       actionButton("nbest8", label=nbest[8], class="btn btn-success")
              )
              
            } else if(input$maxWords == 9){
              
              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success"),
                       actionButton("nbest6", label=nbest[6], class="btn btn-success"),
                       actionButton("nbest7", label=nbest[7], class="btn btn-success"),
                       actionButton("nbest8", label=nbest[8], class="btn btn-success"),
                       actionButton("nbest9", label=nbest[9], class="btn btn-success")
              )
              
                            
            } else if(input$maxWords == 10){

              tags$div(class="panel-body", align = "center",
                       actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
                       actionButton("nbest2", label=nbest[2], class="btn btn-success"),
                       actionButton("nbest3", label=nbest[3], class="btn btn-success"),
                       actionButton("nbest4", label=nbest[4], class="btn btn-success"),
                       actionButton("nbest5", label=nbest[5], class="btn btn-success"),
                       actionButton("nbest6", label=nbest[6], class="btn btn-success"),
                       actionButton("nbest7", label=nbest[7], class="btn btn-success"),
                       actionButton("nbest8", label=nbest[8], class="btn btn-success"),
                       actionButton("nbest9", label=nbest[9], class="btn btn-success"),
                       actionButton("nbest10", label=nbest[10], class="btn btn-success")
              )
              
            }

 })
  
# larger database predictions and button generation
  
  output$nbest2 <- renderUI({
    if(input$userText == ""){return(NULL)}
    
    nbest <- predictNextWord2(input$userText, input$maxWords)
    
    
    if(input$maxWords == 1){
      
      tags$div(class="panel-body", align = "center", actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"))
      
    } else if(input$maxWords == 2){
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success")
      )
      
      
    } else if(input$maxWords == 3){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success")
      )
      
    } else if(input$maxWords == 4){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success")
      )
      
      
    } else if(input$maxWords == 5){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success")
      )
      
    } else if(input$maxWords == 6){
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success"),
               actionButton("nbest6", label=nbest[6], class="btn btn-success")
      )
      
    } else if(input$maxWords == 7){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success"),
               actionButton("nbest6", label=nbest[6], class="btn btn-success"),
               actionButton("nbest7", label=nbest[7], class="btn btn-success")
      )
    } else if(input$maxWords == 8){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success"),
               actionButton("nbest6", label=nbest[6], class="btn btn-success"),
               actionButton("nbest7", label=nbest[7], class="btn btn-success"),
               actionButton("nbest8", label=nbest[8], class="btn btn-success")
      )
      
    } else if(input$maxWords == 9){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success"),
               actionButton("nbest6", label=nbest[6], class="btn btn-success"),
               actionButton("nbest7", label=nbest[7], class="btn btn-success"),
               actionButton("nbest8", label=nbest[8], class="btn btn-success"),
               actionButton("nbest9", label=nbest[9], class="btn btn-success")
      )
      
      
    } else if(input$maxWords == 10){
      
      tags$div(class="panel-body", align = "center",
               actionButton("NextWord", label=nbest[1],class="btn btn-primary btn-lg"),
               actionButton("nbest2", label=nbest[2], class="btn btn-success"),
               actionButton("nbest3", label=nbest[3], class="btn btn-success"),
               actionButton("nbest4", label=nbest[4], class="btn btn-success"),
               actionButton("nbest5", label=nbest[5], class="btn btn-success"),
               actionButton("nbest6", label=nbest[6], class="btn btn-success"),
               actionButton("nbest7", label=nbest[7], class="btn btn-success"),
               actionButton("nbest8", label=nbest[8], class="btn btn-success"),
               actionButton("nbest9", label=nbest[9], class="btn btn-success"),
               actionButton("nbest10", label=nbest[10], class="btn btn-success")
      )
      
    }

  })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

