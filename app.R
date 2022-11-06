
#word cloud builder project
library(shiny)
library("tm")
library(tidyverse)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggwordcloud")
library("ggplot2")
source("helpers.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Word Cloud Builder App"),
  
  #create an input field to users to select and upload files
  sidebarLayout(
    
    # creating a side bar for selecting files
    sidebarPanel(
      
      #create upload input field
      fileInput("file1", "Select File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      #separator
      hr(),
      
      checkboxInput("showraw", label = "Show original text", value = TRUE),
      
      radioButtons("radio", label = h3("Available Options"),
                   choices = list("Table" = 1, "Word Cloud" = 2), 
                   selected = 1)
      
    ),
    
    #a section for displaying the table with words extracted from the uploaded file
    mainPanel(
      
      conditionalPanel(
        "input.showraw == true", 
        wellPanel(
          textOutput("rawtext")
        )
      ),
      
      #output contents displayed by the table
      conditionalPanel(
        "input.radio == 1", 
        tableOutput("table")
      ), 
      conditionalPanel(
        "input.radio == 2", 
        plotOutput("wdPlot")
      ),
      
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  d_story <- readLines("tortoise and hare.txt")
  
  freq_dat <- reactive({
    #check if file has contents
    if (is.null(input$file1)) {
      df <- word_freq("tortoise and hare.txt")
    }
    else {
      # Read the text in the uploaded file
      df <- word_freq(input$file1$datapath)
    }
    df
  })
  
  output$table <- renderTable({
    freq_dat()
  })
  
  output$wdPlot <- renderPlot({
    ggplot(freq_dat(), size=1.6, shape = 'diamond', aes(label = word, size=freq,
                                                color = factor(sample.int(10, nrow(df), replace = TRUE))
    )) +
      geom_text_wordcloud() +scale_size_area(max_size = 16) +
      theme_minimal()
  })
  
  #section for displaying the story in textual format
  output$rawtext <- renderText({
    
    if(is.null(input$file1)){
      return(d_story) #default story
      
    }else{
      return(input$file1$datapath) #display text from uploaded file
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)