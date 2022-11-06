
#word cloud builder project
library(shiny)
library("tm")
library(tidyverse)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggwordcloud")
library("ggplot2")
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
      
      
      radioButtons("radio", label = h3("Available Options"),
                   choices = list("Table" = 1, "Word Cloud" = 2), 
                   selected = 1)
      
    ),
    
    #a section for displaying the table with words extracted from the uploaded file
    mainPanel(
      
      #ouput contents displayed by the table
      tableOutput("contents"),
      plotOutput("wdPlot"),
      textOutput("rawtext")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  d_story <- readLines("tortoise and hare.txt")
  
  
  output$contents <- renderTable({
    outs()
    
  })
  output$wdPlot <- renderPlot({
    outs()
  })
  outs <- reactive({
    #check if file has contents
    if (is.null(input$file1)) {
      text = readLines("tortoise and hare.txt")
    }
    else {
      # Read the text in the uploaded file
      text = readLines(input$file1$datapath)
    }
    
    #start preprocessing
    # Load all the data as a corpus
    docs <- Corpus(VectorSource(text))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    df <- data.frame(word = names(v),freq=v) #create a data frame from the uploaded file.
    
    #create a wordcloud from the data frame
    #used withVisible function to avoid the coerce to df error
    plot1 <- ggplot(df, size=1.6, shape = 'diamond', aes(label = word, size=freq,
                            color = factor(sample.int(10, nrow(df), replace = TRUE))
                            
    )) +
      geom_text_wordcloud() +scale_size_area(max_size = 16) +
      theme_minimal()
    
    #display datafrma and the word clouds depending on user selection
    if(input$radio == 1){
      return(df) #return the data frame
      
    }else{
      #plot the word cloud if word cloud is selected
      return(plot1)
    }
  })
  
  #section for displaying the story in textual format
  output$rawtext <- renderText({
    
    if(is.null(input$file1)){
      return(d_story) #default story
      
    }else{
      return(text) #display text from uploaded file
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)