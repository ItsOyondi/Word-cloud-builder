#word cloud builder project
library(shiny)
library("tm")
library(tidyverse)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggwordcloud")
library("ggplot2")
library(fresh)
library(qpdf)
source("helpers.R")


set.seed(12500)

custom_colors_theme <- create_theme(
  theme = "default",
  bs_vars_navbar(
    default_bg = "#064273",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#75b8d1",
    default_link_active_bg = "#FFFFFF",
    default_link_hover_color = "#aaa"
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    title = "WORD CLOUD BUILDER",
    tags$style(type = 'text/css', '.navbar {
                           font-family: Century Gothic;
                           font-size: 13px;}',
               
               '.navbar-dropdown { 
                           font-family: Century Gothic;
                           font-size: 13px;}'
               
    ),
    header = tagList(
      use_theme(custom_colors_theme) #add background color
    ),
     tabPanel("Preprocessing",
              # Application title
              titlePanel(h1("Text Analysis App", align = "center", style = "font-family: 'Century Gothic', serif;
    font-weight: 500; font-size: 50px; text-shadow: 3px 3px 3px #aaa; line-height: 1; 
     color: #404040;")),
              
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
                  checkboxInput("with_stopwords", label = "Include stop words?", value = TRUE),
                  numericInput("max_size", label = "Maximum Font Size (controls word cloud size)", value = 20, min = 1, step = 1),
                  hr(),
                  
                  textInput("excludes", "Text To Exclude (NB: no spaces between each word)", placeholder = "e.g. word1,word2,word3"),
                  hr(),
                  
                  radioButtons(
                    "stories", "Default Stories: ",
                    c("Hare and Tortoise", "Computer History", "Comic Con","Netflix", "Bitcoin"),
                    selected = "Hare and Tortoise",
                    inline = TRUE
                  ),
                  hr(),
                  
                  radioButtons(
                    "shapes", "Choose word cloud shape: ",
                    c("circle", "cardioid", "diamond",
                      "square", "triangle-upright",
                      "pentagon"),
                    selected = "diamond",
                    inline = TRUE
                  ),
                  hr(),
                  textInput("keywords", "Enter word to search in the word cloud"),
                  #submitButton("Search", icon("refresh")),
                  verbatimTextOutput("value"),
                  hr(),
                  radioButtons("radio", label = h3("Available Options"),
                               choices = list("Table" = 1, "Word Cloud" = 2, 
                                              "Bar Chart" = 3), 
                               selected = 1),
                  hr(),
                  checkboxInput("top_words", label = "Bar chart: Top 10 most used words", value = TRUE),
                  checkboxInput("btm_words", label = "Bar chart: Least used words", value = FALSE),
                  
                  
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
                    shiny::dataTableOutput("contents"),
                  ), 
                  conditionalPanel(
                    "input.radio == 2", 
                    plotOutput("wdPlot")
                  ),
                  conditionalPanel(
                    "input.radio == 3", 
                    plotOutput("bchart")
                  ),
                  #wellPanel(
                    #h4("Words read from the word cloud"),
                    #shiny::dataTableOutput("txt_from_viz")
                #  ),
                  
                  
                )
              )
     ),
     navbarMenu("More Options",
                tabPanel("Table",
                         shiny::dataTableOutput("table")
                )
     )
  )
  
  
)

# Define server logic
server <- function(input, output) {
  d_story <- readLines("tortoise and hare.txt",warn=FALSE)
  d_story1 <- readLines("netflix.txt", warn=FALSE)
  d_story2 <- readLines("computer history.txt", warn=FALSE)
  d_story3 <- readLines("comicon.txt", warn=FALSE)
  d_story4 <- readLines("Bitcoin.txt", warn=FALSE)
  
  
  freq_dat <- reactive({
    #get all the excluded words
    wd_excluded <- unlist(strsplit(input$excludes, ",")
    )
    print(wd_excluded)
    #check if file has contents
    if (is.null(input$file1)) {
      
      #check the default story selected
      if(input$stories == "Hare and Tortoise"){
        df <- word_freq("tortoise and hare.txt", input$with_stopwords, wd_excluded)
      }else if(input$stories == "Computer History"){
        df <- word_freq("computer history.txt", input$with_stopwords, wd_excluded)
      }else if(input$stories == "Comic Con"){
        df <- word_freq("comicon.txt", input$with_stopwords, wd_excluded)
      }else if(input$stories == "Netflix"){
        df <- word_freq("netflix.txt", input$with_stopwords, wd_excluded)
      }else if(input$stories == "Bitcoin"){
        df <- word_freq("Bitcoin.txt", input$with_stopwords, wd_excluded)
      }
      
    }
    else {
      # Read the text in the uploaded file
      df <- word_freq(input$file1$datapath, input$with_stopwords)
    }
    return(df)
  })
  
  
  
  output$contents <- shiny::renderDataTable({
    freq_dat()
  })
  
  output$value <- renderPrint({ 
    
    #check if input word exists in the data frame
    dt <- freq_dat()
    
    if(input$keywords %in% dt$word){
      #get the freq value
      val <- dt[input$keywords == dt$word, ]$freq
      return(val)
    }else{
      return("Word not found")
    }
    
  })
  
  output$wdPlot <- renderPlot({
    
      set.seed(100)
      
        print(ggplot(freq_dat(), size=1.6, aes(label = word, size=freq,
                                                               color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = input$shapes) +scale_size_area(max_size = input$max_size) +
          theme_minimal()
        )
    
  })
  
    
    
    output$bchart <- renderPlot({
      #words adjusted at angle 45 degrees
      #the chart shows top words first
      #guides fill=false, no need for highlighting the word and colors, user can see top words first.
      #create a data frame with sliced dataset
      dt <- freq_dat()
      #check if user wonts top or lest used words
      if(input$top_words == TRUE){
        df_t <- dt[c(1:10), c("word", "freq")] #select the first 10 words with highest frequency
      }
      #check if user wonts the least used rrds
      else if(input$btm_words == TRUE){
        df_t <- tail(dt, n=10) #select the firsr 10 words with highest frequency
      }else{
        #plot chart with all words if user not specified
       df_t <- dt
      }
      ggplot(df_t, aes(y=reorder(word, freq), x=freq)) + geom_bar(stat="identity")+
        xlab("Frequency") + 
        ylab("Word") + 
        theme(axis.text.y=element_text(size = 14))
    })
    
    
    
    #data table
    output$table <- shiny::renderDataTable({
      freq_dat()
    })
    #section for displaying the story in textual format
    output$rawtext <- renderText({
      
      if(is.null(input$file1)){
       
        #check the default story selected
        if(input$stories == "Hare and Tortoise"){
          return(d_story) 
        }else if(input$stories == "Netflix"){
          return(d_story1) 
        }else if(input$stories == "Computer History"){
          return(d_story2) 
        }else if(input$stories == "Comic Con"){
          return(d_story3) 
        }else if(input$stories == "Bitcoin"){
          return(d_story4) 
        }
        
      }else{
        return(readLines(input$file1$datapath)) #display text from uploaded file
      }
    })
    
    
    #show text read from visualization
    #output$txt_from_viz <- shiny::renderDataTable({ 
     # txt = get_word_in_cloud("viz.png")
     # return(txt)
   # })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
