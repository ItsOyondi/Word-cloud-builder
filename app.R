
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
              titlePanel(h1("Word Cloud Builder App", align = "center", style = "font-family: 'Century Gothic', serif;
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
                  radioButtons("radio", label = h3("Available Options"),
                               choices = list("Table" = 1, "Word Cloud" = 2), 
                               selected = 1),
                  
                  
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
  d_story <- readLines("tortoise and hare.txt")
  d_story1 <- readLines("netflix.txt")
  d_story2 <- readLines("computer history.txt")
  d_story3 <- readLines("comicon.txt")
  d_story4 <- readLines("Bitcoin.txt")
  
  freq_dat <- reactive({
    #check if file has contents
    if (is.null(input$file1)) {
      
      #check the default story selected
      if(input$stories == "Hare and Tortoise"){
        df <- word_freq("tortoise and hare.txt", input$with_stopwords)
      }else if(input$stories == "Computer History"){
        df <- word_freq("computer history.txt", input$with_stopwords)
      }else if(input$stories == "Comic Con"){
        df <- word_freq("comicon.txt", input$with_stopwords)
      }else if(input$stories == "Netflix"){
        df <- word_freq("netflix.txt", input$with_stopwords)
      }else if(input$stories == "Bitcoin"){
        df <- word_freq("Bitcoin.txt", input$with_stopwords)
      }
      
    }
    else {
      # Read the text in the uploaded file
      df <- word_freq(input$file1$datapath, input$with_stopwords)
    }
    df
  })
  
  output$contents <- shiny::renderDataTable({
    freq_dat()
  })
  
  output$wdPlot <- renderPlot({
    for (shape in c(
      "circle", "cardioid", "diamond",
      "square", "triangle-upright",
      "pentagon"
    )) {
      set.seed(100)
      
      if(input$shapes == "circle"){
        shape = "circle"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                               color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }else if(input$shapes == "cardioid"){
        shape = "cardioid"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                              color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }else if(input$shapes == "diamond"){
        shape = "diamond"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                              color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }else if(input$shapes == "square"){
        shape = "square"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                              color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }else if(input$shapes == "triangle-upright"){
        shape = "triangle-upright"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                              color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }else if(input$shapes == "pentagon"){
        shape = "pentagon"
        print(ggplot(freq_dat(), size=1.6, shape = shape, aes(label = word, size=freq,
                                                              color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
        )) +
          geom_text_wordcloud_area(shape = shape) +scale_size_area(max_size = 16) +
          theme_minimal())
      }
      
    }
    
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
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
