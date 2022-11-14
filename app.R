
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
  
  output$contents <- shiny::renderDataTable({
    freq_dat()
  })
  
  output$wdPlot <- renderPlot({
    ggplot(freq_dat(), size=1.6, shape = 'diamond', aes(label = word, size=freq,
                                                        color = factor(sample.int(10, nrow(freq_dat()), replace = TRUE))
    )) +
      geom_text_wordcloud() +scale_size_area(max_size = 16) +
      theme_minimal()
  })
    
    #data table
    output$table <- shiny::renderDataTable({
      freq_dat()
    })
    #section for displaying the story in textual format
    output$rawtext <- renderText({
      
      if(is.null(input$file1)){
        return(d_story) #default story
        
      }else{
        return(readLines(input$file1$datapath)) #display text from uploaded file
      }
    })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)