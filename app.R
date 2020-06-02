#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Package for Shiny
library(shiny)
library(DT)
library(shinythemes)

# Package for data manipulation
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    theme = shinytheme('flatly'),
    titlePanel("RDA (Redundancy analysis) with step selection"),
    h4('WIP yet'),
    p('What is RDA with step selection?'),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput('df_com',
                      'Please upload Species Matrix'),
            fileInput('df_env',
                      'Please uploda Environment Matrix')
        ),
        mainPanel(tabsetPanel(
            tabPanel(
                'Species & Environment Matrix',
                DTOutput('df_com'),
                DTOutput('df_env')
            )
        ))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    df_com <- reactive({
        if (is.null(input$df_com)) {
            return("")
        } else {
            read_csv(input$df_com$datapath)
        }
    })
    df_env <- reactive({
        if (is.null(input$df_env)) {
            return("")
        } else {
            read_csv(input$df_env$datapath)
        }
    })
    output$df_com <- renderDataTable({
        if (df_com() == "") {
            tribble( ~ spe_A, ~ spe_B,
                     1, 2,
                     3, 4)
        } else {
            df_com()
        }
    })
    output$df_env <- renderDataTable({
        if (df_env() == "") {
            tribble( ~ env_A, ~ env_B,
                     1, 2,
                     3, 4)
        } else {
            df_env()
        }
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
