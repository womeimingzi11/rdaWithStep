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
library(vegan)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    theme = shinytheme('flatly'),
    titlePanel("RDA (Redundancy analysis) with step selection"),
    h4('WIP yet'),
    p('What is RDA with step selection?'),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p(
                'The species and environment matrice must be formatted as the right side.'
            ),
            fileInput('df_com',
                      'Please upload Species Matrix'),
            fileInput('df_env',
                      'Please uploda Environment Matrix'),
            selectInput(
                'full_scale',
                'Do you want to scale the matrice?
                If you select TRUE, the observation with missing value in matrice will be removed.',
                choices = c(TRUE, FALSE)
            ),
            selectInput(
                'select_direction',
                'The mode of stepwise search',
                choices = c("both", "backward", "forward"),
                selected = 'backward'
            ),
            sliderInput(
                'select_perm_max',
                'Permutation times (higher may be more stable and accurate, but will take more time)',
                min = 999,
                max = 9999,
                value = 999,
                step = 5000
            )
            # selectInput(
            #     'select_trace',
            #     'Do you want to check the information during the model building? (Where the 0 means show the final model only)',
            #     choices = c(TRUE, FALSE),
            #     selected = 0
            # )
        ),
        mainPanel(tabsetPanel(
            tabPanel(
                'Species & Environment Matrix',
                DTOutput('df_com'),
                DTOutput('df_env')
            ),
            tabPanel('RDA wihout Selection',
                     verbatimTextOutput('rda_full'),
                     DTOutput('envfit_full')),
            tabPanel('RDA with Selection',
                     verbatimTextOutput('rda_selection'))
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Reveal the data frame secton
    
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
            tribble(~ spe_A, ~ spe_B,
                    1, 2,
                    3, 4)
        } else {
            df_com()
        }
    })
    output$df_env <- renderDataTable({
        if (df_env() == "") {
            tribble(~ env_A, ~ env_B,
                    1, 2,
                    3, 4)
        } else {
            df_env()
        }
    })
    
    # Perform RDA analysis Section
    
    rct_rda_full <- reactive({
        if (df_com() == "") {
            return('Please Upload Species Matrice')
        }
        if (df_env() == "") {
            return('Please Upload Environment Matrice')
        }
        
        if (input$full_scale) {
            rda(
                df_com() ~ .,
                data = df_env(),
                na.action = na.omit,
                scale = TRUE
            )
        } else {
            rda(df_com() ~ .,
                data = df_env())
        }
    })
    
    rct_envfit_full <- 
        reactive({
            if (df_com() == "") {
                return('Please Upload Species Matrice')
            }
            if (df_env() == "") {
                return('Please Upload Environment Matrice')
            }
            envfit(df_com(), df_env(), na.rm = input$full_scale)
        })

    # Here, env_obj indicates the result of envfit. In this case, it's the res_envfit.
    # r2_dig is the significant figure of R2
    # p_dig is the significant figure of p value
    rct_envfit_to_df_full <-
        reactive({
            r2_fmt <- as.character(paste('%.', 6, 'f', sep = ''))
            p_fmt <- as.character(paste('%.', 3, 'f', sep = ''))
            
            tibble(
                # the name of explainary variables
                factor = names(rct_envfit_full()$vectors$r),
                # list or vector of R2
                r2 = rct_envfit_full()$vectors$r,
                # list or vector of p values
                pvals = rct_envfit_full()$vectors$pvals
            ) %>%
                # generate significant levels by p values
                mutate(sig = case_when(
                    pvals <= 0.001 ~ '***',
                    pvals <= 0.01 ~ '**',
                    pvals <= 0.05 ~ '*',
                    TRUE ~ ' '
                )) %>%
                # format the significant figure by format definition before.
                mutate(pvals = sprintf('%.3f', pvals),
                       r2 = sprintf(r2_fmt, r2))
            
        })
        
    output$rda_full <-
        renderPrint({
            rct_rda_full()
        })
    output$envfit_full <-
        renderPrint({
            rct_envfit_to_df_full
        })
    
    rct_rda_selection <-
        reactive({
            if (df_com() == "") {
                return('Please Upload Species Matrice')
            }
            if (df_env() == "") {
                return('Please Upload Environment Matrice')
            }
            if (input$select_direction == 'backward') {
                rct_rda_full() %>%
                    ordistep(direction = input$select_direction,
                             perm.max = input$select_perm_max,
                             trace = 0)
            } else {
                return('WIP')
            }
        })
    
    output$rda_selection <-
        renderPrint({
            rct_rda_selection()
        })
}

# Run the application
shinyApp(ui = ui, server = server)
