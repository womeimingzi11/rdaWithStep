#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# To track the development progress, please check:
#
#    https://github.com/womeimingzi11/rdaWithStep
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
    titlePanel("RDA (Redundancy analysis) with Step Selection (WIP yet)"),
    h4(
        'Creator:',
        a(href = "https://womeimingzi11.github.io", 'Han Chen')
    ),
    h5(
        a(href = "mailto://chenhan28@gmail.com", 'chenhan28@gmail.com')
    ),
    h6('Update version: ',
       Sys.Date()),
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
                'rda_scale',
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
                'selection_perm_max',
                'Permutation times of RDA selection (higher may be more stable and accurate, but will take more time)',
                min = 999,
                max = 9999,
                value = 999,
                step = 5000
            ),
            sliderInput(
                'envfit_p_max',
                'Permutation times of individual variable significance detection (higher may be more stable and accurate, but will take more time)',
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
            tabPanel(
                'RDA wihout Selection',
                verbatimTextOutput('rda_full'),
                DTOutput('envfit_full')
            ),
            tabPanel(
                'RDA with Selection',
                verbatimTextOutput('rda_selection'),
                DTOutput('envfit_selection')
            )
            # tabPanel('ENVFIT',
            #          DTOutput('envfit_full'),
            #          DTOutput('envfit_selection'))
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ##############################
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
    #############################
    # Perform RDA without Section
    rct_rda_full <- reactive({
        if (df_com() == "") {
            return('Please Upload Species Matrice')
        }
        if (df_env() == "") {
            return('Please Upload Environment Matrice')
        }
        ## Determine whether scale data or not,
        ## becasuse NA can't be scaled, once select scale,
        ## NA must be omit by na.action = na.omit
        if (input$rda_scale) {
            rda(
                df_com() ~ .,
                data = df_env(),
                na.action = na.omit,
                scale = TRUE
            )
            ## If the data don't have to scale
            ## there is no need to omit NA value
        } else {
            rda(df_com() ~ .,
                data = df_env())
        }
    })
    
    # Reveal the result of RDA without Selection
    output$rda_full <-
        renderPrint({
            rct_rda_full()
        })
    ############################
    # Perform RDA with Selection
    rct_rda_selection <-
        reactive({
            if (df_com() == "") {
                return('Please Upload Species Matrice')
            }
            if (df_env() == "") {
                return('Please Upload Environment Matrice')
            }
            ## If the backwad was selected, there is only need the RDA with all variables
            ## The rct_rda_full() was enought to be selected.
            if (input$select_direction == 'backward') {
                rct_rda_full() %>%
                    ordistep(
                        direction = input$select_direction,
                        perm.max = input$selection_perm_max,
                        trace = 0
                    )
                
                ## For bothward selection and forward selection
                ## We have to create a RDA model with no variable as predict varibale.
                ## y ~ 1 is the formula for null model
                ## ATTENTION: if your full model was scaled,
                ## the same method was also needed to be used at creating null model.
                ## Therefore, we should detect wether scale is needed or not.
            } else {
                if (input$rda_scale) {
                    rda(
                        df_com() ~ 1,
                        data = df_env(),
                        na.action = na.omit,
                        scale = TRUE
                    ) %>%
                        ordistep(
                            scope = formula(rct_rda_full()),
                            direction = input$select_direction,
                            perm.max = input$selection_perm_max,
                            trace = 0
                        )
                } else {
                    rda(df_com() ~ 1,
                        data = df_env()) %>%
                        ordistep(
                            scope = formula(rct_rda_full()),
                            direction = input$select_direction,
                            perm.max = input$selection_perm_max,
                            trace = 0
                        )
                }
            }
        })
    
    # Reveal the result of RDA with Selection
    output$rda_selection <-
        renderPrint({
            rct_rda_selection()
        })
    ############################
    # Perform permutation test
    # to detect the significant
    # environment variables
    
    # Because the envfit can not be output as matrix or data.frame directly,
    # we should convert envfit as data.frame,
    # generally we consider the first two axes of RDA.
    # Therefore, we choose the first two axes of envfit
    # Here, env_obj indicates the result of envfit. In this case, it's the res_envfit.
    # r2_dig is the significant figure of R2
    # p_dig is the significant figure of p value
    envfit_to_df <- function(env_obj,
                             r2_dig = 6,
                             p_dig = 3) {
        r2_fmt <- as.character(paste('%.', r2_dig, 'f', sep = ''))
        p_fmt <- as.character(paste('%.', p_dig, 'f', sep = ''))
        tibble(
            # the name of explainary variables
            factor = names(env_obj$vectors$r),
            # list or vector of R2
            r2 = env_obj$vectors$r,
            # list or vector of p values
            pvals = env_obj$vectors$pvals
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
    }
    
    
    ## ENVFIT to FULL Model
    rct_envfit_full <-
        reactive({
            envfit(formula(rct_rda_full()),
                   data = df_env(),
                   p.max = input$envfit_p_max)
        })
    output$envfit_full <-
        renderDataTable({
            rct_envfit_full() %>%
                envfit_to_df(r2_dig = 3)
        },
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
    ## ENVFIT to SELECTED Model
    rct_envfit_selection <-
        reactive({
            envfit(
                formula(rct_rda_selection()),
                data = df_env(),
                p.max = input$envfit_p_max
            )
        })
    output$envfit_selection <-
        renderDataTable({
            rct_envfit_selection() %>%
                envfit_to_df(r2_dig = 3)
        },
        extensions = 'Buttons',
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
}

# Run the application
shinyApp(ui = ui, server = server)
