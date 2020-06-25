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

# Package for Shiny
library(shiny)
library(shinythemes)
library(DT)

# Package for data manipulation
library(tidyverse)
library(vegan)
library(ggvegan)

ui <- fluidPage(# Application title
  theme = shinytheme('flatly'),
  navbarPage(
    'rdaWithStep',
    tabPanel(
      'Overview',
      fluidRow(column(3,
                      h4(
                        'Creator:',
                        a(href = "https://womeimingzi11.github.io", 'Han Chen')
                      )),
               column(3,
                      h5(
                        a(href = "mailto://chenhan28@gmail.com", 'chenhan28@gmail.com')
                      )),
               column(6,
                      h6(
                        'Update version: 20600625'
                      )),),
      includeMarkdown('resource/page/overview.md'),
    ),
    tabPanel('Analysis',
             sidebarLayout(
               sidebarPanel(
                 p('The species and environment matrice must be formatted as the demo.'),
                 radioButtons(
                   'data_source',
                   'Upload files or try the demo',
                   choices = c('Upload files' = 'file',
                               'Try the demo' = 'demo'),
                   selected = 'demo'
                 ),
                 conditionalPanel(
                   condition = "input.data_source == 'file'",
                   fileInput('df_com',
                             'Please upload Species Matrix'),
                   fileInput('df_env',
                             'Please uploda Environment Matrix')
                 ),
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
                 ),
                 selectInput(
                   'axes_explain',
                   'Show the explainiation power by each axes?',
                   choices = c(TRUE, FALSE),
                   selected = TRUE
                 )
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
                   DTOutput('envfit_full'),
                   fluidRow(
                     column(
                       3,
                       selectInput(
                         'dl_format_full',
                         'Choose the figure format (PDF is recommanded)',
                         choices = c('pdf', 'png', 'jpeg'),
                         selected = 'pdf'
                       ),
                       selectInput(
                         'dl_dpi_full',
                         'Choose the DPI (300 DPI is recomanded). PDF is a vector diagram, DPI is not needed for it',
                         choices = c(
                           '320' = 'retina',
                           '300' = 'print',
                           '72' = 'screen'
                         ),
                         selected = 'print'
                       ),
                       downloadButton('dl_rda_full',
                                      'Download Figure')
                     ),
                     column(6,
                            plotOutput('fig_rda_full'))
                   )
                 ),
                 tabPanel(
                   'RDA with Selection',
                   verbatimTextOutput('rda_selection'),
                   DTOutput('envfit_selection'),
                   fluidRow(
                     column(
                       3,
                       selectInput(
                         'dl_format_selection',
                         'Choose the figure format (PDF is recommanded)',
                         choices = c('pdf', 'png', 'jpeg'),
                         selected = 'pdf'
                       ),
                       selectInput(
                         'dl_dpi_selection',
                         'Choose the DPI (300 DPI is recomanded). PDF is a vector diagram, DPI is not needed for it',
                         choices = c(
                           '320' = 'retina',
                           '300' = 'print',
                           '72' = 'screen'
                         ),
                         selected = 'print'
                       ),
                       downloadButton('dl_rda_selection',
                                      'Download Figure')
                     ),
                     column(6,
                            plotOutput('fig_rda_selection'))
                   )
                 )
               ))
             )),
    tabPanel(
      'Acknowledgements & References',
      includeMarkdown('resource/page/acknowledgements.md')
    )
  ))

server <- function(input, output) {
  ##############################
  # Reveal the data frame secton
  df_com <- reactive({
    if (input$data_source == 'demo') {
      read_csv('resource/data/df_com_smp.csv')
    } else {
      if (is.null(input$df_com)) {
        return("")
      } else {
        read_csv(input$df_com$datapath)
      }
    }
  })
  
  df_env <- reactive({
    if (input$data_source == 'demo') {
      read_csv('resource/data/df_env_smp.csv')
    } else {
      if (is.null(input$df_env)) {
        return("")
      } else {
        read_csv(input$df_env$datapath)
      }
    }
  })
  
  output$df_com <- renderDataTable({
    df_com()
  })
  output$df_env <- renderDataTable({
    df_env()
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
  ## Load envfit_to_df function
  # source('Function/envfit_to_df.R')
  
  ## ENVFIT to FULL Model
  rct_envfit_full <-
    reactive({
      envfit(formula(rct_rda_full()),
             data = df_env(),
             p.max = input$envfit_p_max) %>%
        envfit_to_df(r2_dig = 3)
    })
  
  output$envfit_full <-
    renderDataTable({
      rct_envfit_full()
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
      ) %>%
        envfit_to_df(r2_dig = 3)
    })
  output$envfit_selection <-
    renderDataTable({
      rct_envfit_selection()
    },
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  ############################
  #Plot the figure of RDAs
  ## Load ggRDA function
  # source('Function/ggRDA.R')
  ## Plot the RDA figures
  rct_fig_rda_full <-
    reactive({
      p <-
        ggRDA(rda_obj = rct_rda_full(),
              envfit_df = rct_envfit_full(),
              sp_size = 5) +
        # Generally theme_classic is a good choice to paint a figure
        theme_classic() +
        # In general, we don't need to show the legend in RDA figure
        theme(legend.position = "none") +
        # scale_XXXXX_manual series provide the ability
        # to define the style of legend by variable value
        scale_size_manual(values = c('ns' = .6,
                                     'sig' = .8)) +
        # Q: What's species here? I don't remember their is a significant level which is called 'species'
        # A: Indeed, their is no significant 'species'. However,
        # the species name in RDA which is generated from geom_text contains colour attribution.
        scale_colour_manual(values = c(
          'ns' = '#606060',
          'sig' = 'black',
          'species' = 'red'
        )) +
        scale_linetype_manual(values = c('ns' = 8, 'sig' = 1))
      if (input$axes_explain) {
        exp_by_x <-
          (as.list(rct_rda_full()$CCA$eig)$RDA1) / (rct_rda_full()$tot.chi) * 100
        exp_by_y <-
          (as.list(rct_rda_full()$CCA$eig)$RDA2) / (rct_rda_full()$tot.chi) * 100
        p +
          xlab(paste('RDA1 (', round(exp_by_x, 2), '%)', sep = '')) +
          ylab(paste('RDA2 (', round(exp_by_y, 2), '%)', sep = ''))
      } else {
        p +
          xlab('RDA1') +
          ylab('RDA2')
      }
      
    })
  output$fig_rda_full <-
    renderPlot(rct_fig_rda_full())
  rct_fig_rda_selection <-
    reactive({
      p <-
        ggRDA(rda_obj = rct_rda_selection(),
              envfit_df = rct_envfit_selection(),
              sp_size = 5) +
        # Generally theme_classic is a good choice to paint a figure
        theme_classic() +
        # In general, we don't need to show the legend in RDA figure
        theme(legend.position = "none") +
        # scale_XXXXX_manual series provide the ability
        # to define the style of legend by variable value
        scale_size_manual(values = c('ns' = .6,
                                     'sig' = .8)) +
        # Q: What's species here? I don't remember their is a significant level which is called 'species'
        # A: Indeed, their is no significant 'species'. However,
        # the species name in RDA which is generated from geom_text contains colour attribution.
        scale_colour_manual(values = c(
          'ns' = '#606060',
          'sig' = 'black',
          'species' = 'red'
        )) +
        scale_linetype_manual(values = c('ns' = 8, 'sig' = 1))
      if (input$axes_explain) {
        exp_by_x <-
          (as.list(rct_rda_selection()$CCA$eig)$RDA1) / (rct_rda_selection()$tot.chi) * 100
        exp_by_y <-
          (as.list(rct_rda_selection()$CCA$eig)$RDA2) / (rct_rda_selection()$tot.chi) * 100
        p +
          xlab(paste('RDA1 (', round(exp_by_x, 2), '%)', sep = '')) +
          ylab(paste('RDA2 (', round(exp_by_y, 2), '%)', sep = ''))
      } else {
        p +
          xlab('RDA1') +
          ylab('RDA2')
      }
      
    })
  output$fig_rda_selection <-
    renderPlot(rct_fig_rda_selection())
  
  output$dl_rda_full <-
    downloadHandler(
      filename = function() {
        paste('rda_full.', input$dl_format_full, sep = '')
      },
      content = function(file) {
        if (input$dl_format_full == 'pdf') {
          ggsave(file,
                 plot = rct_fig_rda_full())
        } else {
          ggsave(file,
                 plot = rct_fig_rda_full(),
                 dpi = input$dl_dpi_full)
        }
      }
    )
  
  output$dl_rda_selection <-
    downloadHandler(
      filename = function() {
        paste('rda_selection.', input$dl_format_selection, sep = '')
      },
      content = function(file) {
        if (input$dl_format_selection == 'pdf') {
          ggsave(file,
                 plot = rct_fig_rda_selection())
        } else {
          ggsave(file,
                 plot = rct_fig_rda_selection(),
                 dpi = input$dl_dpi_selection)
        }
      }
    )
}

shinyApp(ui = ui, server = server)