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


# 
# shinyApp(ui = ui, server = server)