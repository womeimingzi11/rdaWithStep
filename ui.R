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
  h6('Update version: 20600604'),
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
      ),
      selectInput(
        'axes_explain',
        'Show the explainiation power by each axes?',
        choices = c(TRUE, FALSE),
        selected = TRUE
      )
      # selectInput(
      #     'select_trace',
      #     'Do you want to check the information during the model building? (Where the 0 means show the final model only)',
      #     choices = c(TRUE, FALSE),
      #     selected = 0
      # )
    ),
    mainPanel(tabsetPanel(
      tabPanel('Overview',
               includeMarkdown('README.md')),
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
              choices = c(72,96,150,300,600),
              selected = 300
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
              choices = c(72,96,150,300,600),
              selected = 300
            ),
            downloadButton('dl_rda_selection',
                           'Download Figure')
          ),
          column(6,
                 plotOutput('fig_rda_selection'))
        )
      )
    ))
  )
)