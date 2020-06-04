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
library(plotly)
library(shinythemes)

# Package for data manipulation
library(tidyverse)
library(vegan)
library(ggvegan)

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
    h6('Update version: 20600603'),
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
        mainPanel(
            tabsetPanel(
                tabPanel('Overview',
                         div(
                             h3('Wha is RDA with step selection?'),
                             div(
                                 p(
                                     'Briefly, the Monte Carlo permutation tests followed by backward, forward or bothward selection were used to determine which variable was contained in each variable set.'
                                 ),
                                 p(
                                     'As',
                                     a(href = 'https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordistep', 'vegan::ordistep'),
                                     'described：'
                                 )
                             ),
                             div(
                                 p(
                                     'The basic functions for model choice in constrained ordination are add1.cca and drop1.cca. With these functions, ordination models can be chosen with standard R function step which bases the term choice on AIC. AIC-like statistics for ordination are provided by functions deviance.cca and extractAIC.cca (with similar functions for rda). Actually, constrained ordination methods do not have AIC, and therefore the step may not be trusted. This function provides an alternative using permutation P-values.'
                                 ),
                                 p(
                                     'Function ordistep defines the model, scope of models considered, and direction of the procedure similarly as step. The function alternates with drop and add steps and stops when the model was not changed during one step. The - and + signs in the summary table indicate which stage is performed. It is often sensible to have Pout > Pin in stepwise models to avoid cyclic adds and drops of single terms'
                                 ),
                                 style = "color: blue"
                             )
                         ),
                         div(
                             h3('Focus on species or sample site?'),
                             div(
                                 p(
                                     'In gerneral, there is two main scopes of RDA: 1. determine the relationships of species and environment variables only; 2.except determine the relationships of species and environment variables, the simple sites were also considered.'
                                 ),
                                 p('In this case, adding sample sites in the figure is not in my plan yet.'),
                                 p(
                                     'However, you are welcomed to commit any feature about this and even any other features in my',
                                     a(href = "https://github.com/womeimingzi11/rdaWithStep", 'repo on GitHub.')
                                 ),
                                 p(
                                     'You are also welcomed to visited my',
                                     a(href = "https://womeimingzi11.github.io", 'Blog (in Chinese)'),
                                     ', or contact me by',
                                     a(href = "mailto://chenhan28@gmail.com", 'mail.')
                                 )
                             )
                         )),
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
                ),
                tabPanel('Figures',
                         plotOutput('fig_rda_full'),
                         plotOutput('fig_rda_selection'))
                # tabPanel('ENVFIT',
                #          DTOutput('envfit_full'),
                #          DTOutput('envfit_selection'))
            )
        )
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
    ## Define the ggRDA function
    ggRDA <-
        function(rda_obj,
                 sp_size = 4,
                 arrow_txt_size = 4,
                 envfit_df) {
            # ggplot doesn't support rda object directly, we use ggvegan::fortify function to convert the rda to data.frame
            fmod <- fortify(rda_obj)
            # to get the arrow of biplot, we plot rda by vegan::plot.rda function firstly.
            # The arrow attributes contain in the attributes(plot_obejct$biplot)$arrow.mul
            basplot <- plot(rda_obj)
            mult <- attributes(basplot$biplot)$arrow.mul
            
            # To check if envfit_df exists or not
            # If envfit_df exists, join the fortified rda_obj and envfit to mark which variable is significant.
            if (missingArg(envfit_df)) {
                bplt_df <- filter(fmod, Score == "biplot") %>%
                    # If there is no requirement to mark significant variable
                    # use the sytle of sinificant (black bolder solid arrow)
                    # to paint the arrow
                    mutate(bold = 'sig')
            } else {
                bplt_df <- filter(fmod, Score == "biplot") %>%
                    left_join(envfit_df, by = c('Label' = 'factor')) %>%
                    # To mark the significant variables as sig, not significant variables as ns
                    # these information are stored in bold column
                    mutate(bold = ifelse(str_detect(sig, fixed('*')), 'sig', 'ns'))
            }
            ggplot(fmod, aes(x = RDA1, y = RDA2)) +
                coord_fixed() +
                geom_segment(
                    data = bplt_df,
                    # mult and RDA1/RDA2 are from fortified RDA data.frame
                    # they contain the direction and effects of every variabl
                    # their products are the direction and length of arrows
                    aes(
                        x = 0,
                        xend = mult * RDA1,
                        y = 0,
                        yend = mult * RDA2,
                        # Use different arrow size to indicate the significant level
                        size = bold,
                        # Use different arrow color to indicate the significant level
                        color = bold,
                        # Use different arrow linetype to indicate the significant level
                        linetype = bold
                    ),
                    #############################
                    # Q:Why use three different attibution to control the significant levels?
                    # It is redundancy, isn't it?
                    # A: In fact, it's not easy to recongize the significant level by one kind attribution
                    # Becasue it is not delicate to indicate it with supper bold and supper thin arrow,
                    # by the same logic, high contrast colors are not delicate neither.
                    # As for the line type, some arrow are really short, it's not easy to recognize
                    # weather it is solid or dashed line at all.
                    # To sum up, we use three different attributions
                    # to indicate the same difference to avoid any misleading.
                    #############################
                    # to control the size of the header of arrow
                    arrow = arrow(length = unit(0.25, "cm")),
                ) +
                # Add the text of variable name at the end of arrow
                geom_text(
                    data =  subset(fmod, Score == "biplot"),
                    aes(
                        x = (mult + mult / 10) * RDA1,
                        #we add 10% to the text to push it slightly out from arrows
                        y = (mult + mult / 10) * RDA2,
                        label = Label
                    ),
                    size = arrow_txt_size,
                    #otherwise you could use hjust and vjust. I prefer this option
                    hjust = 0.5
                ) +
                # Add the text of species
                geom_text(
                    data = subset(fmod, Score == "species"),
                    aes(colour = "species", label = Label),
                    size = sp_size
                )
        }
    ## Plot the RDA figures
    rct_fig_rda_full <-
        reactive({
            p <-
                ggRDA(
                    rda_obj = rct_rda_full(),
                    envfit_df = rct_envfit_full(),
                    sp_size = 5
                ) +
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
                ggRDA(
                    rda_obj = rct_rda_selection(),
                    envfit_df = rct_envfit_selection(),
                    sp_size = 5
                ) +
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
}

# Run the application
shinyApp(ui = ui, server = server)
