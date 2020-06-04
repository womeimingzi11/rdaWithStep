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
  source('Function/envfit_to_df.R')
  
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
  source('Function/ggRDA.R')
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
  
  output$dl_rda_full <-
    downloadHandler(
      filename = "rda_full.pdf",
      content = function(file){
        ggsave(file,
               plot = rct_fig_rda_full()
        )
      }
    )
  
  output$dl_rda_selection <-
    downloadHandler(
      filename = "rda_selection.pdf",
      content = function(file){
          ggsave(file,
                 plot = rct_fig_rda_selection()
                 )
        }
    )
}