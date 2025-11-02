# Server部分 - 服务器逻辑
server <- function(input, output) {
  ##############################
  # Reveal the data frame secton
  df_com <- reactive({
    if (input$data_source == 'demo') {
      read_csv('resource/data/df_com_smp.csv')
    } else {
      if (is.null(input$df_com)) {
        return(NULL)
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
        return(NULL)
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
    req(df_com(), df_env())
    ## Inputs are required above with req(); proceed to model fit
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
      # 确保数据可用
      req(df_com(), df_env())
      
      # 提前获取数据并存储在全局环境中（这是一个变通方法）
      # 注意：在生产环境中应该使用更优雅的解决方案
      .GlobalEnv$.temp_com_data <- df_com()
      .GlobalEnv$.temp_env_data <- df_env()
      scale_option <- input$rda_scale
      direction_option <- input$select_direction
      perm_max_option <- input$selection_perm_max
      
      # 使用tryCatch来处理错误并清理全局环境
      result <- tryCatch({
        # 创建完整模型
        if (scale_option) {
          full_model <- rda(.temp_com_data ~ ., data = .temp_env_data, na.action = na.omit, scale = TRUE)
        } else {
          full_model <- rda(.temp_com_data ~ ., data = .temp_env_data, na.action = na.omit)
        }
        
        # 创建零模型
        if (scale_option) {
          null_model <- rda(.temp_com_data ~ 1, data = .temp_env_data, na.action = na.omit, scale = TRUE)
        } else {
          null_model <- rda(.temp_com_data ~ 1, data = .temp_env_data, na.action = na.omit)
        }
        
        # 执行逐步选择
        if (direction_option == 'backward') {
          # 向后选择
          ordistep(full_model, 
                   direction = direction_option,
                   perm.max = perm_max_option,
                   trace = 0)
        } else {
          # 向前或双向选择
          ordistep(null_model,
                   scope = list(lower = formula(null_model), upper = formula(full_model)),
                   direction = direction_option,
                   perm.max = perm_max_option,
                   trace = 0)
        }
      }, 
      finally = {
        # 清理全局环境中的临时数据
        rm(.temp_com_data, .temp_env_data, envir = .GlobalEnv)
      })
      
      return(result)
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
  source('R/envfit_to_df.R')
  
  ## ENVFIT to FULL Model
  rct_envfit_full <- 
    reactive({
      envfit(ord = rct_rda_full(),
             env = as.data.frame(df_env()),
             permutations = input$envfit_perm) %>%
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
      # 确保所有必要的对象都可用
      req(rct_rda_selection(), df_env())
      
      # 提前获取所有必要的数据，完全避免响应式函数的引用
      rda_selection_result <- rct_rda_selection()
      env_data <- df_env()
      perm_value <- input$envfit_perm
      
      # 使用预保存的变量执行envfit
      result <- envfit(
        ord = rda_selection_result,
        env = as.data.frame(env_data),
        permutations = perm_value
      ) %>%
        envfit_to_df(r2_dig = 3)
      
      return(result)
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
  source('R/ggRDA.R')
  ## Plot the RDA figures
  rct_fig_rda_full <- 
    reactive({
      # 提前获取所有必要的结果，避免多次调用
      rda_full_result <- rct_rda_full()
      envfit_full_result <- rct_envfit_full()
      axes_explain_option <- input$axes_explain
      
      p <- 
        ggRDA(rda_obj = rda_full_result,
              envfit_df = envfit_full_result,
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
      if (axes_explain_option) {
        # 使用保存的结果对象计算解释率，避免多次调用响应式函数
        exp_by_x <-
          (as.list(rda_full_result$CCA$eig)$RDA1) / (rda_full_result$tot.chi) * 100
        exp_by_y <-
          (as.list(rda_full_result$CCA$eig)$RDA2) / (rda_full_result$tot.chi) * 100
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
      # 确保所有必要的对象都可用
      req(rct_rda_selection(), rct_envfit_selection())
      
      # 提前获取所有必要的结果，完全避免响应式函数的引用
      rda_selection_result <- rct_rda_selection()
      envfit_result <- rct_envfit_selection()
      axes_explain_option <- input$axes_explain
      
      p <- 
        ggRDA(rda_obj = rda_selection_result,
              envfit_df = envfit_result,
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
        # 使用保存的结果对象计算解释率
        exp_by_x <-
          (as.list(rda_selection_result$CCA$eig)$RDA1) / (rda_selection_result$tot.chi) * 100
        exp_by_y <-
          (as.list(rda_selection_result$CCA$eig)$RDA2) / (rda_selection_result$tot.chi) * 100
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