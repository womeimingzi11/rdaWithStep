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

# 导入必要的包
# Package for Shiny
library(shiny)
library(shinythemes)
library(DT)
library("markdown")

# Package for data manipulation
library(tidyverse)
library(vegan)

# 条件加载ggvegan包，如果不可用则提供自动安装选项
tryCatch({
  library(ggvegan)
  ggvegan_available <- TRUE
}, error = function(e) {
  cat("ggvegan包未安装。正在尝试自动安装...\n")
  
  # 尝试多种安装方法
  install_ggvegan <- function() {
    # 方法1: 尝试使用remotes从GitHub安装（推荐方法）
    tryCatch({
      # 先确保remotes包已安装
      if (!requireNamespace('remotes', quietly = TRUE)) {
        install.packages('remotes', repos = 'https://cran.r-project.org')
      }
      
      cat("尝试使用remotes从GitHub安装ggvegan...\n")
      remotes::install_github('gavinsimpson/ggvegan')
      if ('ggvegan' %in% installed.packages()[,1]) {
        cat("✓ 使用remotes从GitHub成功安装ggvegan\n")
        return(TRUE)
      }
    }, error = function(e) {
      cat("✗ 使用remotes从GitHub安装ggvegan失败\n")
    })
    
    # 方法2: 尝试使用devtools从GitHub安装
    tryCatch({
      # 先确保devtools包已安装
      if (!requireNamespace('devtools', quietly = TRUE)) {
        install.packages('devtools', repos = 'https://cran.r-project.org')
      }
      
      cat("尝试使用devtools从GitHub安装ggvegan...\n")
      devtools::install_github('gavinsimpson/ggvegan')
      if ('ggvegan' %in% installed.packages()[,1]) {
        cat("✓ 使用devtools成功安装ggvegan\n")
        return(TRUE)
      }
    }, error = function(e) {
      cat("✗ 使用devtools安装ggvegan失败\n")
    })
    
    return(FALSE)
  }
  
  # 执行安装尝试
  if (install_ggvegan()) {
    library(ggvegan)
    ggvegan_available <- TRUE
  } else {
    cat("⚠️ 所有安装ggvegan的尝试都失败了。某些功能可能不可用。\n")
    cat("   请尝试手动安装: devtools::install_github('gavinsimpson/ggvegan')\n")
    ggvegan_available <- FALSE
  }
})

# 加载UI和Server组件
source('ui.R')
source('server.R')

# 运行Shiny应用
shinyApp(ui = ui, server = server)