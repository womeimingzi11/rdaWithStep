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

# Load UI
source('ui.R')

# Load Server
source('server.R')

# Run the application
shinyApp(ui = ui, server = server)