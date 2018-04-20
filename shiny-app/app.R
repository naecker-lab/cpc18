#####################
## CPC18 Shiny App ##
#####################

## In order to run, make sure you have installed      ##
## -- shiny: for making shiny apps                    ##
## -- shinydashboard: for easy dashboard UI template  ##
## -- ggplot2: for making graphs                      ##
## -- plotly: for making graphs into htmlwidgets      ##
## -- plyr and dplyr: for data manipulation           ##
## -- fread: makes reading csv's faster               ##
## -- DT: for shiny data tables                       ##

## Load libraries and read in data here (i.e. outside the ui and server) so that it's only run once ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(data.table)
library(DT)

## Working directory should be top of the directory (fairness) ##
## Read in data (use fread because read.csv is super slow with lots of observations ##
data <- fread("data.csv")

##############
## Shiny UI ##
##############
ui <- dashboardPage(
  skin = "red",
  
  ## This is the header of the dashboard ##
  dashboardHeader(title="CPC18 Shiny App"),
  
  ## This is where the sidebar of the dashboard is defined ##
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tab 1", tabName = "tab1", icon = icon("th")),
      menuItem("Tab 2", tabName = "tab2", icon = icon("th")),
      menuItem("Tab 3", tabName = "tab3", icon = icon("th"))
    )
    ## End of sidebar menu
  ),
  ## End of dashboard sidebar
  
  ## This is where the body of the dashboard is defined ##
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              h2("Tab 1"),
              DT::dataTableOutput("raw_dt")),
      tabItem(tabName = "tab2",
              h2("Tab 2"),
              plotlyOutput("histogram")),
      tabItem(tabName = "tab3",
              h2("Tab 3"))
    )
    ## End of tab items
  ) 
  ## End of dashboard body
)
## End of UI

##################
## Shiny Server ##
##################
server <- function(input, output) {
  
  output$raw_dt <- DT::renderDataTable({
    data
  })
  
  output$histogram <- renderPlotly({
    p <- ggplot(data, aes(x=Order, fill=Button)) + 
      geom_histogram(binwidth=.5, position="dodge") +
      geom_vline(xintercept=15, linetype="dashed", size=.25)
    ggplotly(p)
  })

}
## End of server

## Puts the ui and server together ##
shinyApp(ui = ui, server = server)