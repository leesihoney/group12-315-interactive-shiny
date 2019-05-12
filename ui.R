library(shiny)
library(ggsn)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(igraph)
library(igraphdata)
library(plotly)
library(ggthemes)
library(leaflet)
dashboardPage(
  dashboardHeader(title = "Group 12 Interactive Graphics Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Regression Modeling", tabName = "rg_modeling"),
      menuItem("Time Series Plot", tabName = "time_series"),
      menuItem("Choropleth Map", tabName = "immig_map"),
      menuItem("Word Cloud", tabName = "babyname"),
      menuItem("Dendrogram", tabName = "dendro")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              titlePanel("Barcelona"),
              mainPanel(
                img(src='https://thesefootballtimes.co/wp-content/uploads/2015/11/barcelona1.jpg', align = "center", width = "100%"),
                h4("Group Members: Weng Shian Ho, Andrew Lee, Tishya Gidhar, Neha Srivastava"),
                p("Data sets from the Portal Open Data BCN, the Ajuntament de Barcelona's open data service.

                  Open Data BCN, a project that was born in 2010, implementing the portal in 2011, has evolved and is now part of the Barcelona Ciutat Digital strategy, fostering a pluralistic digital economy and developing a new model of urban innovation based on the transformation and digital innovation of the public sector and the implication among companies, administrations, the academic world, organizations, communities and people, with a clear public and citizen leadership.")
                )
              ),
      # First tab content
      tabItem(tabName = "rg_modeling", 
              # Application title
              titlePanel("Regression Modeling"),
              sidebarPanel( 
                p("Select the input for the Dependent Variable"),
                selectInput(inputId = "DepVar", label = "Dependent Variables",
                            multiple = FALSE,
                            choices =list("Victims", "Vehicles.involved"),
                            selected = "Victims"),
                p("Select the input for the Independent Variable"),
                selectInput(inputId = "IndVar", label = "Independent Variables",
                            multiple = FALSE, choices = list("Weekday", "Month",
                                                             "Vehicles.involved",
                                                             "Day_of_Month", "Time_of_Day"),
                            selected = "Weekday")
                
                
              ),
              mainPanel(
                tabsetPanel(type = "tabs", 
                            tabPanel("Distributions",
                                     fluidRow(
                                       column(6, plotOutput("hist_Dep")),
                                       column(6, plotOutput("Ind"))
                                     )),
                            tabPanel("Scatterplot", plotOutput("scatterplot"))
                            )
                )
              ),
      tabItem(tabName = "time_series",
              titlePanel("Time Series of Barcelona Accidents in 2017"),
              sidebarPanel( 
                p("Select the input for the Dependent Variable"),
                selectInput(inputId = "DepVariable", label = "Dependent Variables",
                            multiple = FALSE,
                            choices =list("Victims", "Vehicles.involved", "Mild.injuries", "Serious.injuries"),
                            selected = "Victims")
                ),
              mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Time Series", plotOutput("timeseries"))
                            )
                )
              ),
      # Second Tab Content
      tabItem(tabName = "immig_map",
              titlePanel("Immigrants in Barcelona"),
              fluidRow(
                column(10,
                  p("This graphs show immigrants by nationality and by neighbourhoods of the city of Barcelona (2015-2017).")
                  )
              ),
              fluidRow(
                column(5,
                 p("Select the year of dataset"),
                 selectInput(inputId = "yr", label = "Year",
                             multiple = FALSE,
                             choices = list(2017, 2016, 2015),
                             selected = 2017)),
                column(5,
                   p("Select the continent for immigrants' nationality"),
                   selectInput(inputId = "cont", label = "Continent",
                               multiple = FALSE, choices = list("All","Asia", "Europe",
                                                                "Africa",
                                                                "North America", "South America"),
                               selected = "All"))
                ),
              mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Plotly", plotlyOutput("chorop_map")),
                            tabPanel("Leaflet", leafletOutput(outputId = "leaflet_map")))
              )
      ),
      tabItem(tabName = "babyname",
              titlePanel("Top 20 Baby Names in Barcelona"),
              sidebarPanel(
                selectInput(inputId = "year", label = "Year",
                            multiple = FALSE,
                            choices =list(1996,1997,1998,1999,2000,2001,2002,2003,
                                          2004,2005,2006,2007, 2008, 2009, 2010,
                                          2011, 2012, 2013, 2014, 2015, 2016),
                            selected = 1996),
                selectInput(inputId = "sex", label = "Gender",
                            multiple = FALSE, choices = list("Male", "Female"),
                            selected = "Female")
              ),
              mainPanel(
                tabsetPanel(type = "tabs", 
                            tabPanel("Word Cloud", plotOutput("wordc")),
                            tabPanel("Bar Chart", plotlyOutput("barp"))
                )
              )
      ),
      tabItem(tabName = "dendro",
              # Application title
              titlePanel("Dendrogram of Barcelona's Districts According Rates"),
              sidebarPanel( 
                p("First Variable To Cluster on"),
                selectInput(inputId = "var1", label = "Variable 1",
                            multiple = FALSE,
                            choices =list("Unemployment", "Birth","Death"),
                            selected = "Unemployment"),
                p("Second Variable To Cluster on"),
                selectInput(inputId = "var2", label = "Variable 2",
                            multiple = FALSE, choices = list("Unemployment", "Birth","Death"),
                            selected = "Death")
                
              ),
              plotOutput("Dendy")
              )
    )
  )
)

