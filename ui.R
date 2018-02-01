#------------- LOAD REQUIRED PACKAGES, FUNCTIONS, AND DATA -------------#

# Load packages
library(shiny)
library(leaflet)

# Load functions
source("R/TakeSomeYears.R")
source("R/CreateDF.R")
source("R/CreateExtDF.R")
source("R/CreateGGPlot.R")
source("R/CreateGGBarPlot.R")
source("R/CreateExtPlot.R")
source("R/CreateExtBarPlot.R")

# Load data
dat <- read.csv("data/Table1_2015.csv")
datStack <- read.csv("data/Stacked.csv")
StatePop <- read.csv("data/StatePop.csv")

#------------- LOAD REQUIRED PACKAGES, FUNCTIONS, AND DATA -------------#

#------------- BEGIN SHINY USER INTERFACE -------------#

# DEFINE USER INTERFACE
shinyUI(fluidPage(
  
  tags$head(includeScript("google-analytics.js")),
  
  #TITLE OF APPLICATION
  titlePanel("National Overview: Crime in the U.S."),
  
  #------------------------ BEGIN SIDEBAR ------------------------#
  #sidebarLayout(
    sidebarPanel(
      helpText("Scroll down and select your choices. Click this button when you wish to update the figures and tables."),
      #ACTION BUTTON
      actionButton("go","Update"),
      #SELECT STARTING YEAR
      selectInput(inputId = "syear",
                  label = "Start Year",
                  choices = sort(unique(dat$YEAR)),
                  selected = 1996),
      #SELECT ENDING YEAR
      selectInput(inputId = "eyear",
                  label = "End Year",
                  choices = sort(unique(dat$YEAR)), 
                  selected = 2015),
      #CHOOSE WHAT CRIME TO DISPLAY IN BASIC GRAPHS
      checkboxGroupInput(inputId = "y2", 
                         label = "Choose Type of Crime", 
                         choices = c("Violent Crime" = "VC", "Property Crime" = "PC"), 
                         selected = c("VC","PC"), 
                         inline = FALSE,
                         width = NULL),
      helpText("Note: Violent Crime includes murder and non-negligent manslaughter, rape (legacy definition), robbery, and aggravated assault. Property
               crime includes burglary, larceny-theft, and motor vehicle theft."),
      #CHOOSE WHAT CRIMES TO DISPLAY IN EXTENDED GRAPHS
      checkboxGroupInput(inputId = "y",
                         label = "Choose Variables to Display in Extended Graphs & Tables",
                         choices = c("Population" = "POP", "Murder & Non-negligent Manslaughter" = "MURDER", 
                                     "Rape (Legacy definition)" = "RAPELEG", "Robbery" = "ROB",
                                     "Aggravated Assault" = "AGGASSAU", "Burglary" = "BURG",
                                     "Larceny-Theft" = "LT", "Motor Vehicle Theft" = "MVT")
                        )
      # #CHOOSE WHAT CRIMES TO DISPLAY IN EXTENDED GRAPHS
      # selectInput(inputId = "y",label = "Choose Variables to Display in Extended Graphs & Tables",choices = c(
      #   "Violent & Property Crime" = 1, "All Violent Crime" = 2, "All Property Crime" = 3, "Population" = 4, 
      #   "Murder & Non-negligent Manslaughter" = 5, 
      #   "Rape (Legacy definition)" = 9,"Robbery" = 11,"Aggravated Assault" = 13, "Burglary" = 17,
      #   "Larceny-Theft" = 19,"Motor Vehicle Theft" = 21)
      
    ),
    
    #------------------------ END SIDEBAR ------------------------#
    
    #------------------------ BEGIN MAIN PANEL ------------------------#
    # MAIN PANEL WITH TABS AND ASSOCIATED OUTPUTS
    mainPanel(
      tabsetPanel(
        
        ## CORE TABS
        tabPanel("Introduction", includeMarkdown("Intro.md")), 
        tabPanel("Map",leafletOutput("StateMap")), #NEED TO ADD MAP WITH US STATES AND THEIR VIOLENT CRIMES + PROPERTY CRIME STATS
        tabPanel("Graphs", plotOutput("xyPlot"),plotOutput("barPlot")), #LEGEND TITLE FOR LINE GRAPH
        tabPanel("Table", dataTableOutput("xyPlotDataTable"), downloadLink('downloadData1', 'Download')), #NEED TO OFFER DOWNLOAD OPTION
        tabPanel("Extended Graphs",plotOutput("xyExtPlot"),plotOutput("ExtBarPlot")), #LEGEND TITLE FOR LINE GRAPH
        tabPanel("Extended Table",dataTableOutput("ExtDataTable"), downloadLink('downloadData2', 'Download')), #NEED TO OFFER DOWNLOAD OPTION
        tabPanel("Changes")
      )
    )
    #------------------------ END MAIN PANEL ------------------------#
    
))

#------------- END SHINY USER INTERFACE -------------#




