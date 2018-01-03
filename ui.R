#------------- CALL REQUIRED PACKAGES AND FUNCTIONS -------------#

library(shiny)
library(leaflet)

#------------- CALL REQUIRED PACKAGES AND FUNCTIONS -------------#

#------------- BEGIN SHINY USER INTERFACE -------------#

# DEFINE USER INTERFACE
shinyUI(fluidPage(
  
  #TITLE OF APPLICATION
  titlePanel("National Overview: Crime in the U.S."),
  
  #------------------------ BEGIN SIDEBAR ------------------------#
  #sidebarLayout(
    sidebarPanel(
      helpText("Scroll down and select your choices. Click this button when you wish to update the figures and tables."),
      #ACTION BUTTON
      actionButton("go","Update"),
      #SELECT STARTING YEAR
      selectInput(inputId = "syear",label = "Start Year",choices = c(
        "1996" = 1,"1997" = 2,"1998" = 3,"1999" = 4,
        "2000" = 5,"2001" = 6,"2002" = 7,"2003" = 8,
        "2004" = 9,"2005" = 10,"2006" = 11,"2007" = 12,
        "2008" = 13,"2009" = 14,"2010" = 15,"2011" = 16,
        "2012" = 17,"2013" = 18,"2014" = 19,"2015" = 20), selected = 1
       ),
      #SELECT ENDING YEAR
      selectInput(inputId = "eyear",label = "End Year",choices = c(
        "1996" = 1,"1997" = 2,"1998" = 3,"1999" = 4,
        "2000" = 5,"2001" = 6,"2002" = 7,"2003" = 8,
        "2004" = 9,"2005" = 10,"2006" = 11,"2007" = 12,
        "2008" = 13,"2009" = 14,"2010" = 15,"2011" = 16,
        "2012" = 17,"2013" = 18,"2014" = 19,"2015" = 20), selected = 20
      ),
      #CHOOSE WHAT CRIME TO DISPLAY IN BASIC GRAPHS
      checkboxGroupInput(inputId = "y2", "Choose Type of Crime", c("Violent Crime" = 3, "Property Crime" = 15), 
                         selected = c(3,15), inline = FALSE,
                         width = NULL),
      helpText("Note: Violent Crime includes murder and non-negligent manslaughter, rape (legacy definition), robbery, and aggravated assault. Property
               crime includes burglary, larceny-theft, and motor vehicle theft."),
      #CHOOSE WHAT CRIMES TO DISPLAY IN EXTENDED GRAPHS
      selectInput(inputId = "y",label = "Choose Variables to Display in Extended Graphs & Tables",choices = c(
        "Violent & Property Crime" = 1, "All Violent Crime" = 2, "All Property Crime" = 3, "Population" = 4, 
        "Murder & Non-negligent Manslaughter" = 5, 
        "Rape (Legacy definition)" = 9,"Robbery" = 11,"Aggravated Assault" = 13, "Burglary" = 17,
        "Larceny-Theft" = 19,"Motor Vehicle Theft" = 21)
      )
      
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




