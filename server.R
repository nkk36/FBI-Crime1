#------------- LOAD REQUIRED PACKAGES, FUNCTIONS, AND DATA -------------#

# Load packages
library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)
library(maps)
library(leaflet)
library(rgdal)

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

#------------- BEGIN SHINY SERVER -------------#

shinyServer(function(input, output) {
  
  #UPDATE BUTTON
  v <- reactiveValues(doPlot = FALSE)
  
  #NEED TO OBSERVE THE USER CLICKED THE UPDATE BUTTON
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })

  #------------- LINE GRAPH BASIC PC + VC -------------#
  output$xyPlot = renderPlot({
    if (v$doPlot == FALSE) return()
    
    
    #Plot graph depending on type of crime desired
    isolate({
      CreateGGPlot(input$syear,input$eyear,input$y2,dat)
    })
  })
  #------------- LINE GRAPH BASIC PC + VC -------------#
  
  #------------- BAR GRAPH BASIC PC + VC -------------#
  output$barPlot = renderPlot({
    if (v$doPlot == FALSE) return()
    
    
    #Plot graph depending on variable desired
    isolate({
      CreateGGBarPlot(input$syear,input$eyear,input$y2,dat)
    })
  })
  #------------- BAR GRAPH BASIC PC + VC -------------#
  
  #------------- DATA TABLE BASIC PC + VC -------------#
   output$xyPlotDataTable = renderDataTable({
     if (v$doPlot == FALSE) return()
     isolate({CreateDF(input$syear,input$eyear,input$y2,dat)})
       
     
     })
   #------------- DATA TABLE BASIC PC + VC -------------#
   
   #------------- DOWNLOAD DATA TABLE -------------#
   output$downloadData1 <- downloadHandler(
     filename = function() { 
       paste("data", '.csv', sep='') 
     },
     content = function(file) {
       DataForDownload <- CreateDF(input$syear,input$eyear,input$y2,dat)
       write.csv(DataForDownload, file)
     }
   )
   #------------- DOWNLOAD DATA TABLE -------------#
   
   #------------- LINE GRAPH EXTENDED -------------#
   output$xyExtPlot = renderPlot({
     if (v$doPlot == FALSE) return()
     
     
     #Plot graph depending on type of crime desired
     isolate({
       CreateExtPlot(input$syear,input$eyear,input$y,dat)
     })
   })
   #------------- LINE GRAPH EXTENDED -------------#
   
   #------------- BAR GRAPH EXTENDED -------------#
   output$ExtBarPlot = renderPlot({
     if (v$doPlot == FALSE) return()
     
     
     #Plot graph depending on variable desired
     isolate({
       CreateExtBarPlot(input$syear,input$eyear,input$y,dat,datStack)
     })
   })
   #------------- BAR GRAPH EXTENDED -------------#
   
   #------------- DATA TABLE EXTENDED -------------#
   output$ExtDataTable = renderDataTable({
     if (v$doPlot == FALSE) return()
     isolate({CreateExtDF(input$syear,input$eyear,input$y,dat)})
     
     
   })
   #------------- DATA TABLE EXTENDED -------------#

   #------------- DOWNLOAD EXTENDED DATA TABLE -------------#
   output$downloadData2 <- downloadHandler(
     filename = function() { 
       paste("data", '.csv', sep='') 
     },
     content = function(file) {
       DataForDownload <- CreateExtDF(input$syear,input$eyear,input$y,dat)
       write.csv(DataForDownload, file)
     }
   )
   #------------- DOWNLOAD EXTENDED DATA TABLE -------------#

   
   output$StateMap <- renderLeaflet({
     
     map <- readOGR(dsn="data/cb_2015_us_state_500k.shp", layer="cb_2015_us_state_500k", verbose = FALSE)

     leaflet(map)%>% 
       addProviderTiles("Thunderforest.Transport",
                        options = providerTileOptions(noWrap = TRUE, opacity = 1))%>%
     addPolygons(stroke = TRUE, color = "#000000",weight = 1, fillOpacity = 0.5,
                 smoothFactor = 1, popup = as.character(map$VCR) ,fillColor = ~colorQuantile("YlOrRd", map$VCR)(VCR)
                 )%>%
     fitBounds(-125.77755, 25.79522, -64.91645, 48.93582)%>%
     addLegend(position = "topright", 
                 color = c("#ffffb2", "#feb751", "#f55629", "#c00029"),
                 na.label = "NA", 
                 opacity = 0.5, 
                 labels = c("121 - 288","288 - 399","399 - 524","524 - 1491"),
                 title = "Violent Crime Rate"
                 )
     
     

              # leaflet() %>%
     #   addProviderTiles("Thunderforest.Transport",
     #                    options = providerTileOptions(noWrap = TRUE)
     #   ) %>%
     #   addMarkers(data = cbind(16.57021,46.75527)
     #              )%>%
     #   fitBounds(-125.77755, 25.79522, -64.91645, 48.93582)

     })
}) 

#------------- END SHINY SERVER -------------#



#------------- EXTRA CODE FOR DISPLAYING THE MAP -------------#


#   map <- readOGR(dsn="C:/Users/nkallfa36/Desktop/US_States/cb_2015_us_state_500k.shp", layer="cb_2015_us_state_500k", verbose = FALSE)
# 
# leaflet(map) %>% addPolygons(stroke = FALSE, fillOpacity = 0.5,
#                              smoothFactor = 0.5,
#                              color = ~colorQuantile("YlOrRd", map$VCR)(VCR))#%>%
#    # addLegend("bottomright",pal = colorQuantile, values = map$VCR,
#    #          title = "Login Status Merck",
#    #          opacity = 1)


#THE ABOVE CODE WORKS TO DISPLAY A MAP



# m <- map("state", interior = FALSE, plot=FALSE)
# 
# m$my.colors <- 0
# m$my.colors[m$names %in% c("virginia:main","georgia","vermont","kentucky","indiana","south carolina")] <- 2
# m$my.colors[m$names %in% c("west virginia","north carolina:main","ohio")] <- 3
# m$my.colors[m$names %in% c("tennessee","rhode island","oklahoma","new jersey","mississippi","massachusetts:main","illinois","maryland","maine","missouri","new hampshire","new jersey","pennsylvania","florida","connecticut","delaware","district of columbia","alabama")] <- 4
# m$my.colors[m$names %in% c("arkansas")] <- 5
# m$my.colors[m$names %in% c("arizona","colorado","kansas","louisiana","minnesota","michigan:north","michigan:south","nebraska","new york:main","new york:manhattan","new york:staten island","new york:long island","new mexico","north dakota","south dakota","texas","wyoming","wisconsin")] <- 6
# m$my.colors[m$names %in% c("iowa","montana","nevada","utah")] <- 7
# m$my.colors[m$names %in% c("california","idaho","oregon","washington:main")] <- 8
# #Add Hawaii
# m.world <- map("world", c("USA","hawaii"), xlim=c(-180,-65), ylim=c(19,72),interior = FALSE)
# leaflet(m) %>% addTiles() %>% addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
# title("Election Day Poll Closing Times")

#map("state", boundary = FALSE, col="grey", add = TRUE, fill=FALSE)
#map("state", boundary = TRUE, col=m$my.colors, add = TRUE, fill=TRUE )
#map("world", c("hawaii"), boundary = TRUE, col=8, add = TRUE, fill=TRUE )
#map("world", c("USA:Alaska"), boundary = TRUE, col='orange', add = TRUE, fill=TRUE )
# legend("topright", c('7:00pm','7:30pm','8:00pm','8:30pm','9:00pm','10:00pm','11:00pm','1:00am'),
#        pch=15, col=c(2,3,4,5,6,7,8,'orange'), title="Poll Closing Time (EST)", ncol=2, cex=1.2)
#})
# points <- eventReactive(input$recalc, {
#   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
# }, ignoreNULL = FALSE)
# 
# output$StateMap <- renderLeaflet({
#   leaflet() %>%
#     addProviderTiles("Stamen.TonerLite",
#                      options = providerTileOptions(noWrap = TRUE)
#     ) %>%
#     addMarkers(data = points())
# })
