CreateExtBarPlot <- function(syear,eyear,var,dat,datStack){
  #------------- SET LABELS IN PLOTS -------------#
  
  Header = c("Violent Crime", "Property Crime",
             "Population", "Murder", 
             "Rape (Legacy definition)", "Robbery", "Aggravated Assault", 
             "Burglary", "Larceny-Theft", 
             "Motor Vehicle Theft")
  
  Title = c("Violent Crime Records and Property Crime Records per Year", "Violent Crimes per Year",
            "Property Crimes per Year", "Population per Year", 
            "Murders per Year", "Rapes per Year (Legacy definition)", 
            "Robberies per Year", "Aggravated Assaults per Year",
            "Burglaries per Year", "Larceny-Thefts per Year", 
            "Motor Vehicle Thefts per Year"
  )
  
  XLab = "Year"
  YLab = "Number of Records"
  LegendTitle = "Type of Crime"
  PopulationTitle = "Legend"
  
  cols = c("Murder" = "MURDER", "Rape (Leg)" = "RAPELEG", "Robbery" = "ROB", 
           "Aggravated Assault" = "AGGASSAU", "Burglary" = "BURG", 
           "Larceny-Theft" = "LT", "Motor Vehicle Theft" = "MVT")
  index = which(var %in% cols)
  
  dat2 = datStack[datStack$YEAR >= as.numeric(syear) & datStack$YEAR <= as.numeric(eyear),]
  dat2 = subset(dat2, TYPE %in% names(cols[index]))
  #------------- SET LABELS IN PLOTS -------------#
  
  
  #------------- BEGIN DATA SELECTION -------------#
  
  
    ggplot(data=dat2, aes(x=CRIME, y=AMOUNT, fill=TYPE)) +
      geom_bar(stat="identity") +
      scale_y_continuous(labels = scales::comma) +
      labs(y = YLab) +
      facet_grid(~YEAR) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), text = element_text(size = 20))
  
  #------------- END DATA SELECTION -------------#
  
}
