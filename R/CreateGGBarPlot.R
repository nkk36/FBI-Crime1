CreateGGBarPlot <- function(syear,eyear,var,dat){
  
  #------------- SET LABELS IN PLOTS -------------#
  Header = c("Property Crime", "Violent Crime")
  Title = c("Violent Crime Records and Property Crime Records per Year", "Violent Crimes per Year",
            "Property Crimes per Year")
  XLab = "Year"
  YLab = "Number of Records"
  LegendTitle = "Type of Crime"
  
  dat = dat[dat$YEAR >= as.numeric(syear) & dat$YEAR <= as.numeric(eyear),]
  dat = data.table::melt(dat, "YEAR" , c("VC", "PC"))
  dat$variable = ifelse(dat$variable == "VC", "Violent Crime", "Property Crime")
  #------------- SET LABELS IN PLOTS -------------#
  
  #CHECK IF THE USER SELECTS NO CRIMES FOR DISPLAY. THE DEFAULT IS FOR BOTH (VIOLENT CRIME & PROPERTY CRIME)TO DISPLAY
  if(is.null(var)){
    var <- c("VC","PC")
  }
  #------------- BEGIN DATA SELECTION -------------#
  
  #VIOLENT CRIMES AND PROPERTY CRIMES
  if (length(var) == 2){
    ggplot(data = dat) +   
      geom_bar(mapping = aes(x = YEAR, 
                             y = value, 
                             group = variable, 
                             fill = variable), 
               position = "dodge", 
               stat="identity") + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[1]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) + 
      scale_fill_discrete(name=LegendTitle, 
                          labels = Header)
  }
  #vIOLENT CRIME
  else if (var == "VC"){
    ggplot(data = subset(dat, variable == "Violent Crime")) +   
      geom_bar(mapping = aes(x = YEAR, 
                             y = value, 
                             group = variable, 
                             fill = variable), 
               position = "dodge", 
               stat="identity") + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[2]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+ 
    #scale_fill_discrete(name=LegendTitle, 
    # labels = Header[2]) + 
    #geom_text(aes(label=dat$VC), position=position_dodge(width=0.9), vjust=-0.25)
    
  }
  #PROPERTY CRIME
  else if (var == "PC"){
    ggplot(data = subset(dat, variable == "Property Crime")) +   
      geom_bar(mapping = aes(x = YEAR, 
                             y = value, 
                             group = variable, 
                             fill = variable), 
               position = "dodge", 
               stat="identity") + 
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[3]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+ 
    #scale_fill_discrete(name=LegendTitle, 
    # labels = Header[1]) +
    #geom_text(aes(label=dat$PC), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  
  #------------- END DATA SELECTION -------------#
  
  
}
