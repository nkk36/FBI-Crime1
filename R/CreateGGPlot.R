CreateGGPlot <- function(syear,eyear,var,dat){
  
  #------------- SET LABELS IN PLOTS -------------#
  
  Header = c("Property Crime Rate", "Violent Crime Rate")
  Title = c("Violent Crime Rate and Property Crime Rate per Year", "Violent Crime Rate per Year",
            "Property Crime Rate per Year")
  XLab = "Year"
  YLab = "Rate per 100,000 People"
  LegendTitle = "Type of Crime"

  dat = dat[dat$YEAR >= as.numeric(syear) & dat$YEAR <= as.numeric(eyear),]
  dat = data.table::melt(dat, "YEAR" , c("VCR", "PCR"))
  dat$variable = ifelse(dat$variable == "VCR", "Violent Crime Rate", "Property Crime Rate")
  #------------- SET LABELS IN PLOTS -------------#
  
  #CHECK IF THE USER SELECTS NO CRIMES FOR DISPLAY. THE DEFAULT IS FOR BOTH (VIOLENT CRIME & PROPERTY CRIME)TO DISPLAY
  if(is.null(var)){
    var <- c("VC","PC")
  }
  
  #------------- BEGIN DATA SELECTION -------------#
  
  #VIOLENT CRIMES AND PROPERTY CRIMES
  if (length(var) == 2){      
    ggplot(data = dat) +
      geom_point(mapping = aes(x = YEAR, y = value, fill = variable, group = variable, colour = variable)) + 
      geom_line(mapping = aes(x = YEAR, y = value, fill = variable, group = variable, colour = variable)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[1]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #VIOLENT CRIMES
  else if (var == "VC"){
    ggplot(subset(dat, variable == "Violent Crime Rate")) +
      geom_point(mapping = aes(x = YEAR, y = value)) + 
      geom_line(mapping = aes(x = YEAR, y = value)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[2]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") 
  }
  #PROPERTY CRIMES
  else if (var == "PC"){
    ggplot(subset(dat, variable == "Property Crime Rate")) +
      geom_point(mapping = aes(x = YEAR, y = value)) + 
      geom_line(mapping = aes(x = YEAR, y = value)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[3]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none")  
  }
  
}
