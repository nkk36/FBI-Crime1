CreateGGPlot <- function(syear,eyear,var,dat){
  
  #------------- SET LABELS IN PLOTS -------------#
  
  Header = c("Property Crime Rate", "Violent Crime Rate")
  Title = c("Violent Crime Rate and Property Crime Rate per Year", "Violent Crime Rate per Year",
            "Property Crime Rate per Year")
  XLab = "Year"
  YLab = "Rate per 100,000 People"
  LegendTitle = "Type of Crime"
  
  #------------- SET LABELS IN PLOTS -------------#
  
  #CHECK IF THE USER SELECTS NO CRIMES FOR DISPLAY. THE DEFAULT IS FOR BOTH (VIOLENT CRIME & PROPERTY CRIME)TO DISPLAY
  if(is.null(var)){
    var <- c(3,15)
  }
  
  #------------- BEGIN DATA SELECTION -------------#
  
  #VIOLENT CRIMES AND PROPERTY CRIMES
  if (length(var) == 2){      
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, colour = "Violent Crime Rate")) + 
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, colour = "Violent Crime Rate")) + 
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, colour = "Property Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, colour = "Property Crime Rate")) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[1]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #VIOLENT CRIMES
  else if (var == 3){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) + 
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) + 
      labs(x = XLab, y = YLab) +
      ggtitle(Title[2]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") 
  }
  #PROPERTY CRIMES
  else if (var == 15){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[3]) + 
      scale_x_continuous(breaks=seq(1996,2015,1)) + 
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none")  
  }
  
}
