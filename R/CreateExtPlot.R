CreateExtPlot <- function(syear,eyear,var,dat){
  
  #------------- SET LABELS IN PLOTS -------------#
  
  Header = c("Property Crime Rate", "Violent Crime Rate")
  Title = c("Violent Crime Rate and Property Crime Rate per Year", "Violent Crime Rate per Year",
            "Property Crime Rate per Year", "Population per Year", 
            "Murder Rate per Year", "Rape Rate per Year (Legacy definition)",
            "Robbery Rate per Year", "Aggravated Assault Rate per Year", 
            "Burglary Rate per Year", "Larceny-Theft Rate per Year", 
            "Motor Vehicle Theft Rate per Year"
  )
  XLab = "Year"
  YLab = "Rate per 100,000 People"
  YLabPop = "Population"
  LegendTitle = "Type of Crime"
  
  #------------- SET LABELS IN PLOTS -------------#
  
  #------------- BEGIN DATA SELECTION -------------#
  
  #ALL CRIMES
  if (var == 1){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR, color = "Murder Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR, color = "Murder Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR, color = "Rape (Leg) Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR, color = "Rape (Leg) Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR, color = "Robbery Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR, color = "Robbery Rate")) +        
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR, color = "Aggravated Assault Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR, color = "Aggravated Assault Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR, color = "Burglary Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR, color = "Burglary Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR, color = "Larceny-Theft Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR, color = "Larceny-Theft Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR, color = "Motor Vehicle Theft Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR, color = "Motor Vehicle Theft Rate")) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[1]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) +
      scale_fill_discrete(name=LegendTitle)
    
  }
  #ALL VIOLENT CRIMES
  else if (var == 2){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=VCR, color = "Violent Crime Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR, color = "Murder Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR, color = "Murder Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR, color = "Rape (Leg) Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR, color = "Rape (Leg) Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR, color = "Robbery Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR, color = "Robbery Rate")) +        
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR, color = "Aggravated Assault Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR, color = "Aggravated Assault Rate")) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[2]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12))
  }
  #ALL PROPERTY CRIMES
  else if (var == 3){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=PCR, color = "Property Crime Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR, color = "Burglary Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR, color = "Burglary Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR, color = "Larceny-Theft Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR, color = "Larceny-Theft Rate")) +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR, color = "Motor Vehicle Theft Rate")) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR, color = "Motor Vehicle Theft Rate")) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[3]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12))
  }
  #POPULATION
  else if (var == 4){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=POP)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=POP)) +
      labs(x = XLab, y = YLabPop) +
      ggtitle(Title[4]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #MURDER AND MANSLAUGHTER
  else if (var == 5){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MURDERR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[5]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #RAPE LEGACY
  else if (var == 9){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=RAPELEGR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[6]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #ROBBERY
  else if (var == 11){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=ROBR)) +        
      labs(x = XLab, y = YLab) +
      ggtitle(Title[7]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #AGGRAVATED ASSAULT
  else if (var == 13){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=AGGASSAUR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[8]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #BURGLARY
  else if (var == 17){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=BURGR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[9]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #LARCENY-THEFT
  else if (var == 19){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=LTR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[10]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  #MOTOR VEHICLE THEFT
  else if (var == 21){
    ggplot() +
      geom_point(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR)) +
      geom_line(data = dat[syear:eyear,1:ncol(dat)],aes(x=YEAR,y=MVTR)) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[11]) +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  }
  
  #------------- END DATA SELECTION -------------#
  
}
