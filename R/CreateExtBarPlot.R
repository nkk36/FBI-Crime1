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
  
  #------------- SET LABELS IN PLOTS -------------#
  
  #------------- CALL REQUIRED FUNCTIONS -------------#
  
  source("TakeSomeYears.R")
  
  #------------- CALL REQUIRED FUNCTIONS -------------#
  
  #------------- BEGIN DATA SELECTION -------------#
  
  #ALL CRIMES
  if (var == 1){
    DataYears <- TakeSomeYears(syear,eyear,datStack)
    datStack <- with(DataYears, DataYears[order(YEAR, TYPE, CRIME),])
    ggplot(data=datStack, aes(x=CRIME, y=AMOUNT, fill=TYPE)) + 
      geom_bar(stat="identity") + 
      scale_y_continuous(labels = scales::comma) +
      labs(y = YLab) + 
      facet_grid(~YEAR) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), text = element_text(size = 20))
  }
  #ALL VIOLENT CRIMES
  else if (var == 2){
    datf <-  melt(dat[syear:eyear,c(1,5,9,11,13)],id.vars="YEAR")
    ggplot(datf, aes(x = YEAR, y = value, fill = variable)) + 
      geom_bar(stat = "identity") + 
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[2]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) +
      scale_fill_discrete(name=LegendTitle,labels = c(Header[4], Header[5],
                                                      Header[6], Header[7])
      )
  }
  #ALL PROPERTY CRIMES
  else if (var == 3){
    datf <-  melt(dat[syear:eyear,c(1,17,19,21)],id.vars="YEAR")
    ggplot(datf, aes(x = YEAR, y = value, fill = variable)) + 
      geom_bar(stat = "identity") + 
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[3]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) +
      scale_fill_discrete(name=LegendTitle,labels = c(Header[8],Header[9],
                                                      Header[10]))
  }
  #POPULATION
  else if (var == 4){
    df.m <- melt(dat[syear:eyear,c(1,2)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[4]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=PopulationTitle,labels = Header[3])+
    #geom_text(aes(label=dat$POP), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #MURDER AND MANSLAUGHTER
  else if (var == 5){
    df.m <- melt(dat[syear:eyear,c(1,5)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[5]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[4])+
    #geom_text(aes(label=dat$MURDER), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #RAPE LEGACY DEFINITION
  else if (var == 9){
    df.m <- melt(dat[syear:eyear,c(1,9)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[6]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[5])+
    #geom_text(aes(label=dat$RAPELEG), position=position_dodge(width=0.9), vjust=-0.25)
    
  }
  #ROBBERY
  else if (var == 11){
    df.m <- melt(dat[syear:eyear,c(1,11)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[7]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[6])+
    #geom_text(aes(label=dat$ROB), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #AGGRAVATED ASSAULT
  else if (var == 13){
    df.m <- melt(dat[syear:eyear,c(1,13)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[8]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[7])+
    #geom_text(aes(label=dat$AGGASSAU), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #BURGLARY
  else if (var == 17){
    df.m <- melt(dat[syear:eyear,c(1,17)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[9]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[8])+
    #geom_text(aes(label=dat$BURG), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #LARCENY-THEFT
  else if (var == 19){
    df.m <- melt(dat[syear:eyear,c(1,19)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[10]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+ 
    #scale_fill_discrete(name=LegendTitle,labels = Header[9])+
    #geom_text(aes(label=dat$LT), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  #MOTOR VEHICLE THEFT
  else if (var == 21){
    df.m <- melt(dat[syear:eyear,c(1,21)],id.vars='YEAR')
    ggplot(df.m, aes(YEAR, value)) +
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_x_continuous(breaks=seq(1996,2015,1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = XLab, y = YLab) +
      ggtitle(Title[11]) +
      theme(text = element_text(size = 20), axis.text.x = element_text(size = 12), legend.position = "none") #+
    #scale_fill_discrete(name=LegendTitle,labels = Header[10])+
    #geom_text(aes(label=dat$MVT), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  }
  
  #------------- END DATA SELECTION -------------#
  
}
