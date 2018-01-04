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
  
  dat2 = dat[,c(1,2,3,4,15,16,5,6,7,8,9,10,11,12,13,14,17,18,19,20,21,22)]

  if (TRUE %in% (which(colnames(dat2) %in% var) > 6)){
    add = which(colnames(dat2) %in% var)[which(colnames(dat2) %in% var) > 6] + 1
  }
  dat2 = dat2[dat2$YEAR >= as.numeric(syear) & dat2$YEAR <= as.numeric(eyear),c(1, add)]
  dat2 = data.table::melt(dat2, "YEAR" , colnames(dat2)[2:1 + length(add) - 1])
  #------------- SET LABELS IN PLOTS -------------#
  
  #------------- BEGIN DATA SELECTION -------------#
  
  ggplot(data = dat2) +
    geom_point(mapping = aes(x = YEAR, 
                             y = value, 
                             fill = variable, 
                             group = variable)) + 
    geom_line(mapping = aes(x = YEAR, 
                            y = value, 
                            fill = variable, 
                            group = variable,
                            colour = variable)) +
    labs(x = XLab, y = YLab) +
    ggtitle(Title[1]) +
    scale_x_continuous(breaks=seq(1996,2015,1)) +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 20), axis.text.x = element_text(size = 12)) 
  #------------- END DATA SELECTION -------------#
  
}
