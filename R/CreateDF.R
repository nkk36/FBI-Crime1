CreateDF <- function(syear,eyear,var,dat){
  
  #------------- SET COLUMN LABELS IN TABLE -------------#
  
  Header = c("Year","Population", "Number of Violent Crimes", "Violent Crime Rate", "Number of Property Crimes", "Property Crime Rate")
  
  #------------- SET COLUMN LABELS IN TABLE -------------#
  
  #CHECK IF THE USER SELECTS NO CRIMES FOR DISPLAY. THE DEFAULT IS FOR BOTH (VIOLENT CRIME & PROPERTY CRIME)TO DISPLAY
  if (is.null(var)){
    var <- c(3,15)
  }
  
  #------------- BEGIN DATA SELECTION -------------#
  
  if (length(var) == 2){
    dat[syear:eyear,2] <- formatC(dat[syear:eyear,2],format = "d", big.mark = ",")
    dat[syear:eyear,3] <- formatC(dat[syear:eyear,3],format = "d", big.mark = ",")
    dat[syear:eyear,15] <- formatC(dat[syear:eyear,15],format = "d", big.mark = ",")
    data <- dat[syear:eyear,c(1,2,3,4,15,16)]
    names(data)[1:6] <- c(Header[1], Header[2], Header[3], Header[4], Header[5], Header[6]) 
  }
  else if (var == 3){
    dat[syear:eyear,2] <- formatC(dat[syear:eyear,2],format = "d", big.mark = ",")
    dat[syear:eyear,3] <- formatC(dat[syear:eyear,3],format = "d", big.mark = ",")
    data <- dat[syear:eyear,c(1,2,3,4)] 
    names(data)[1:4] <- c(Header[1], Header[2], Header[3], Header[4]) 
  }
  else if (var == 15){
    dat[syear:eyear,2] <- formatC(dat[syear:eyear,2],format = "d", big.mark = ",")
    dat[syear:eyear,15] <- formatC(dat[syear:eyear,15],format = "d", big.mark = ",")
    data <- dat[syear:eyear,c(1,2,15,16)] 
    names(data)[1:4] <- c(Header[1], Header[2], Header[5], Header[6]) 
  }
  
  #------------- END DATA SELECTION -------------#
  
  #RETURN DATA
  return(data)  
}