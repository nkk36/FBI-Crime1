CreateDF <- function(syear,eyear,var,dat){
  
  #------------- SET COLUMN LABELS IN TABLE -------------#
  
  Header = c("Year","Population", "Number of Violent Crimes", "Violent Crime Rate", "Number of Property Crimes", "Property Crime Rate")
  
  dat = dat[dat$YEAR >= as.numeric(syear) & dat$YEAR <= as.numeric(eyear),]
  
  #------------- SET COLUMN LABELS IN TABLE -------------#
  
  #CHECK IF THE USER SELECTS NO CRIMES FOR DISPLAY. THE DEFAULT IS FOR BOTH (VIOLENT CRIME & PROPERTY CRIME)TO DISPLAY
  if (is.null(var)){
    var <- c(3,15)
  }
  
  #------------- BEGIN DATA SELECTION -------------#
  
  if (length(var) == 2){
    data = dat[, c("YEAR", "POP", "VC", "VCR", "PC", "PCR")]
    names(data)[1:6] <- c(Header[1], Header[2], Header[3], Header[4], Header[5], Header[6]) 
  }
  else if (var == "VC"){
    data = dat[, c("YEAR", "POP", "VC", "VCR")]
    names(data)[1:4] <- c(Header[1], Header[2], Header[3], Header[4]) 
  }
  else if (var == "PC"){
    data = dat[, c("YEAR", "POP", "PC", "PCR")]
    names(data)[1:4] <- c(Header[1], Header[2], Header[5], Header[6]) 
  }
  
  #------------- END DATA SELECTION -------------#
  
  #RETURN DATA
  return(data)  
}