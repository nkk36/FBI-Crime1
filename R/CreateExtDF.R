CreateExtDF <- function(syear,eyear,var,dat){
  
  #------------- SET COLUMN LABELS IN TABLE -------------#
  
  Header = c("Year","Population", 
             "Number of Violent Crimes", "Violent Crime Rate", 
             "Number of Murders","Murder Rate",
             "Number of Rapes (Legacy definition)","Rape Rate (Legacy definition)",
             "Number of Robberies", "Robbery Rate",
             "Number of Aggravated Assaults", "Aggravated Assault Rate",
             "Number of Property Crimes", "Property Crime Rate",
             "Number of Burglaries", "Burglary Rate",
             "Number of Larceny-Thefts", "Larceny-Theft Rate",
             "Number of Motor Vehicle Thefts", "Motor Vehicle Theft Rate"
  )
  
  #------------- SET COLUMN LABELS IN TABLE -------------#  
  
  #------------- BEGIN DATA SELECTION -------------#
  
  #ALL CRIMES
  if (var == 1){
    data <- dat[syear:eyear,c(1,2,3,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21,22)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    data[,5] <- formatC(data[,5],format = "d", big.mark = ",")
    data[,7] <- formatC(data[,7],format = "d", big.mark = ",")
    data[,9] <- formatC(data[,9],format = "d", big.mark = ",")
    data[,11] <- formatC(data[,11],format = "d", big.mark = ",")
    data[,13] <- formatC(data[,13],format = "d", big.mark = ",")
    data[,15] <- formatC(data[,15],format = "d", big.mark = ",")
    data[,17] <- formatC(data[,17],format = "d", big.mark = ",")
    data[,19] <- formatC(data[,19],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2], 
                                   Header[3], Header[4], 
                                   Header[5], Header[6],
                                   Header[7], Header[8],
                                   Header[9], Header[10],
                                   Header[11], Header[12],
                                   Header[13], Header[14],
                                   Header[15], Header[16],
                                   Header[17], Header[18],
                                   Header[19], Header[20]
    ) 
  }
  #ALL VIOLENT CRIMES
  else if (var == 2){
    data <- dat[syear:eyear,c(1,2,3,4,5,6,9,10,11,12,13,14)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    data[,5] <- formatC(data[,5],format = "d", big.mark = ",")
    data[,7] <- formatC(data[,7],format = "d", big.mark = ",")
    data[,9] <- formatC(data[,9],format = "d", big.mark = ",")
    data[,11] <- formatC(data[,11],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[3], Header[4], 
                                   Header[5], Header[6],
                                   Header[7], Header[8],
                                   Header[9], Header[10],
                                   Header[11], Header[12]
    ) 
  }
  #ALL PROPERTY CRIMES
  else if (var == 3){
    data <- dat[syear:eyear,c(1:2,15:22)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    data[,5] <- formatC(data[,5],format = "d", big.mark = ",")
    data[,7] <- formatC(data[,7],format = "d", big.mark = ",")
    data[,9] <- formatC(data[,9],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[13], Header[14],
                                   Header[15], Header[16],
                                   Header[17], Header[18],
                                   Header[19], Header[20]
    )     
  }
  #POPULATION
  else if (var == 4){
    data <- dat[syear:eyear,c(1,2)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2]
    )     
  }
  #MURDER AND MANSLAUGHTER
  else if (var == 5){
    data <- dat[syear:eyear,c(1:2,5:6)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[5], Header[6]
    )     
  }
  #RAPE LEG
  else if (var == 9){
    data <- dat[syear:eyear,c(1:2,9:10)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[7], Header[8]
    )     
  }
  #ROBBERY
  else if (var == 11){
    data <- dat[syear:eyear,c(1:2,11:12)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[9], Header[10]
    )     
  }
  #AGGRAVATED ASSAULT
  else if (var == 13){
    data <- dat[syear:eyear,c(1:2,13:14)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[11], Header[12]
    )     
  }
  #BURGLARY
  else if (var == 17){
    data <- dat[syear:eyear,c(1:2,17:18)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[15], Header[16]
    )     
  }
  #LARCENY-THEFT
  else if (var == 19){
    data <- dat[syear:eyear,c(1:2,19:20)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[17], Header[18]
    )     
  }
  #MOTOR VEHICLE THEFT
  else if (var == 21){
    data <- dat[syear:eyear,c(1:2,21:22)] 
    data[,2] <- formatC(data[,2],format = "d", big.mark = ",")
    data[,3] <- formatC(data[,3],format = "d", big.mark = ",")
    names(data)[1:ncol(data)] <- c(Header[1], Header[2],
                                   Header[19], Header[20]
    )     
  }
  
  #------------- END DATA SELECTION -------------#
  
  #RETURN DATA
  return(data)  
}
