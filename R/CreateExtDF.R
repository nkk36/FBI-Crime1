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
  
  dat2 = dat[,c(1,2,3,4,15,16,5,6,7,8,9,10,11,12,13,14,17,18,19,20,21,22)]
  
  if (TRUE %in% (which(colnames(dat2) %in% var) > 6)){
    add = which(colnames(dat2) %in% var)[which(colnames(dat2) %in% var) > 6] + 1
    all = c(var,colnames(dat2)[add])
    sort = sort(all[2:length(all)])
    all = c(all[1], sort)
  }
  
  data = dat2[dat2$YEAR >= as.numeric(syear) & dat2$YEAR <= as.numeric(eyear), all]  
  #------------- END DATA SELECTION -------------#
  
  #RETURN DATA
  return(data)  
}
