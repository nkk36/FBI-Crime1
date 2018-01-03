TakeSomeYears <- function(syear,eyear,dat){
  
  if (syear == 2){
    syear == 8
  }
  else if (syear == 3){
    syear = 15
  }
  else if (syear == 4){
    syear = 22
  }
  else if (syear == 5){
    syear = 29
  }
  else if (syear == 6){
    syear = 36
  }
  else if (syear == 7){
    syear = 43
  }
  else if (syear == 8){
    syear = 50
  }
  else if (syear == 9){
    syear = 57
  }
  else if (syear == 10){
    syear = 64
  }
  else if (syear == 11){
    syear = 71
  }
  else if (syear == 12){
    syear = 78
  }
  else if (syear == 13){
    syear = 85
  }
  else if (syear == 14){
    syear = 92
  }
  else if (syear == 15){
    syear = 99
  }
  else if (syear == 16){
    syear = 106
  }
  else if (syear == 17){
    syear = 113
  }
  else if (syear == 18){
    syear = 120
  }
  else if (syear == 19){
    syear = 127
  }
  else if (syear == 20){
    syear = 134
  }
  
  if (eyear == 1){
    eyear == 7
  }
  else if (eyear == 2){
    eyear = 14
  }
  else if (eyear == 3){
    eyear = 21
  }
  else if (eyear == 4){
    eyear = 28
  }
  else if (eyear == 5){
    eyear = 35
  }
  else if (eyear == 6){
    eyear = 42
  }
  else if (eyear == 7){
    eyear = 49
  }
  else if (eyear == 8){
    eyear = 56
  }
  else if (eyear == 9){
    eyear = 63
  }
  else if (eyear == 10){
    eyear = 70
  }
  else if (eyear == 11){
    eyear = 77
  }
  else if (eyear == 12){
    eyear = 84
  }
  else if (eyear == 13){
    eyear = 91
  }
  else if (eyear == 14){
    eyear = 98
  }
  else if (eyear == 15){
    eyear = 105
  }
  else if (eyear == 16){
    eyear = 112
  }
  else if (eyear == 17){
    eyear = 119
  }
  else if (eyear == 18){
    eyear = 126
  }
  else if (eyear == 19){
    eyear = 133
  }
  else if (eyear == 20){
    eyear = 140
  }
  
  
  dat2 <- dat[syear:eyear,]
  return(dat2)
  
}