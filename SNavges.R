# Calculates the average Signal to Noise for each round in the mega table

x=SNavges(Rall) # I like to use x as a sandbox variable that I write over all the time
View(SNavges(Rall)) # This is a nice way to look at tables in R

SNavges = function(Stats=Rall){
  Round = c(rep("oops",1000))
  Round[1] = Stats[1,]$Round
  SNAverage = c(rep("oops",1000))
  SNAverage[1] = as.double(Stats[1,]$Signal_to_Noise)
  j=1
  m=1
  for (i in 1:length(Stats[,1])){
    if (Round[j] != Stats[i,]$Round){
      SNAverage[j] = as.double(SNAverage[j])/m
      j = j+1
      Round[j] = Stats[i,]$Round
      SNAverage[j] = Stats[i,]$Signal_to_Noise
      m = 1
    }
    else{
      SNAverage[j] = sum(as.double(SNAverage[j]),as.double(Stats[i,]$Signal_to_Noise), na.rm = TRUE)
      m = m+1
    }
    if (i == length(Stats[,1])){
      SNAverage[j] = as.double(SNAverage[j])/m
    }
  }
  k = 0
  for ( i in 1:length(Round)){
    if (Round[i] != "oops"){
      k = k+1
    }
  }
  Round = Round[1:k]
  SNAverage = SNAverage[1:k]
  return(cbind(Round,SNAverage))
}

