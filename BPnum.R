# This function counts the number of A's, U's, G's, or C's in a sequence
BPnum = function( base = "A", sequence = c("AUCG","AUCG")){
  y = c(0,0,0)
  for (i in 1:length(sequence)){
    n = 0
    for (j in 1:nchar(sequence[i])){
      if ( substr(sequence[i],j,j) == base ){
        n = n+1
      }
    }
    y[i] = n
  }
  return(y)
}
