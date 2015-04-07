# Add subsequent rounds to mega table

# Make sure to add ".0" to the name of rounds (or ".1" etc.)
  # That way, if you need to do pattern recognition to select just the first run,
  # R won't want to also grab the reruns
  # Okay, maybe that didn't make much sense... just trust me!
x = R94[,c(1:12)]
x$Round = rep("R94.0",length(R94[,1]))
x = funColumns(x)
Rall = rbind(Rall,x)
funColumns = function(x=x){
	x$Length = c(rep("oops",length(x[,1])))
	for (i in 1:length(x[,1])){
	x[i,]$Length = nchar(x[i,]$Structure)
	}
	x$RepA = c(rep("oops",length(x[,1])))
	x$RepA = repeats("A",x$Sequence)
	x$RepU = c(rep("oops",length(x[,1])))
	x$RepU = repeats("U",x$Sequence)
	x$RepG = c(rep("oops",length(x[,1])))
	x$RepG = repeats("G",x$Sequence)
	x$RepC = c(rep("oops",length(x[,1])))
	x$RepC = repeats("C",x$Sequence)
	return(x)
}
# write.table(Rall, file = "C:/Users/Meechl/Documents/Eterna/csv/Result.csv") 
