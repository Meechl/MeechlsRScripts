# Generate mega data table
# This is mostly for show, just in case something happens to my mega table
  # You should be able to load the previous mega table into R, and then just add rows
  # For the current round, using MegaTableAddOn.r
# Note that for rounds that were rerun, only one of the runs was added to the table. 
  # I chose whichever round had best signal to noise and all columns filled.
  # The other rounds are commented out 

# Load data for all rounds
# add a row specifying what round it is
  # Note the ".0" at the end of rounds. It's important!!!
Rall=R00[,c(1:12)]
Rall$Round = rep("R00.0",length(R00[,1]))
x = R69[,c(1:12)]
x$Round = rep("R69.0",length(R69[,1]))
Rall = rbind(Rall,x)
x = R71[,c(1:12)]
x$Round = rep("R71.0",length(R71[,1]))
Rall = rbind(Rall,x)
x = R72[,c(1:12)]
x$Round = rep("R72.0",length(R72[,1]))
Rall = rbind(Rall,x)
#x = R73[,c(1:12)]
#x$Round = rep("R73.0",length(R73[,1]))
#Rall = rbind(Rall,x)
x = R73.1[,c(1:12)]
x$Round = rep("R73.1",length(R73.1[,1]))
Rall = rbind(Rall,x)
x = R74[,c(1:12)]
x$Round = rep("R74.0",length(R74[,1]))
Rall = rbind(Rall,x)
x = R75[,c(1:12)]
x$Round = rep("R75.0",length(R75[,1]))
Rall = rbind(Rall,x)
x = R76[,c(1:12)]
x$Round = rep("R76.0",length(R76[,1]))
Rall = rbind(Rall,x)
x = R76.1[,c(1:12)]
x$Round = rep("R76.1",length(R76.1[,1]))
Rall = rbind(Rall,x)
x = R77.2[,c(1:12)]
x$Round = rep("R77.2",length(R77.2[,1]))
Rall = rbind(Rall,x)
x = R78.1[,c(1:12)]
x$Round = rep("R78.0",length(R78.1[,1]))
Rall = rbind(Rall,x)
x = R79.1[,c(1:12)]
x$Round = rep("R79.0",length(R79.1[,1]))
Rall = rbind(Rall,x)
x = R80.1[,c(1:12)]
x$Round = rep("R80.0",length(R80.1[,1]))
Rall = rbind(Rall,x)
x = R81.1[,c(1:12)]
x$Round = rep("R81.0",length(R81.1[,1]))
Rall = rbind(Rall,x)
x = R82.1[,c(1:12)]
x$Round = rep("R82.0",length(R82.1[,1]))
Rall = rbind(Rall,x)
#x = R83[,c(1:12)]
#x$Round = rep("R83.0",length(R83[,1]))
#Rall = rbind(Rall,x)
#x = R83.2[,c(1:12)]
#x$Round = rep("R83.2",length(R83.2[,1]))
#Rall = rbind(Rall,x)
x = R83.3[,c(1:12)]
x$Round = rep("R83.3",length(R83.3[,1]))
Rall = rbind(Rall,x)
x = R84[,c(1:12)]
x$Round = rep("R84.0",length(R84[,1]))
Rall = rbind(Rall,x)
x = R85[,c(1:12)]
x$Round = rep("R85.0",length(R85[,1]))
Rall = rbind(Rall,x)
x = R86[,c(1:12)]
x$Round = rep("R86.0",length(R86[,1]))
Rall = rbind(Rall,x)
x = R87[,c(1:12)]
x$Round = rep("R87.0",length(R87[,1]))
Rall = rbind(Rall,x)
x = R87.1[,c(1:12)]
x$Round = rep("R87.1",length(R87.1[,1]))
Rall = rbind(Rall,x)
x = R87.2[,c(1:12)]
x$Round = rep("R87.2",length(R87.2[,1]))
Rall = rbind(Rall,x)
x = R88[,c(1:12)]
x$Round = rep("R88.0",length(R88[,1]))
Rall = rbind(Rall,x)
x = R88.2[,c(1:12)]
x$Round = rep("R88.2",length(R88.2[,1]))
Rall = rbind(Rall,x)
x = R89[,c(1:12)]
x$Round = rep("R89.0",length(R89[,1]))
Rall = rbind(Rall,x)
x = R90[,c(1:12)]
x$Round = rep("R90.0",length(R90[,1]))
Rall = rbind(Rall,x)

# Add fun columns: length and longest length of repeated bases in a sequence
Rall = funColumns(Rall)
funColumns = function(x=Rall){
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
