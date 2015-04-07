# Format Shape data from RMDB
# Make sure all # and " and ' and , are changed to space

Stats = read.table("C:/Users/Meechl/Documents/Eterna/csv/R94unstack.csv",sep=",",header=TRUE, na.strings=".") # Replace the file location with the location of your .csv file
R94 = formatShape(Stats) # This runs the below function and saves the data in R as a dataframe called R94
write.table(R94, file = "C:/Users/Meechl/Documents/Eterna/csv/Result.csv") # This will write formatted file to a location of your choosing

formatShape = function(data){           # Column Names:
  data[,1]  = substr(data[,1],17,1000)  # Annotation 
  data[,2]  = substr(data[,2],20,1000)  # Design_Name
  data[,3]  = substr(data[,3],21,1000)  # Project_Name
  data[,4]  = substr(data[,4],11,1000)  # Sequence_ID
  data[,5]  = substr(data[,5],10,1000)  # Sequence
  data[,6]  = substr(data[,6],11,1000)  # Structure
  data[,7]  = substr(data[,7],17,1000)  # Signal_to_Noise # Only keep the number
  data[,8]  = substr(data[,8],27,1000)  # Score
  data[,9]  = substr(data[,9],24,1000)  # Score_Min
  data[,10] = substr(data[,10],24,1000) # Score_Max
  data[,11] = substr(data[,11],30,1000) # Score_Thresh
  data$SNrating = data[,7] # Makes a column for the character half of S/N: <1 weak, 1-5 medium, >5 good (?)
  for (i in 1:length(data[,1])){
    if (substr(data[i,7],1,1) == "w" || substr(data[i,7],1,1) == "g"){
      data[i,]$SNrating = substr(data[i,7],1,4)
      data[i,7] = substr(data[i,7],6,1000)
    }
    if (substr(data[i,7],1,1) == "m" || substr(data[i,7,],1,1) == "s"){  
      data[i,]$SNrating = substr(data[i,7],1,6)
      data[i,7] = substr(data[i,7],8,1000)
    }
    if (substr(data[i,7],1,1) == "N"){
      data[i,7] = NA
      data[i,]$SNrating = NA
    }
  }
  datum = data[,c(1:7,length(data),9:length(data)-1)]
  return(datum)
}