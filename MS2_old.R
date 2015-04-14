

# Number of AU/GC/GU pairs, melt point, free E, designer name
MS2 = read.table("C:/Users/Meechl/Documents/Eterna/csv/MS2.csv",sep=",",header=TRUE, na.strings=".") #read in data
MS2in = function (R = MS2){
  R[,4] = as.double(substr(R[,4],1,2)) #AU pairs
  R[,5] = as.double(substr(R[,5],1,2)) #GC pairs
  R[,6] = as.double(substr(R[,6],1,2)) #GU pairs
  R$AUpct = R[,4]/34*100
  R$GCpct = R[,5]/34*100
  R$GUpct = R[,6]/34*100
  R$Melt_Point = as.double(substr(R$Melt_Point,1,2))
  x = c(0,0,0)
  for (i in 1:length(R[,1])){
    x[i] = substr(R[i,]$Free_E,1,nchar(as.character(R[i,]$Free_E))-4)
  }
  R$Free_E = x
  # Add number of A, U, G, C
  R$A = c(rep("oops",length(R[,1])))
  R$A = BPnum("A",as.vector(R$Sequence))
  R$U = c(rep("oops",length(R[,1])))
  R$U = BPnum("U",as.vector(R$Sequence))
  R$G = c(rep("oops",length(R[,1])))
  R$G = BPnum("G",as.vector(R$Sequence))
  R$C = c(rep("oops",length(R[,1])))
  R$C = BPnum("C",as.vector(R$Sequence))
  R$length = c(rep("oops",length(R[,1])))
  for (i in 1:length(R[,1])){
    R[i,]$length = nchar(as.character(R[i,]$Sequence))
  }
  R$Apct = as.double(R$A)/as.double(R$length)*100
  R$Upct = as.double(R$U)/as.double(R$length)*100
  R$Gpct = as.double(R$G)/as.double(R$length)*100
  R$Cpct = as.double(R$C)/as.double(R$length)*100
  return(R)  
}

write.table(MS2in(MS2), file = "C:/Users/Meechl/Documents/Eterna/csv/Result.csv")

MS2 = MS2in(MS2)

par(mfrow = c(2,2))
plot(MS2[grep("Ex1",MS2$Sublab),]$Apct, MS2[grep("Ex1",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex1", )
plot(MS2[grep("Ex1",MS2$Sublab),]$Upct, MS2[grep("Ex1",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex1", )
plot(MS2[grep("Ex1",MS2$Sublab),]$Gpct, MS2[grep("Ex1",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex1", )
plot(MS2[grep("Ex1",MS2$Sublab),]$Cpct, MS2[grep("Ex1",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex1", )

plot(MS2[grep("Ex2",MS2$Sublab),]$Apct, MS2[grep("Ex2",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex2", )
plot(MS2[grep("Ex2",MS2$Sublab),]$Upct, MS2[grep("Ex2",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex2", )
plot(MS2[grep("Ex2",MS2$Sublab),]$Gpct, MS2[grep("Ex2",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex2", )
plot(MS2[grep("Ex2",MS2$Sublab),]$Cpct, MS2[grep("Ex2",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex2", )

plot(MS2[grep("Ex3",MS2$Sublab),]$Apct, MS2[grep("Ex3",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex3", )
plot(MS2[grep("Ex3",MS2$Sublab),]$Upct, MS2[grep("Ex3",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex3", )
plot(MS2[grep("Ex3",MS2$Sublab),]$Gpct, MS2[grep("Ex3",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex3", )
plot(MS2[grep("Ex3",MS2$Sublab),]$Cpct, MS2[grep("Ex3",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex3", )

plot(MS2[grep("Ex4",MS2$Sublab),]$Apct, MS2[grep("Ex4",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex4", )
plot(MS2[grep("Ex4",MS2$Sublab),]$Upct, MS2[grep("Ex4",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex4", )
plot(MS2[grep("Ex4",MS2$Sublab),]$Gpct, MS2[grep("Ex4",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex4", )
plot(MS2[grep("Ex4",MS2$Sublab),]$Cpct, MS2[grep("Ex4",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "Ex4", )

plot(MS2[grep("SS1",MS2$Sublab),]$Apct, MS2[grep("SS1",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS1", )
plot(MS2[grep("SS1",MS2$Sublab),]$Upct, MS2[grep("SS1",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS1", )
plot(MS2[grep("SS1",MS2$Sublab),]$Gpct, MS2[grep("SS1",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS1", )
plot(MS2[grep("SS1",MS2$Sublab),]$Cpct, MS2[grep("SS1",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS1", )

plot(MS2[grep("SS2",MS2$Sublab),]$Apct, MS2[grep("SS2",MS2$Sublab),]$Eterna_Score, xlab = "Percent A", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS2", )
plot(MS2[grep("SS2",MS2$Sublab),]$Upct, MS2[grep("SS2",MS2$Sublab),]$Eterna_Score, xlab = "Percent U", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS2", )
plot(MS2[grep("SS2",MS2$Sublab),]$Gpct, MS2[grep("SS2",MS2$Sublab),]$Eterna_Score, xlab = "Percent G", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS2", )
plot(MS2[grep("SS2",MS2$Sublab),]$Cpct, MS2[grep("SS2",MS2$Sublab),]$Eterna_Score, xlab = "Percent C", ylab = "Eterna Score", ylim = c(0,100), xlim = c(0,100), main = "SS2", )

par(mfrow = c(1,1))
plot(MS2$Apct,MS2$Eterna_Score)
