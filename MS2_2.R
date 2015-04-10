# Include number of basepairs of each type in second state

MS2 = read.table("C:/Users/Meechl/Documents/Eterna/csv/MS2/Full.csv",sep=",",header=TRUE, na.strings=".") #read in data

# Generate a pair map from the sequence
pairMapMS2 = function (data = MS2){
  #ProjectName = c(rep("oops",1000))
  #PairMap = t(data.frame(c(1:length(grep("X",names(data))))))
  #length = length(grep("X",names(data)))
  for (j in 1:length(data[,1])){
    length = nchar(as.character(MS2[i,]$Structure_2))
    map = c(rep(0,length))
    left = c(0,0)
    l = 1
    for (i in 1:length){
      if (substr(data[j,]$Structure_2,i,i) == "("){
        left[l] = i
        #browser()
        l = l+1
      }
      if (substr(data[j,]$Structure_2,i,i) == ")"){
        map[left[length(left)]] = i
        map[i] = left[length(left)]
        left = left[1:length(left)-1]
        l = l-1
        #browser()
      }
    }
    #row.names(PairMap(x) = data[j,]$Project_Name
    if (j == 1){
      PairMap = t(map)
    }
    if (j != 1){
      PairMap = rbind(PairMap,t(map))
    }
  }
  return(PairMap)
}

# Trying to debug...
#x=c(0,0,0)
#y=c(0,0,0)
#for (i in 1:nchar(as.character(MS2[30,]$Structure_2))){
#  x[i] = substr(MS2[30,]$Structure_2,i,i)
#  y[i] = substr(MS2[30,]$Structure_2,i,i) == "("
#}
#x = cbind(x,y)
#for (i in 1:nchar(as.character(MS2[30,]$Structure_2))){
#  y[i] = substr(MS2[30,]$Structure_2,i,i) == ")"
#}
#x = cbind(x,y)
#for (i in 1:nchar(as.character(MS2[30,]$Structure_2))){
#  y[i] = substr(MS2[30,]$Structure_2,i,i) == "."
#}
#x = cbind(x,y)

# determine basepair number for a design, given sequence and pairmap
BPMS2 = function(base1="C", base2="G", Sequences=MS2$Sequence, map=Map){
  counts = c(rep(0,length(Sequences)))
  for (j in 1:length(Sequences)){ # how designs many tested
    #browser()
    x = as.character(Sequences[j])
    for ( i in 1:length(map[1,]) ){ # Length of mapped sequences
      #browser()
      if (map[j,i] != 0){
        if (substr(x,i,i)==base1 && substr(x,map[j,i],map[j,i])==base2){
          counts[j] = counts[j]+1
        }
      }
    }
  }
  return (counts)
}

# make a table with the basepair%'s
allBPMS2 = function(){
  GC_2 = BPMS2()
  AU_2 = BPMS2("A","U")
  GU_2 = BPMS2("G","U")
  total = c(rep("oops",length(GC_2)))
  for (i in 1:length(GC_2)){
    total[i] = GC_2[i] + AU_2[i] + GU_2[i]
  }
  AUpct_2 = as.double(AU_2)/as.double(total)*100
  GCpct_2 = as.double(GC_2)/as.double(total)*100
  GUpct_2 = as.double(GU_2)/as.double(total)*100
  all = cbind(AU_2,GC_2,GU_2,AUpct_2,GCpct_2,GUpct_2,total)
}

#write.table(x, file = "C:/Users/Meechl/Documents/Eterna/csv/Result.csv")

