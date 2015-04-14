# Takes the MS2 data generated from this script: http://nando.eternadev.org/web/script/3386853/
# Adds columns for the number of each type of basepair in state 1 and state 2

MS2 = read.table("C:/Users/Meechl/Documents/Eterna/csv/MS2/R93_all.csv",sep=",",header=TRUE, na.strings=".") # Read in data
Map = pairMapMS2() # Generate pair map for the first state
Map_2 = pairMapMS2_2() # Generate pair map for the second state
x = allBPMS2() # Make a table with number and percent of each basepair
write.table(x, file = "C:/Users/Meechl/Documents/Eterna/csv/Result.csv") # Write results to external table

# Generate a pair map from the predicted structure
pairMapMS2 = function (data = MS2){
  for (j in 1:length(data[,1])){
    length = nchar(as.character(MS2[i,]$Structure))
    map = c(rep(0,length))
    left = c(0,0)
    l = 1
    for (i in 1:length){
      if (substr(data[j,]$Structure,i,i) == "("){
        left[l] = i
        l = l+1
      }
      if (substr(data[j,]$Structure,i,i) == ")"){
        map[left[length(left)]] = i
        map[i] = left[length(left)]
        left = left[1:length(left)-1]
        l = l-1
      }
    }
    if (j == 1){
      PairMap = t(map)
    }
    if (j != 1){
      PairMap = rbind(PairMap,t(map))
    }
  }
  return(PairMap)
}
pairMapMS2_2 = function (data = MS2){
  for (j in 1:length(data[,1])){
    length = nchar(as.character(MS2[i,]$Structure_2))
    map = c(rep(0,length))
    left = c(0,0)
    l = 1
    for (i in 1:length){
      if (substr(data[j,]$Structure_2,i,i) == "("){
        left[l] = i
        l = l+1
      }
      if (substr(data[j,]$Structure_2,i,i) == ")"){
        map[left[length(left)]] = i
        map[i] = left[length(left)]
        left = left[1:length(left)-1]
        l = l-1
      }
    }
    if (j == 1){
      PairMap = t(map)
    }
    if (j != 1){
      PairMap = rbind(PairMap,t(map))
    }
  }
  return(PairMap)
}

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
  GC = BPMS2()
  AU = BPMS2("A","U")
  GU = BPMS2("G","U")
  total = c(rep("oops",length(GC)))
  for (i in 1:length(GC)){
    total[i] = GC[i] + AU[i] + GU[i]
  }
  AUpct = as.double(AU)/as.double(total)*100
  GCpct = as.double(GC)/as.double(total)*100
  GUpct = as.double(GU)/as.double(total)*100
  
  GC_2 = BPMS2(map=Map_2)
  AU_2 = BPMS2("A","U",map=Map_2)
  GU_2 = BPMS2("G","U",map=Map_2)
  total_2 = c(rep("oops",length(GC_2)))
  for (i in 1:length(GC_2)){
    total_2[i] = GC_2[i] + AU_2[i] + GU_2[i]
  }
  AUpct_2 = as.double(AU_2)/as.double(total_2)*100
  GCpct_2 = as.double(GC_2)/as.double(total_2)*100
  GUpct_2 = as.double(GU_2)/as.double(total_2)*100
  all = cbind(AU,GC,GU,AUpct,GCpct,GUpct,AU_2,GC_2,GU_2,AUpct_2,GCpct_2,GUpct_2)
  return (all)
}

