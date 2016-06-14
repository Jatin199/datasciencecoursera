getwd()
setwd("C:/Users/Jatin/Desktop/courseraa")
getwd()

###############################################################
############### ASSIGNMENT WEEK-2 R Coursera ##################
########################## 1st ################################
pollutantmean<-function(directory,pollutant,id=1:332){
  idIndex<-id
  rawdataset<-combine(idIndex,directory)
  cleandataset<-rawdataset[!is.na(rawdataset[c(pollutant)]),c(pollutant)]
  result<-mean(cleandataset)
}

combine<-function(idIndex,directory){
  startI<-1
  endI<-length(idIndex)
  binded<-data.frame("mean"=integer(0))
  
  while(startI<=endI){
    otherDataset<-read.csv(formatFileName(idIndex[startI],directory),header=TRUE)
    binded<-rbind(binded,otherDataset)
    startI<-startI+1
  }
  binded
}

formatFileName<-function(fileName,directory){
  if(nchar(fileName)==1){
    dir<-paste("./",directory,"/","00",fileName,".csv",sep="")
    
  }
  else if (nchar(fileName)==2){
    dir<-paste("./",directory,"/","0",fileName,".csv",sep="")
  }
  else if (nchar(fileName)==3){
    dir<-paste("./",directory,"/",fileName,".csv",sep="")
  }
}
sample1 <- pollutantmean("specdata", "sulfate", 34)
sample1
sample3 <- pollutantmean("specdata", "nitrate", 23)

############# 2 nd code ##################
complete<-function(directory,id=1:332){
  idIndex<-id
  startI<-1
  endI<-length(idIndex)
  
  completeObs<-data.frame("id"=character(0),"nobs"=integer(0))
  
  while(startI<=endI){
    rawdataset<-read.csv(formatFileName(idIndex[startI],directory),header=TRUE)
    cleandataset<-rawdataset[complete.cases(rawdataset),]
    newObs<-data.frame("id"=idIndex[startI],"nobs"=nrow(cleandataset))
    completeObs<-rbind(completeObs,newObs)
    startI<-startI+1
  }
  
  completeObs
}

formatFileName<-function(fileName,directory){
  if(nchar(fileName)==1){
    dir<-paste("./",directory,"/","00",fileName,".csv",sep="")
    
  }
  else if (nchar(fileName)==2){
    dir<-paste("./",directory,"/","0",fileName,".csv",sep="")
  }
  else if (nchar(fileName)==3){
    dir<-paste("./",directory,"/",fileName,".csv",sep="")
  }
  
}
a<-pollutantmean("specdata", "nitrate")
a

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#### 3rd #############################################
corr<-function(directory,threshold=0){
  completeObs<-complete(directory)
  corVector<-vector()
  val<-1
  stop<-nrow(completeObs)
  
  while(val <= stop){
    if(completeObs[val,c("nobs")]>threshold){
      rawdataset<-read.csv(formatFileName(completeObs[val,c("id")],directory),header=TRUE)
      cleandataset<-rawdataset[complete.cases(rawdataset),]
      x<-cleandataset[,c("sulfate")]
      y<-cleandataset[,c("nitrate")]
      corVector<-c(corVector,cor(x,y))
    }
    val<-val+1
  }
  corVector
}

formatFileName<-function(fileName,directory){
  if(nchar(fileName)==1){
    dir<-paste("./",directory,"/","00",fileName,".csv",sep="")
    
  }
  else if (nchar(fileName)==2){
    dir<-paste("./",directory,"/","0",fileName,".csv",sep="")
  }
  else if (nchar(fileName)==3){
    dir<-paste("./",directory,"/",fileName,".csv",sep="")
  }
  
}


cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
