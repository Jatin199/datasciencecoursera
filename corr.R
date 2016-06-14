getwd()
setwd("C:/Users/Jatin/Desktop/courseraa")
getwd()

###############################################################
############### ASSIGNMENT WEEK-2 R Coursera ##################

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
