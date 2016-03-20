complete <- function(directory, id = 1:332) {
  setwd(paste0("/Users/tanvishashikant/Desktop/Coursera/",directory))
  output <- data.frame()
  nobs <- vector()
  ids <- vector()
  for (i in id){
    if (i <10) {
      file <- read.csv(paste0("00",i,".csv"))
    } else if (i > 9 && i < 100) {
      file <- read.csv(paste0("0",i,".csv"))
    } else {
      file <- read.csv(paste0(i,".csv"))
    }
    complete <- complete.cases(file)
    number <- sum(complete)
    nobs <- c(nobs,number)
    ids <- c(ids,i)
  } 
  output <- as.data.frame(cbind(ids,nobs))
}

corr <- function(directory,threshold=0) {
  output <- complete(directory)
  idstoextract <- vector()
  for (i in 1:nrow(output)){
    if (output$nobs[i] > threshold) {
      idstoextract <- c(idstoextract,output$ids[i]) 
    }
  }
  sulfate <- vector()
  nitrate <- vector()
  correlateOutput <- vector()
  for (i in idstoextract){
    if (i <10) {
      file <- read.csv(paste0("00",i,".csv"))
    } else if (i > 9 && i < 100) {
      file <- read.csv(paste0("0",i,".csv"))
    } else {
      file <- read.csv(paste0(i,".csv"))
    }
    
    sulfate <- file$sulfate
    nitrate <- file$nitrate
    correlate <- cor(sulfate,nitrate,use="complete.obs")
    correlateOutput <- c(correlateOutput,correlate)
  }

  correlateOutput
}

