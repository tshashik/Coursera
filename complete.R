complete <- function(directory, id = 1:322) {
  setwd(paste0("/Users/tanvishashikant/Desktop/Coursera","/",directory))
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
  print (output)
}