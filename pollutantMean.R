pollutantMean <- function(directory, pollutant, id = 1:322) {
      toset <- paste0("/Users/tanvishashikant/Desktop/Coursera","/",directory)
      if (!identical(toset,getwd())){
        setwd(toset)
      }
      allMeans <- vector()
    for (i in id) {
        if (i < 10){
          file <- paste0("00",i,".csv")
        }else if (i > 9 && i < 100){
          file <- paste0("0",i,".csv")
        }
        else {
          file <- paste0(i,".csv")
        }
        
        data <- read.csv(file)
        meanColumn <- data[,pollutant]
        allMeans <- c(allMeans,meanColumn)
    }
      
  print(mean(allMeans,na.rm = TRUE))
}
