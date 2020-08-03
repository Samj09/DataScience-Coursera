pollutantmean<-function(directory,pollutant,id=1:332)
{
  files<-list.files(path=directory, pattern = ".csv", full.names = TRUE)
  values<-numeric()
  for(i in id)
  {
      data<-read.csv(files[i])
      values<-c(values,data[[pollutant]])
  }
  mean(values, na.rm=T)
}

complete<-function(directory,id = 1:332)
{
  files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()
  
  for (i in id) {
    data <- read.csv(files[i])
    nobs <- c(nobs, sum(complete.cases(data)))
  }
  data.frame(id, nobs)
}

corr<-function(directory,threshold=0)
{
  files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  correlation<-c()
  for(i in 1:length(files))
  {
    data <- read.csv(files[i])
    no_na<-na.exclude(data)
    
    if(nrow(no_na)>=threshold)
    {
      correlation <- c(correlation, cor(no_na$nitrate, no_na$sulfate))
      
    }
    
  }
  correlation
}