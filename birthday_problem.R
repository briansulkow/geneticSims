##find out if the people two or more people have the same birthday. 
library(ggplot2)
birthday <- function(pop_size){
  bds <- rep(0,pop_size)
  for(i in 1:pop_size){
    bds[i] <- sample(1:365,1)
  }
  return(bds)
}

birthdayProb<- function(pop_size , num_sims){
  
  no_match <- 0
  
  for(i in 1:num_sims)
  {
    bd <- birthday(pop_size)
    if(length(unique(table(bd)))== 1)
    {
      no_match <- no_match+1
      }
    
  }
  
  return(1-(no_match/num_sims))
}


probArray <- function(pop_size, num_sims,num_exp){
  bProbsArray <- rep(0,num_exp)
  
  for(i in 1: num_exp){
    bProbsArray[i] <- birthdayProb(pop_size,num_sims)
  }
  ind <- 1:length(bProbsArray)
  df <- data.frame(ind, bProbsArray)
  return (df)
  }
pArray <- probArray(23, 1000, 500) 
dim(pArray)
summary(pArray) 
sd(pArray[,2])
hist(pArray[,2])
head(pArray)

ggplot(data = pArray,aes(x=bProbsArray, col="red"))+ geom_histogram(binwidth = .003) + geom_density()+ xlab("B-day Prob")
+ labs(title = "B-day Probs Pop_Size= 23")

bdaySim <- function(pop_size, niters){
  count = 0
  for(i in 1:niters){
    bdaySamples <- sample(1:365,pop_size, replace =T)
    if(length(bdaySamples)!= length(unique(bdaySamples)))
    {
      count= count + 1 
    }
  }
    return (1- count/niters)
  }


bdaySim(23,500) 
