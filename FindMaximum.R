library(ggplot2)
library("tidyr")


set.seed(999)
q <- runif(100)
(best_candidate_quality <- max(q))
which(q==best_candidate_quality)


best_candidate <- function(r,q){
  #r : stopping point
  #q : vector giving quality of candidates
  
  #Step 1: Find the best candidate  up to the stopping point
  best_1 <- max(q[1:r])
  
  # Step 2: Find the 2nd  candidate after the stopping point  than the best candidate before the stopping point
  best_2 <- which((q[(r+1):length(q)]>=best_1) == TRUE)
  
  selected_candidate <- ifelse(length(best_2)==0,length(q),r+best_2) # if the best2 lengthe value is 0 , then assign the length of the value , else the stoppingpoint + value 
  selected_candidate_value <- q[selected_candidate]
  
  return(c(selected_candidate,selected_candidate_value))
  
}

experiment <- function(r,N){
  #N: No of experiments to run
  #r: stopping point to be tested


  mat <- matrix(ncol=1, nrow=N)
  
  
  for (i in seq_len(N)){
    q_new <- sample(q)
    # print(q_new)
    if (best_candidate(r,q_new)[2]==best_candidate_quality) # if the best candidate is the highest among the sample, add it to a new value
      
    {
      candid <- best_candidate(r,q_new)[1]
      mat[i,] <- c(candid) 
         
    }   
   }
 da <- mat
  print(da)
  head(na.omit(da))
}
stop_points <- c(30) 
experiment(stop_points,1000)



