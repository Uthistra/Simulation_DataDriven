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
  
  #C : counter to keep track of number of times optimal candidate was selected
  C <- 0
  
  for (i in seq_len(N)){
    q_new <- sample(q)
   # print(q_new)
    if (best_candidate(r,q_new)[2]==best_candidate_quality) # if the best candidate is the highest among the sample, add it to a new value
      
      {
         C <-  C + 1
       }   
    
  }
  
  return(C)
}


stop_points <- c(10,20,30,31,32,33,34,35,37,38,40,50,60,80) 
print(stop_points)

  (results <- sapply(stop_points,experiment,1000)) # among 1000 iterations the best selections on certain stopping points
   df <- data.frame(x=stop_points,y=results)

   ggplot(df,aes(x=x,y=y))+ geom_bar(stat='identity')+labs(x='Omitted Candidates',y='No of Optimal Selections')+
    scale_x_continuous(breaks=stop_points,labels=stop_points)
   
   

#density function
   
   x <- df$x
   par(mfrow = c(1, 2))
   # Create a histogram
   hist(x, freq = FALSE, main = "Histogram and density")
   # Calculate density
   dx <- density(x)
   # Add density
   lines(dx, lwd = 2, col = "red")
   # Plot the density without histogram
   plot(dx, lwd = 2, col = "red",
        main = "Density")
   # Add the data-poins with noise in the X-axis
   rug(jitter(x))
 