
#37%rule
sims <- 10000
p <- c()
p[1] <- 0

for (k in 2:999) {
  success <- 0
  for (n in 1:sims) {
    x <- sample(1:1000, 1000)
    init_best <- max(x[1:k])
    candidate_score = -1
    for (i in (k+1):1000) {
      if (x[i] > init_best) {
        candidate_score <- x[i]
        break
      }
    }
    if (candidate_score == 1000)
      success <- success + 1
  }
  p[k] <- success/sims
  cat(k, " complete \n")
}
plot(1:999, p, type = "l", xlab = "Omitted Candidates",
     ylab = "Probability of Best Candidate Selection",
     main = "The Secretary Problem")