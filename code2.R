# Lucas Silva 916213897

# Problem 1 
# Problem 2

sim2 <- function(r,s,nreps){
  count <- 0
  index <- 0
  values <- rep(0, s + 1)
  
  for(rep in 1:nreps){
    consecutiveNums <- 0
    
    # For k in range 3 to 6
    # Basically we need to calculate probability of getting 3 heads for...
    # ... 3 rolls, 4 rolls, 5 rolls, 6 rolls.
    for(k in r:s){
      
      # For i in range 1 to k
      # Simulates flipping the coin k times depending on the iteration.
      for(i in 1:k){
        # 1 = heads; 2 = tails
        toss <- sample(0:1,1, replace = TRUE)
        if(toss == 1){
          consecutiveNums <- consecutiveNums + 1
          if(consecutiveNums == r){
            count <- count + 1
            break
          }
        }else{
          consecutiveNums = 0
        }
      }
      values[k] <- values[k] + count
      count = 0
      consecutiveNums = 0
    }
  }
  
  print(values)
  expectedValue <- 0
  for(i in r:s){
    expectedValue <- expectedValue + (i * (values[i]/nreps))
  }
  cat("E(X) = ", expectedValue)
}

#sim2(3,6,100000)

# Problem 3
probability <- rep(0, 11)
print(probability)
p <- 0.97
trials <- 10
for(i in 1:11){
  # cat("Probability index = ",i, "Is equal to =>", dbinom(i-1,trials,p))
  probability[i] <- dbinom(i-1,trials,p)
}

x <- 0:10
plot(x , probability,col="blue", main = "Problem #5", xlab = "Number of Chips Passing", ylab = "Probability")
axis(side=2, at=seq(0.0,1.0,.1))
