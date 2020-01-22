# Lucas Silva 916213897

# Problem 1
spin <- function(){
  spinValue <- sample(0:10,1,replace = TRUE)
  spinValue
}

sim1 <- function(tot = 15, x1, nreps){
  firstSpin <- x1
  count <- 0
  for(rep in 1:nreps){
    secondSpin <- spin()
    # cat("Second Spin = ", secondSpin, "\n")
    thirdSpin <- spin()
    # cat("Third Spin = ", thirdSpin, "\n")
    
    if(firstSpin + secondSpin + thirdSpin == tot){
      count <- count + 1
    }
  }
  count/nreps
}
# END Of Problem 1

# Problem 2
sim2 <- function(p,q,nreps){
  # Counts of collisions based on X1 != X2
  countCollision <- 0
  count0Collision <- 0
  count1Collision <- 0
  count2Collision <- 0
  
  for(i in 1:nreps){
    numsend <- 0
    #Simulate A and B's decision to send in epoch 1.
    for(j in 1:2){
      if(runif(1) < p) numsend <- numsend + 1
    }
    if(numsend == 1){
      # No collision in epoch 1
      countCollision <- countCollision + 0
      X1 <- 1
    } else{
      # Yes collision in epoch 1
      countCollision <- countCollision + 1
      X1 <- 2
    }
    # If X1 = 1 then one node may generate a new message
    numactive <- X1
    # Simulate generating a new message
    if(X1 == 1 && runif(1) < q) numactive <- numactive + 1
    # Send?
    if(numactive == 1){
      # Simulate single active node sending a message 
      if(runif(1) < p){
        X2 <- 0
      }else{
        X2 <- 1
      }
    }else{ # numactive == 2
      numsend <- 0
      # Simulate A and B's decision to send
      for(j in 1:2){
        if(runif(1) < p) numsend <- numsend + 1
      }
      
      if(numsend == 1){
        # No collision on epoch 2 aka one node sends
        countCollision <- countCollision + 0
        X2 <- 1
      }else{
        # Yes collision on epoch 2 aka none send
        countCollision <- countCollision + 1
        X2 <- 2
      }
    }
    
    # Condition X1 not equal to X2
    if(X1 != X2){
      if(countCollision == 0){
        count0Collision = count0Collision + 1
      }else if(countCollision == 1){
        count1Collision <- count1Collision + 1
      }else{
        count2Collision <- count2Collision + 1
      }
    }else{
      # Do Nothing
    }
    # Reset the collision count at the end
    countCollision <- 0
  }
  
  cat("Probability of 0 Collisions = ", count0Collision/nreps, "\n")
  cat("Probability of 1 Collision = ", count1Collision/nreps, "\n")
  cat("Probability of 2 Collisions = ", count2Collision/nreps, "\n")
}

sim2(.4, .8, 1000)
# END Of Problem 2

# Problem 3
sim3 <- function(nreps){
  
  # Probability of having two aces in your hand
  count2Aces <- 0
  for(rep in 1:nreps){
    hand <- sample(1:52, 5, replace = FALSE)
    aces <- intersect(1:4, hand)
    if(length(aces) == 2) count2Aces <- count2Aces + 1
  }
  
  cat("Probability Of Having 2 Aces: ", count2Aces/nreps, "\n")
  
  # Probability of having 3 diamonds in your hand
  count3Diamonds <- 0
  for(rep in 1:nreps){
    hand <- sample(1:52,5, replace = FALSE)
    diamonds <- intersect(1:13, hand)
    if(length(diamonds) == 3) count3Diamonds <- count3Diamonds + 1
  }
  
  cat("Probability Of Having 3 Diamonds: ", count3Diamonds/nreps, "\n")
  
}

# END Of Problem 3


