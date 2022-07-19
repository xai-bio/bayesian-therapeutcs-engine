## Function to calculate expected value
expectedvalue <- function(x) {
  values <- x[,1][,apply(.SD,FUN=as.numeric,1)]
  expect <- sum(values*x$probs)
  return(expect)
}


## Function to calculate normalized expected value...
## ...as ratio between (exp - min)/(max-min) 
normexp <- function(x) {
  values <- x[,1][,apply(.SD,FUN=as.numeric,1)]
  expect <- sum(values*x$probs)
  norm <- (expect - min(values))/(max(values)-min(values))
  return(norm)
}


## Function to extract probabilities from data
prob <- function(DT,VRBL) {
  # Need DT as data.table and VRBL as a string
  samples <- nrow(DT)
  probability <- table(DT[,VRBL,with = F])/nrow(DT)
  probability <- as.data.table(probability)[,.(as.factor(format(as.numeric(V1),digits = 2)),probs = N)]
  names(probability)[1] <- VRBL
  return(probability)
}


# Function to assign distribution to node and propagate to other nodes
propagate <- function(DT,BN.fitted,VRBL.list,COND.prob) {
  ## Initialise empty result list
  result <- list()
  
  for (VRBL in VRBL.list) { ## Go through all variables
  
    VRBL.prob <- prob(DT,VRBL) ## Probability of ocurrence of a variable for each event in sample space
    OLD.VRBL.prob <- rbind(VRBL.prob) ## Save probabilities in old probability vector
    VRBL.prob <- VRBL.prob[1:nrow(VRBL.prob), probs := 0] ## Create a zero-vector of probabilities
    
    for (i in 1:nrow(COND.prob)) {
      XTR <- as.data.frame(attr(predict(BN.fitted , VRBL, COND.prob[i,1,with = F], method = "bayes-lw", prob = T),"prob"))
      XTR$bins <- rownames(XTR)
      XTR <- as.data.table(XTR)
      XTR <- XTR[,.(bins = bins, probs = V1)]
      VRBL.prob  <- VRBL.prob[,probs := probs + XTR$probs*as.numeric(COND.prob$probs[i])]
      VRBL.prob$probs.old <- OLD.VRBL.prob$probs
    }
    
    result[[VRBL]] <- VRBL.prob
  }
  return(result)
}
