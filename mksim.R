mksim <- function(nraces = 4, nsim = 1e6){
  # Define the possible scores
  score <- c( 1:10, 12, 15 )
  
  # Sample 4 game streaks. 
  # Sample with replacement so that every sample drawn has the same probability of success
  boots <- replicate( n = nsim, expr = sample( x = score, size = nraces, replace = TRUE ) )
  
  # Sum the values in each column
  sums <- colSums( boots )
  
  mu <- mean( sums )
  sigma2 <- var( sums )
  sigma <- sqrt( var( sums ) )
  skew <- sum( ( sums - mu )^3 ) / ( nsim * sigma^3 )
  
  statvec <- c( mean = mu, variance = sigma2, sd = sigma, skew = skew )
  
  # Calculate win distribution summary
  windist <- summary( sums )
  
  return( list( scores = sums,
                stats = statvec,
                win.distribution = windist ) )
  
}

out <- mksim()












