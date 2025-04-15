runif_with_seed <-
  function(n, min, max, seed){
    set.seed(seed)
    output <- runif(n, min, max)
  }
