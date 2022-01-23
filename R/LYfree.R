LYfree <- function(trait = 3, method = 3, item = 3) {
  out <- matrix(0, trait * method * item, trait + method)
  for (i in 1:(trait * method * item)) {
    for (j in 1:(trait + method)) {
      if (j <= trait) { # trait loadings
        if (ceiling(i / item) %% trait == j | ceiling(i / item) %% trait == 0) {
          out[i, j] <- NA
        }
      } else { # method loadings
        
      }
    }
  }
  return(out)
}