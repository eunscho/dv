LYfree <- function(trait = 3, method = 3, item = 3) {
  out <- matrix(0, trait * method * item, trait + method)
  for (i in 1:(trait * method * item)) {
    for (j in 1:(trait + method)) {
      if (j <= trait) { # trait loadings
        if (j < trait) {
          if (ceiling(i / item) %% trait == j) {
            out[i, j] <- NA
          } 
        } else {
          if (ceiling(i / item) %% trait == 0) {
            out[i, j] <- NA
          }
        }
      } else { # method loadings
        if (j < trait + method) {
          if (ceiling(i / item) %% method == j - trait) {
            out[i, j] <- NA
          }
        } else {
          if (ceiling(i / item) %% method == 0) {
            out[i, j] <- NA
          }
        }
      } # end of } else { # method loadings
    } # end of for (j in 1:(trait + method)) {
  } # end of for (i in 1:(trait * method * item)) {
  return(out)
}

