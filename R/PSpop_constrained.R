PSpop_constrained <- function(trait = 3, method = 3, tcor, mcor) {
  len <- (trait + method)
  out <- matrix(0, len, len)
  count <- 1
  for (i in (trait + 1):len) {
    for (j in (trait + 1):i) {
      if (i == j) {
        out[i, j] <- 1 # variance to 1
      } else if (i > j){
        out[i, j] <- mcor[count]
        count <- count + 1
      } 
    }
  }
  for (i in (trait + 1):(len - 1)) {
    for (j in (i + 1): len) {
      out[i, j] <- out[j, i]
    }
  }
  
  return(out)
}