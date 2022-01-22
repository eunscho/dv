tofull <- function(matrix) {
  out <- matrix
  for (i in 1:nrow(matrix)) {
    for (j in 1:i) {
      out[j, i] <- out[i, j]
    }
  }
  return(out)
}