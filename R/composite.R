composite <- function(dat, trait, method, item) {
  out <- matrix(0, nrow(dat), trait * method)
  for (i in 1:(trait * method * item)) {
    tnum <- mydiv(ceiling(i / item), trait)
    mnum <- ceiling(i / (item * trait))
    for (j in 1:trait) {
      for (l in 1:method) {
        if (j == tnum & l == mnum) {
          out[, ceiling(i / item)] <- out[, ceiling(i / item)] + dat[, i]
        }
      }
    }
  }
  name <- vector("character", trait * method)
  for (i in 1:(trait * method)) {
    tnum <- mydiv(i, trait)
    mnum <- ceiling(i / trait)
    name[i] <- paste0("t", tnum, "m", mnum)
  }
  colnames(out) <- name
  
  return(out)
}