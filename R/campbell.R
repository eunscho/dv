campbell <- function(r, trait, method, item) {
  n <- trait * method # number of rows or columns
  mthm <- hthm <- htmm <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:i) {
      if (i > j | i < j) { # nondiagonal
        tnumi <- ceiling(i / trait)
        tnumj <- ceiling(j / trait)
        #mnum <- ceiling(i / trait)
        if (tnumi == tnumj) { # htmm matrix
          htmm[i, j] <- r[i, j]
        } else { # hthm matrix
          if (mydiv(i, trait) == mydiv(j, trait)) { #diagonal of the hthm matrix
            mthm[i, j] <- r[i, j]
          } else {
            hthm[i, j] <- r[i, j]
          }
        } #  } else { # hthm matrix
      } # if (i > j | i < j) { # nondiagonal
    } # for (j in 1:i) {
  } # for (i in 1:n) {
  out <- list(mthm = mthm, hthm = hthm, htmm = htmm)
  return(out)
}