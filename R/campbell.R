campbell <- function(r, trait, method, item) {
  n <- trait * method # number of rows or columns
  mthm <- hthm <- htmm <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:i) {
      if (i > j | i < j) { # nondiagonal
        tnum <- mydiv(ceiling(i / item), trait)
        mnum <- ceiling(i / (item * trait))
        if (tnum == mnum) { # htmm matrix
          htmm[i, j] <- r[i, j]
        } else { # hthm matrix
          if (mydiv(i, trait) == mydiv(j, trait)) { #diagonal of the hthm matrix
            mthm[i, j] <- r[i, j]
          } else {
            hthm[i, j] <- r[i, j]
          }
        }
      }
    }
  }
  out <- list(mthm = mthm, hthm = hthm, htmm = htmm)
  return(out)
}