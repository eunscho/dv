campbell_matrix <- function(datamat, trait, method, item) {
  r <- cor(composite(datamat, trait, method, item))
  n <- trait * method # number of rows or columns
  mthm <- hthm <- htmm <- rel <- matrix(NA, n, n)
  name <- vector("character", n)
  for (i in 1:n) {
    tnumi <- mydiv(i, trait)
    mnumi <- ceiling(i / trait)
    name[i] <- paste0("t", tnumi, "m", mnumi)
    diag(rel)[i] <- reliacoef::mu4(cor(datamat[((i - 1) * item + 1):(i * item )]))
    for (j in 1:i) {
      if (i > j | i < j) { # nondiagonal
        mnumj <- ceiling(j / trait)
        if (mnumi == mnumj) { # htmm matrix
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
  
  colnames(mthm) <- rownames(mthm) <- name
  colnames(hthm) <- rownames(hthm) <- name
  colnames(htmm) <- rownames(htmm) <- name
  colnames(rel) <- rownames(rel) <- name
  out <- list(mthm = mthm, hthm = hthm, htmm = htmm, rel = rel)
  return(out)
}

