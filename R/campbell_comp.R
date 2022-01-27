campbell_comp <- function(non_mthm, mthm) {
  n <- nrow(non_mthm)
  nviol <- ncomp <- 0 # nviol: number of violations, ncomp: number of comparisons
  for (i in 1:n) {
    for (j in 1:i) {
      if (!is.na(mthm[i, j])) {
        viol <- c(mthm[i, j] < non_mthm[i, ], mthm[i, j] < non_mthm[, j])
        for (k in 1:length(viol)) {
          if (!is.na(viol[k])) {
            ncomp <- ncomp + 1
            if (viol[k] == TRUE) {
              nviol <- nviol + 1
            }
          }
        }
      }
    }
  }
  out <- list(nviol = nviol, ncomp = ncomp)
  return(out)
}