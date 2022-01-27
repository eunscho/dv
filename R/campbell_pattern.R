campbell_pattern <- function(hthm, htmm) {
  t <- 3
  m <- 3
  # number of different proportions: t * (t - 1) / 2
  # number of each proportions: m ^ 2
  ratios <- matrix(0, m^2, t * (t - 1) / 2)
  count <- 1
  n <- nrow(hthm)
  for (i in 1:n) {
    for (j in 1:i) {
      is_diag <- (ceiling(i / t) == ceiling(j / t))
      is_htmm <- (is_diag & i %% m == 2 & j %% m == 1)
      is_hthm_lower <- (!is_diag & i %% m == 2 & j %% m == 1)
      is_hthm_upper <- (!is_diag & i %% m == 1 & j %% m == 2)
      if (is_htmm) { # e.g., (2, 1)
        ratios[count, 1] <- htmm[i, j] / htmm[i + 1, j]
        ratios[count, 2] <- htmm[i, j] / htmm[i + 1, j + 1]
        ratios[count, 3] <- htmm[i + 1, j] / htmm[i + 1, j + 1]
        count <- count + 1
      } else if (is_hthm_lower) { # e.g., (5, 1)
        ratios[count, 1] <- hthm[i, j] / hthm[i + 1, j]
        ratios[count, 2] <- hthm[i, j] / hthm[i + 1, j + 1]
        ratios[count, 3] <- hthm[i + 1, j] / hthm[i + 1, j + 1]
        count <- count + 1
      } else if (is_hthm_upper) {
        ratios[count, 1] <- hthm[i, j] / hthm[i, j + 1]
        ratios[count, 2] <- hthm[i, j] / hthm[i + 1, j + 1]
        ratios[count, 3] <- hthm[i, j + 1] / hthm[i + 1, j + 1]
        count <- count + 1
      }
    }
  }
  viol <- 0
  for (i in nrow(ratios)) {
    for (j in ncol(ratios)) {
      if (ratios[i, j] > 2 * mean(ratios[, j]) | ratios[i, j] < .5 * mean(ratios[, j])) {
        viol <- 1
      }
    }
  }
  return(viol)
}