generate_dat <- function(conditions, condition_number, rep_set, rep) {
  library(simsem)
  #source("misc/conditions.r")
  set.seed(10000 * condition_number + 100 * rep_set + rep)
  n <- as.integer(conditions[condition_number, 1])
  fcor <- as.double(conditions[condition_number, 2])
  t <- 3 # number of traits
  m <- 3 # number of methods
  k <- 3 # number of items per trait-method unit
  tloading <- rep(.6, t * m * k)
  mloading <- rep(.5, t * m * k)
  LY <- bind(LYfree(t, m, k), LYpop(t, m, k, tloading, mloading))
  LY
  tcor <- c(fcor, rep(.3, t * (t - 1) / 2 - 1)) # trait correlatons
  mcor <- rep(.3, m * (m - 1) / 2) # method correlations
  PS <- binds(PSfree(t, m), PSpop(t, m, tcor = tcor, mcor))
  error.cor <- matrix(0, t * m * k, t * m * k)
  diag(error.cor) <- 1
  RTE <- binds(error.cor)
  mod <- model.cfa(LY = LY, PS = PS, RTE = RTE, indLab = item_label(t,m,k), facLab = fac_label(t, m))
  out <- simsem::generate(mod, n)

  return(out)
}
