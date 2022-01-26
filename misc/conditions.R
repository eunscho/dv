n <- c(50, 1000)
fcor <- c(.7, 1) # focal trait correlation
conditions <- tidyr::crossing(n, fcor)
colnames(conditions) <- c("n", "fcor")