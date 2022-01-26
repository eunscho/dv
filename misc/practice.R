library(simsem)
t <- 3
m <- 3
k <- 3
tloading <- rep(.6, t * m * k)
mloading <- rep(.5, t * m * k)
LY <- bind(LYfree(t, m, k), LYpop(t, m, k, tloading, mloading))
LY
tcor <- c(.9, rep(.3, t * (t - 1) / 2 - 1))
mcor <- rep(.3, m * (m - 1) / 2)
PS <- binds(PSfree(t, m), PSpop(t, m, tcor = tcor, mcor))
error.cor <- matrix(0, t * m * k, t * m * k)
diag(error.cor) <- 1
RTE <- binds(error.cor)
mod <- model.cfa(LY = LY, PS = PS, RTE = RTE, indLab = item_label(t,m,k), facLab = fac_label(t, m))


dat <- generate(mod, 200)
dat
out <- analyze(model = mod, dat)


PScon <- binds(PSfree_constrained(t, m), PSpop_constrained(t, m, tcor = tcor, mcor))
PScon

com <- composite(dat, t, m, k)


com
dat
sum(dat[1, 1:2])

r <- cor(com)
cam

cam <- campbell(dat, t, m, k)
hthm(cam$hthm, cam$mthm)
hthm(cam$hthm, cam$htmm)
cam$mtmm
hthm(cam$htmm, cam$rel)
library(semTools)
?discriminantValidity
