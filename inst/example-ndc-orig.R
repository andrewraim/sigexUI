dataALL.ts <- sigex.load(data = ndc,
  start.date = c(1992, 1),
  period = 12,
  epithets = c("Shipments", "NewOrders"),
  plot = TRUE)

data.ts <- sigex.prep(data.ts = dataALL.ts,
  transform = "none",
  aggregate = FALSE,
  subseries = 1:2,
  range = NULL,
  plot = TRUE)

N <- dim(data.ts)[2]
T <- dim(data.ts)[1]

ar.fit <- ar.yw(diff(ts(ndc[2:T, ])))
p.order <- ar.fit$order
par.yw <- aperm(ar.fit$ar, c(2, 3, 1))
covmat.yw <- getGCD(ar.fit$var.pred, 2)
var.out <- var.par2pre(par.yw)
psi.init <- as.vector(c(covmat.yw[[1]][2, 1], log(covmat.yw[[2]]), var.out, colMeans(diff(ts(ndc[2:T, ])))) )

mdl <- NULL
mdl <- sigex.add(mdl = mdl,
  vrank = seq(1,N),
  class = "varma",
  order = c(p.order,0),
  bounds = NULL,
  name = "process",
  delta = c(1,-1) )

# regressors:
mdl <- sigex.meaninit(mdl = mdl, data.ts = data.ts, d = 0)

constraint <- NULL
psi.mle <- psi.init
par.mle <- sigex.psi2par(psi = psi.mle, mdl = mdl, data.ts = data.ts)

fit.mle <- sigex.mlefit(data.ts = data.ts,
  param = par.mle,
  constraint = constraint,
  mdl = mdl,
  method = "bfgs",
  debug = TRUE )


