library(sigex)

# Make a ts object with the built-in ts function
dataALL.ts = ts(data = ndc,
  start = c(1992, 1),
  frequency = 12,
  names = c("Shipments", "NewOrders"))

# Plot ts object with the built-in plot.ts function
plot(dataALL.ts)

data.ts = sigex.prep(data.ts = dataALL.ts,
  transform = "none",
  aggregate = FALSE,
  subseries = 1:2,
  range = NULL,
  plot = TRUE)

# Get the dimensions
N = ncol(data.ts)
T = nrow(data.ts)

# The original example seems to take a very long time to fit in sigex.mlefit.
# If I set order.max to 1, it takes about 3.4 minutes on my laptop.

# Get a starting value for MLE
# ar.fit = ar.yw(diff(ts(ndc[2:T, ])))
order.max = 1
ar.fit = ar.yw(diff(ts(ndc[2:T, ])), order.max = order.max)
p.order = ar.fit$order
par.yw = aperm(ar.fit$ar, c(2, 3, 1))
covmat.yw = getGCD(ar.fit$var.pred, 2)
var.out = var.par2pre(par.yw)
psi.init = as.vector(c(covmat.yw[[1]][2, 1], log(covmat.yw[[2]]), var.out, colMeans(diff(ts(ndc[2:T, ])))) )

get_par = function() {
  # par is a list of four things...
  # 4. Regression parameters?

  list(beta = beta)
}

ar2par = function(object, rank) {
  stopifnot(class(object) == "ar")
  par = aperm(object$ar, c(2, 3, 1))
  covmat = getGCD(object$var.pred, rank)
}

# Set up a model
mdl = NULL
mdl = sigex.add(mdl = mdl,
  vrank = seq(1,N),
  class = "varma",
  order = c(p.order,0),
  bounds = NULL,
  name = "process",
  delta = c(1,-1) )
mdl = sigex.meaninit(mdl = mdl, data.ts = data.ts, d = 0)

## I think that last mysterious colMeans term of psi.init above is related to the
## sigex.meaninit call above. It looks like the colMeans result is directly used
## as the last two entries of par.init next.

## We should make a sigex model with its own type and named attributes

## Let's make a little document for the data structures we need to understand:
## - mdl
## - par
## - psi
## - pre
## - gcd
## - mlefit
## - momfit

par.init = sigex.psi2par(psi = psi.init, mdl = mdl, data.ts = data.ts)

## TBD for Andrew
## - There should be a default for params that's something like "zero"
## - Constraint should be null by default. Create a get_constraint
##   function that gives a valid constraint data structure.
## - I want to be able to print the model object and have it display
##   all the added components.

st = Sys.time()
fit.mle = sigex.mlefit(data.ts = data.ts,
  param = par.init,
  constraint = NULL,
  mdl = mdl,
  method = "bfgs",
  debug = TRUE)
et = Sys.time()
as.numeric(et - st, units = "secs")

# Notes about sigex
# Instead of debug, maybe let user change "trace" option in optim
# In sigex.psi2par and related functions, can we avoid needing a dataset and just take dimensions?
# Maybe sigex could use named lists to help make the interface more intuitive

psi.mle = sigex.eta2psi(eta = fit.mle[[1]]$par, constraint = constraint)
hess = fit.mle[[1]]$hessian
par.mle = fit.mle[[2]]
