library(methods)
library(sigex)
library(dplyr)

# ----- Example using existing sigex interface -----

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

## I think that last  colMeans term of psi.init above is related to the
## sigex.meaninit call above. It looks like the colMeans result is directly used
## as the last two entries of par.init next.

par.init = sigex.psi2par(psi = psi.init, mdl = mdl, data.ts = data.ts)

st = Sys.time()
fit.mle = sigex.mlefit(data.ts = data.ts,
	param = par.init,
	constraint = NULL,
	mdl = mdl,
	method = "bfgs",
	debug = TRUE)
et = Sys.time()
as.numeric(et - st, units = "secs")

psi.mle = sigex.eta2psi(eta = fit.mle[[1]]$par, constraint = constraint)
hess = fit.mle[[1]]$hessian
par.mle = fit.mle[[2]]

# ----- Experimental S4 interface -----
# An S4 SigexConstraint
constraint = SigexConstraint(A = diag(3), b = numeric(3))

gcd = GCD(L = covmat.yw[[1]], D_vec = covmat.yw[[2]])
# ts_param = new("SigexParamTS")
# arma_par = new("SigexParamARMA", ar = par.yw[,,1], ma = par.yw[,,1])
# varma_par = new("SigexParamVARMA", ar = par.yw, ma = par.yw)
varma_par = SigexParamVARMA(ar = par.yw, ma = par.yw)
beta = colMeans(diff(ts(ndc[2:T,])))
show(gcd)

param = SigexParam(gcd_list = list(gcd), ts_param_list = list(varma_par), reg_param = beta)
object = param
show(param)

mdl = sigex.add(mdl = mdl,
	vrank = seq(1,N),
	class = "varma",
	order = c(p.order,0),
	bounds = NULL,
	name = "process",
	delta = c(1,-1) )

# Basically I want to be able to do something like this
model = SigexModel()
add(model, SigexModelComponent())
add(model, SigexModelComponent())

# Then we could hopefully compose the calls like this
model = SigexModel() %>%
	add(SigexModelComponent()) %>%
	add(SigexModelComponent())

# SigexModelComponent is like an abstract class though. Really there are ARMA
# components, VARMA components, etc

# Some of the other arguments of sigex.add belong in our add() and not in the
# components. I think delta, name, and vrank are all like this.

# Since we're just a UI right now, SigexModel should ultiimately just have a
# sigex mdl inside. Our add operations just call sigex.add.

# Default for vrank can be seq(1,N) I think
# Default for name can be determined by a counter: component1
# Default for delta could be 0, but maybe this is something we want specified?
model = SigexModel()
comp1 = SigexModelComponentVARMA(p = 2, q = 2)
model2 = add(model, comp1, name = "process", vrank = seq(1,N), delta = c(1,-1), bounds = c(1,2,3,4))

model = SigexModel() %>%
	add(SigexModelComponentVARMA(p = 2, q = 2, epithet = "process"), vrank = seq(1,N), delta = c(1,-1), bounds = c(1,2,3,4)) %>%
	add(SigexModelComponentVARMA(p = 0, q = 0, epithet = "wn"), vrank = seq(1,N), delta = c(1,-1), bounds = c(1,2,3,4))
model@mdl
