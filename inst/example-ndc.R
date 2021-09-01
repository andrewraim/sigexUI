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

# ---- dput() fit.mle ----
fit.mle =
	list(list(par = c(0.962266346078585, 14.5551830528959, 17.30260372164,
					  -0.331882451313118, 0.0301675758182394, 0.0485821943658045, -0.422593183296109,
					  75.9085560699756, 77.8761035762667), value = 11485.8343821915,
			  counts = c(`function` = 9L, gradient = 9L), convergence = 0L,
			  message = "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH",
			  hessian = structure(c(43.5170259152073, -0.00591489879298024,
			  					  0.131396973301889, 0.0209333848033566, -0.0490833826916059,
			  					  -0.116225919555291, 0.00939439814828802, -0.000477029971079901,
			  					  2.56932253250852e-05, -0.00591489879298024, 339.411655659205,
			  					  0.0122761321108555, -0.536040943188709, -0.0767086021369323,
			  					  1.150638127001, 0.101108980743447, -0.000229874785873108,
			  					  5.11590769747272e-05, 0.131396973301889, 0.0122761321108555,
			  					  339.00310700119, 0.0576828824705444, -0.18523837752582, 1.4101069609751,
			  					  -0.665079596728901, 0.000525005816598423, -0.000568661562283523,
			  					  0.0209333848033566, -0.536040943188709, 0.0576828824705444,
			  					  607.177305482764, -63.6719564681698, 596.188801864628, -17.4675269590807,
			  					  -5.82076609134674e-05, 3.04680725093931e-05, -0.0490833826916059,
			  					  -0.0767086021369323, -0.18523837752582, -63.6719564681698,
			  					  73.2692997189588, -589.646715980052, 54.4217850801942, 0.000514774001203477,
			  					  -0.000112777343019843, -0.116225919555291, 1.150638127001,
			  					  1.4101069609751, 596.188801864628, -589.646715980052, 10355.102898302,
			  					  -305.047722577001, -0.014357055988512, 0.000739419192541391,
			  					  0.00939439814828802, 0.101108980743447, -0.665079596728901,
			  					  -17.4675269590807, 54.4217850801942, -305.047722577001, 492.301566737297,
			  					  0.000654608811601065, -0.0012985310604563, -0.000477029971079901,
			  					  -0.000229874785873108, 0.000525005816598423, -5.82076609134674e-05,
			  					  0.000514774001203477, -0.014357055988512, 0.000654608811601065,
			  					  0.000593900040257722, -5.59339241590351e-05, 2.56932253250852e-05,
			  					  5.11590769747272e-05, -0.000568661562283523, 3.04680725093931e-05,
			  					  -0.000112777343019843, 0.000739419192541391, -0.0012985310604563,
			  					  -5.59339241590351e-05, 4.27462509833276e-05), .Dim = c(9L,
			  					  													   9L))), list(list(structure(c(1, 0.962266346078585, 0, 1), .Dim = c(2L,
			  					  													   																   2L))), list(c(14.5551830528959, 17.30260372164)), list(structure(c(-0.314133262399874,
			  					  													   																   																   0.0233436494343738, 0.0406096127692256, -0.388838692709833), .Dim = c(2L,
			  					  													   																   																   																	  2L, 1L))), c(75.9085560699756, 77.8761035762667)))
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
