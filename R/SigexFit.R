# Constructor for class SigexFit object
#' @export
SigexFit = function(optimOut, paramEst) {
	new("SigexFit",
		optim_output = optimOut,
		param = paramEst)
}


# Wrapper function that takes (model, data), converts to sigex conventions
#     runs sigex.mlefit and then converts to sigexUI conventions.
SigexMLE <- function(model, data.ts, SigexParam = NULL){

	mdl <- to_sigex(model)

	if(!is.null(SigexParam)) {
		param <- to_sigex(SigexParam)
	} else {
		param <- sigex.default(mdl = mdl, data.ts = data.ts, constraint = NULL)
	}

	st = Sys.time()
	fit.mle = sigex.mlefit(data.ts = data.ts,
						   param = param,
						   constraint = NULL,
						   mdl = mdl,
						   method = "bfgs",
						   debug = TRUE)
	et = Sys.time()
	run_time <- as.numeric(et - st, units = "secs")

	paramEst <- asSigexParam(fit.mle[[2]], mdl)

	out <- new("SigexFit",
			   optimOut = fit.mle[[1]],
			   param = paramEst,
			   data = data.ts,
			   model = model)

	return(out)

}

# save(out, file = "~/GitHub/sigexUI/inst/slides/fit.RData")


# ' @export
setMethod("show",
	"SigexFit",
	function(object) {
		optimOut <- object@optimOut
		parFit   <- object@param
		print("SigexFit:" )
		printf("optim_convergence_code = %d, lik = %f\n",
			   optimOut$convergence, optimOut$value)
		print("Parameter Estimates:")
		print(object@param@ts_params)
})


# ' @export
setMethod("resid",
	"SigexFit",
	function(object) {

		optimOut <- object@optimOut
		data.ts <- object@data
		mdl <- to_sigex(object@model)
		par <- to_sigex(object@param)
		psi <- sigex.eta2psi(eta = optimOut$par, constraint = NULL)

		resid.mle <- sigex.resid(psi, mdl, data.ts)[[1]]
		resid.mle <- sigex.load(t(resid.mle), start(data.ts), frequency(data.ts), colnames(data.ts), FALSE)

		return(resid.mle)
})


setMethod("lik",
	"SigexFit",
	function(object){

		optimOut <- object@optimOut
		data.ts <- object@data
		mdl <- to_sigex(object@model)
		psi <- sigex.eta2psi(eta = optimOut$par, constraint = NULL)

		likOut <- sigex.lik(psi = psi, mdl = mdl, data.ts = data.ts, debug = FALSE)

		return(likOut)
})
