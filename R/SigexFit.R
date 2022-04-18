# Constructor for class SigexFit object
#' @export
SigexFit = function(optimOut, paramEst) {
	new("SigexFit",
		optim_output = optimOut,
		param = paramEst)
}


# Wrapper function that takes (model, data), converts to sigex conventions
#     runs sigex.mlefit and then converts to sigexUI conventions.
SigexMLE <- function(SigexModel, data.ts, SigexParam = NULL){

	mdl <- to_sigex(SigexModel)

	if(!is.null(SigexParam)) {
		param <- to_sigex(SigexParm)
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

	optimOut <- fit.mle[[1]]

	paramEst <- asSigexParam(fit.mle[[2]], mdl)

	out <- SigexFit(optimOut, paramEst)

	return(out)

}

# save(out, file = "~/GitHub/sigexUI/inst/slides/fit.RData")


# ' @export
setMethod("show",
	"SigexFit",
	function(object) {
		optimOut <- object@optim_output
		parFit   <- object@param
		print("SigexFit: \n")
		printf("optim_convergence_code = %d, lik = %f\n",
			   optimOut$convergence, optimOut$value)
})


# We need more objects to be part of SigexFit to get residuals
#    probably need: mdl, and data...

# # ' @export
# setMethod("resid",
# 	"SigexFit",
# 	function(object) {
#
# 		par <- to_sigex(object@param)
#
# 		resid.mle <- sigex.resid(psi.mle, mdl, data.ts)[[1]]
# 		resid.mle <- sigex.load(t(resid.mle), start(data.ts), frequency(data.ts), colnames(data.ts), FALSE)
#
# })

