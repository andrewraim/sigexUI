# Constructor for class SigexFit object
#' @export
SigexFit = function(optimOut, paramEst) {
	new("SigexFit",
		optim_output = optimOut,
		param = paramEst)
}



SigexMLE <- function(SigexModel, data, SigexParam = NULL){

	mdl <- to_sigex(SigexModel)

	if(!is.null(SigexParam)) {
		param <- to_sigex(SigexParm)
	} else {
		param <- sigex.default(mdl = mdl, data.ts = data.ts, constraint = NULL)
	}



	st = Sys.time()
	fit.mle = sigex.mlefit(data.ts = data,
						   param = param,
						   constraint = NULL,
						   mdl = mdl,
						   method = "bfgs",
						   debug = TRUE)
	et = Sys.time()
	run_time <- as.numeric(et - st, units = "secs")

	optimOut <- fit.mle[[1]]
	paramEst <- asSigexParam(fit.mle[[2]], mdl) # Needs to be written

	out <- SigexFit(optimOut, paramEst)

	return(out)

}



