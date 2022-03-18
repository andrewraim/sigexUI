
SigexFit <- function(model, data, param = NULL){

	# dimension of vectors
	N <- dim(data)[2]
	TT <- dim(data)[1]

	# ---- Set Default param setup (if no SigexParam supplied) ----
	# This step is not needed since if user doesn't specify a param we can
	#     just directly run the sigex.mlefit and convert to the new S4 param
	#     setup afterwards.

	if(is.null(SigexParam)){

		# initialize param
		param <- SigexParam(N)

		# Number of stocastic components in model
		J <- length( model@mdl$type )

		# Add J default components to param
		gcd_init = GCD(L = diag(N), D_vec = rep(0, N))
		beta_init = rep(0, N)
		for(j in 1:J){
			param <- param %>%
				addParam(SigexParamVARMA(ar = diag(N),
										 ma = diag(N)),
						 gcd_init)
		}

		# Add regression parameters
		# TO DO: need to generalize this for when model includes
		#        regression variables
		param <- param %>% setRegParam(beta = beta_init)

	}

	# ---- convert function input to old format ----

	# setup empty param shell to fill with S4 data
	parOld <- sigex.default(model@mdl, data, NULL)

	# Loop over all stocastic comps and add params to parOld
	for(j in 1:J){
		parOld[[1]][[j]] <- param@gcds[[j]]@L
		parOld[[2]][[j]] <- param@gcds[[j]]@D_vec
		parOld[[3]][[j]] <- diag()
	}

	# ---- Run Tucker mle fitting ----
	st = Sys.time()
	fit.mle = sigex.mlefit(data.ts = data,
						   param = par.init,
						   constraint = NULL,
						   mdl = mdl,
						   method = "bfgs",
						   debug = TRUE)
	et = Sys.time()
	runTime <- as.numeric(et - st, units = "secs")

	# ---- Extract necessary components from fit with sigex functions ----

	# ---- Convert to S4 format ----

	# ---- return stuff ----


}
