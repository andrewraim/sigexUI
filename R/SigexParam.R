#' @export
setValidity("SigexParam", function(object) {
	# TBD: More checking?
	if (length(object@gcds) != length(object@ts_params)) {
		return("@gcds and @ts_params must be the same length")
	}
	return(TRUE)
})

#' @export
SigexParam = function(N) {
	new("SigexParam", N = N, gcds = list(), ts_params = list(), reg_param = numeric(0))
}

#' @export
setMethod("show", "SigexParam", function(object) {
	K = length(object@gcds)
	printf("SigexParam for a K=%d component model for N=%d dimensional series\n",
		K, object@N)

	for (k in seq_len(K)) {
		lab = modelClass(object@ts_params[[k]])
		printf("%d: %s\n", k, lab)
	}

	if (length(object@reg_param) > 0) {
		printf("Regression parameter is set\n")
	}
})

#' @export
setMethod("addParam",
	c(object = "SigexParam", ts_param = "SigexParamTS", Sigma = "GCD"),
	function(object, ts_param, Sigma) {
		nrows = dim(Sigma)[1]
		ncols = dim(Sigma)[2]
		if (nrows != object@N || ncols != object@N) {
			msg = sprintf("Dimension (%d,%d) of Sigma is not compatible with this SigexParam",
				nrows, ncols)
			stop(msg)
		}

		K = length(object@gcds)
		object@gcds[[K + 1]] = Sigma
		object@ts_params[[K + 1]] = ts_param

		invisible(object)
	}
)

#' @export
setMethod("addParam",
	c(object = "SigexParam", ts_param = "SigexParamTS", Sigma = "matrix", rank = "ANY"),
	function(object, ts_param, Sigma, rank) {
		if (missing(rank)) {
			# If rank is missing, just set it to something by default
			# If Sigma is not a square matrix, it probably doesn't make sense, but will
			# something else complain?
			rank = min(nrow(Sigma), ncol(Sigma))
		}
		gcd_out = getGCD(Sigma, rank)
		gcd = GCD(gcd_out[[1]], gcd_out[[2]])
		addParam(object, ts_param, gcd)
	}
)

#' @export
setMethod("setRegParam",
	c(object = "SigexParam", beta = "numeric"),
	function(object, beta) {
		# TBD: do we need to do any checking of dimensions?
		object@reg_param = beta
		invisible(object)
	}
)
