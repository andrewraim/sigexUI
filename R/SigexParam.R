#' SigexParam
#'
#' Construct a new \code{SigexParam} for an \code{N}-dimensional series, with
#' no latent components or regression model.
#'
#' @slot N A \code{SigexParam} object.
#'
#' @export
SigexParam = function(N) {
	new("SigexParam", N = N, gcds = list(), ts_params = list(), reg_param = numeric(0))
}

#' setValidity
#'
#' Check validity of a new \code{SigexParam}.
#'
#' @slot object A \code{SigexParam} object.
#'
#' @export
setValidity("SigexParam", function(object) {
	# TBD: More checking?
	if (length(object@gcds) != length(object@ts_params)) {
		return("@gcds and @ts_params must be the same length")
	}
	return(TRUE)
})

#' show
#'
#' \code{show} method for \code{SigexParam}.
#'
#' @slot object A \code{SigexParam} object.
#'
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

#' addParam
#'
#' Add parameters for a latent component to the structure.
#'
#' @slot object A \code{SigexParam} object.
#' @slot ts_param A \code{SigexParamTS} object.
#' @slot Sigma A \code{GCD} object which represents a \eqn{N \times N}
#' covariance matrix.
#'
#' @return Returns \code{object} invisibly.
#'
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

#' addParam
#'
#' Add parameters for a latent component to the structure.
#'
#' @slot object A \code{SigexParam} object.
#' @slot ts_param A \code{SigexParamTS} object.
#' @slot Sigma A \eqn{N \times N} covariance matrix.
#' @slot rank TBD: this is for GCD. Explain this and its default value.
#'
#' @return Returns \code{object} invisibly.
#'
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

#' setRegParam
#'
#' Set the vector of regression coefficients in a \code{SigexParam}.
#'
#' @slot object A \code{SigexParam} object.
#' @slot beta A vector of regression coefficients.
#'
#' @return Returns \code{object} invisibly.
#'
#' @export
setMethod("setRegParam",
	c(object = "SigexParam", beta = "numeric"),
	function(object, beta) {
		# TBD: do we need to do any checking of dimensions?
		object@reg_param = beta
		invisible(object)
	}
)


#' to_sigex
#'
#' Construct a \code{sigex} data structure for parameters from a
#' \code{SigexParam}.
#'
#' @slot object A \code{SigexParam} object.
#'
#' @return A \code{sigex} data structure.
#'
#' @export
setMethod("to_sigex",
	c(object = "SigexParam"),
	function(object) {
		out <- vector(mode = 'list', length = 4)

		K = length(object@gcds)

		out[[1]] = list() # the L of LDL' decomp
		out[[2]] = list() # the diag of D in LDL' decomp
		for (k in seq_len(K)) {
			out[[1]][[k]] = object@gcds[[k]]@L
			out[[2]][[k]] = object@gcds[[k]]@D_vec
		}

		out[[3]] = list() # the time series comps
		for (k in seq_len(K)) {
			out[[3]][[k]] = to_sigex(object@ts_params[[k]])
		}

		out[[4]] = object@reg_param # vector of regression params

		return(out)
	}
)

#' asSigexParam
#'
#' Construct a \code{SigexParam} from a \code{sigex} data structure.
#'
#' @slot paramObject A \code{sigex} data structure representing parameters.
#' @slot mdlObject A \code{sigex} data structure representing a model.
#'
#' @return A \code{SigexParam}.
#'
#' @details
#' Note that \code{mdlObject} is needed to interpret the elements of
#' \code{paramObject}.
#'
#' @export
setMethod("asSigexParam",
    c(paramObject = "list", mdlObject = "list"),
	function(paramObject, mdlObject){

		N <- dim(paramObject[[1]][[1]])[1]
		K <- length(mdlObject[[1]]) # number of model components

		outSigexParam <- SigexParam(N)

		for (k in 1:K) {
			compTyp <- mdlObject[[2]][[k]][[1]]
			compPar <- mdlObject[[2]][[k]][[2]]
			compGCD = GCD(L = paramObject[[1]][[k]],
						  D_vec = paramObject[[2]][[k]])

			# Switch to decide on type
			if (compTyp == 'arma') {
				p <- compPar[1]
				q <- compPar[2]
				if (p == 0 & q == 0) {
					arMat <- matrix(NA, nrow = N, ncol = 0)
					maMat <- matrix(NA, nrow = N, ncol = 0)
				} else if (p == 0 & q > 0) {
					arMat <- matrix(NA, nrow = N, ncol = 0)
					maMat <- as.matrix(paramObject[[3]][[k]])
				} else if (p > 0 & q == 0) {
					arMat <- as.matrix(paramObject[[3]][[k]])
					maMat <- matrix(NA, nrow = N, ncol = 0)
				} else {
					arMat <- as.matrix(paramObject[[3]][[k]][, 1:p])
					maMat <- as.matrix(paramObject[[3]][[k]][, (p + 1):(p + q)])
				}
				# add component output list
				outSigexParam <- outSigexParam %>%
					addParam(new("SigexParamARMA", ar = arMat, ma = maMat),
							 compGCD)
			} else if (compTyp == "varma") {
				p <- compPar[1]
				q <- compPar[2]
				if (p == 0 & q == 0) {
					arArr <- array(NA, c(N, N, 0))
					maArr <- array(NA, c(N, N, 0))
				} else if (p == 0 & q > 0) {
					arArr <- array(NA, c(N, N, 0))
					maArr <- paramObject[[3]][[k]]
				} else if (p > 0 & q == 0) {
					arArr <- paramObject[[3]][[k]]
					maArr <- array(NA, c(N, N, 0))
				} else {
					arArr <- paramObject[[3]][[k]][, 1:p]
					maArr <- paramObject[[3]][[k]][, (p + 1):(p + q)]
				}
				# add component output list
				outSigexParam <- outSigexParam %>%
					addParam(new("SigexParamVARMA",
								 ar = arArr,
								 ma = maArr),
							 compGCD)
			} else {
				msg <- paste("components of type",
							 compTyp,
							 "not yet supported in asSigexParam")
				stop(msg)
			}
		}

		return(outSigexParam)
	}
)

#' coef
#'
#' Print elements of the given \code{SigexParam}. (TBD: \code{show} also
#' does this... maybe this can do something more like other R \code{coef}
#' methods?)
#'
#' @slot object A \code{SigexParam} object.
#'
#' @export
setMethod("coef",
	c(object = "SigexParam"),
	function(object){
		print(object)
	}
)

#' component
#'
#' Retrieve a specified component of the \code{SigexParam} and return a new
#' \code{SigexParam} with only this component.
#'
#' @slot object A \code{SigexParam} object.
#' @slot index The desired index in \eqn{\{ 1, \ldots, K \}}.
#'
#' @export
setMethod("component",
	c(object = "SigexParam", index = "numeric"),
	function(object, index) {
		new("SigexParam",
			N = object@N,
			gcds = object@gcds[index],
			ts_params = object@ts_params[index],
			reg_param = object@reg_param[index])
	}
)

