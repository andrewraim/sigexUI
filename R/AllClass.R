# Convert the output of ar to an S4 class. Ideally, we should not be doing this
# because we aren't the owners.
setOldClass("ar")

#' SigexModelComponent
#'
#' An abstract base class for model components.
#'
#' @slot model_class A string that uniquely describes the class of model
#' component for a subclass of this class.
#' @slot delta Vector of coefficients of the differencing polynomial.
#' @slot epithet A description for the component
#' @export
setClass("SigexModelComponent",
	slots = c(
		model_class = "character",
		delta = "numeric",
		epithet = "character"
	),
	prototype = list(
		model_class = "NULL",
		delta = NULL,
		epithet = "NULL"
	)
)

#' SigexModelComponentVARMA
#'
#' A subclass of SigexModelComponent that represents a \eqn{\text{VARMA}(p,q)} process.
#'
#' @slot p The order of the AR component. A non-negative integer.
#' @slot q The order of the MA component. A non-negative integer.
#' @export
setClass("SigexModelComponentVARMA",
	contains = "SigexModelComponent",
	slots = c(
		p = "numeric",
		q = "numeric"
	),
	prototype = list(
		model_class = "varma"
	)
)

#' SigexModelComponentARMA
#'
#' A subclass of SigexModelComponent that represents an \eqn{\text{ARMA}(p,q)}
#' series or a vector of independent \eqn{\text{ARMA}(p,q)} series.
#'
#' @slot p The order of the AR component. A non-negative integer.
#' @slot q The order of the MA component. A non-negative integer.
#' @export
setClass("SigexModelComponentARMA",
		 contains = "SigexModelComponent",
		 slots = c(
		 	p = "numeric",
		 	q = "numeric"
		 ),
		 prototype = list(
		 	model_class = "arma",
		 	p = NULL,
		 	q = NULL
		 )
)

#' SigexParamTS
#'
#' An abstract base class for parameters which are specific to time series.
#' For example, this includes AR and MA coefficients in an ARMA process, but not
#' the covariance matrix.
#'
#' @slot model_class A string that uniquely describes the class of time series
#' parameters represented by a subclass of this class.
#' @export
setClass("SigexParamTS",
	slots = c(
		model_class = "character"
	),
	prototype = list(
		model_class = NULL
	)
)

#' SigexParam
#'
#' Parameters for an \eqn{N} dimensional series with \eqn{K} latent components.
#'
#' @slot N The dimension of the series.
#' @slot gcds A list of \eqn{K} \code{GCD} objects which represent the
#' covariance for each component.
#' @slot ts_params A list of \eqn{K} time series parameter objects (of type
#' \code{SigexParamTS}).
#' @slot reg_param A vector of regression parameters.
#' @export
setClass("SigexParam",
	slots = c(
		N = "numeric", # Used to be integer but floating pt arithmetic issues
		gcds = "list",
		ts_params = "list",
		reg_param = "numeric"
	),#
	prototype = list(
		N = NA_integer_,
	#	gcd_list = list(),
	#	ts_param_list = NA_real_,
		reg_param = NA_real_
	)
)

#' SigexParamARMA
#'
#' Parameters structure for an \\eqn{N}-dimensional series with independent
#' \eqn{\text{ARMA}(p,q)} coordinates.
#'
#' @slot ar An \eqn{N \times p} matrix, where element \eqn{(i,j)} represents
#' the \eqn{j}th AR coefficient for the \eqn{i}th series.
#' @slot ma An \eqn{N \times q} matrix, where element \eqn{(i,j)} represents
#' the \eqn{j}th AR coefficient for the \eqn{i}th series.
#'
#' @details
#' TBD: What about independent series with different orders?
#'
#' @export
setClass("SigexParamARMA",
	contains = "SigexParamTS",
	slots = c(ar = "matrix", ma = "matrix"),
	prototype = list(model_class = "ARMA", ar = NULL, ma = NULL)
)

#' SigexParamVARMA
#'
#' Parameters structure for an \\eqn{N}-dimensional series with independent
#' \eqn{\text{VARMA}(p,q)} coordinates.
#'
#' @slot ar An \eqn{N \times N \times p} array, where slice \code{[,,l]}
#' represents the \eqn{l}th AR coefficient matrix.
#' @slot ma An \eqn{N \times N \times p} array, where slice \code{[,,l]}
#' represents the \eqn{l}th MA coefficient matrix.
#'
#' @export
setClass("SigexParamVARMA",
	contains = "SigexParamTS",
	slots = c(ar = "array", ma = "array"),
	prototype = list(model_class = "VARMA")
)

#' SigexModel
#'
#' Model for an \eqn{N} dimensional series with \eqn{K} latent components.
#'
#' @slot N The dimension of the series.
#' @slot mdl TBD
#' @slot components TBD
#' @export
setClass("SigexModel",
	slots = c(
		N = "integer",
		mdl = "list",
		components = "list"
	),
	prototype = list(
		N = NA_integer_,
		mdl = NULL,
		components = list()
	)
)

#' SigexFit
#'
#' Result from Sigex fit via likelihood maximization.
#'
#' @slot optimOut Output from the optimizer (\link{optim}).
#' @slot param A \code{SigexParam} containing the result.
#' @slot data The data matrix
#' @slot model The \code{SigexModel} used to define the likelihood.
#' @export
setClass("SigexFit",
	slots = c(
		optimOut = "list",
		param    = "SigexParam",
		data     = "matrix",
		model    = "SigexModel"
	),
	prototype = list(
		optimOut = NULL,
		param    = NULL,
		data     = NULL,
		model    = NULL
	)
)
