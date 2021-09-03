# Convert the output of ar to an S4 class. Ideally, we should not be doing this
# because we aren't the owners.
setOldClass("ar")

# This is an abstract base class
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

# This class inherits from SigexModelComponent
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

#' A base class for SigexParamTS types
#' @export
setClass("SigexParamTS",
	slots = c(
		model_class = "character"
	),
	prototype = list(
		model_class = NULL
	)
)

# A SigexParam has three things:
# 1. A list of K GCDs
# 2. The time series parameters whose exact form depends on the class of this latent process
# 3. Regression parameters
#' @export
setClass("SigexParam",
	slots = c(
		N = "integer",
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

#' @export
setClass("SigexParamARMA",
	contains = "SigexParamTS",
	slots = c(ar = "numeric", ma = "numeric"),
	prototype = list(model_class = "ARMA")
)

#' @export
setClass("SigexParamVARMA",
	contains = "SigexParamTS",
	slots = c(ar = "array", ma = "array"),
	prototype = list(model_class = "VARMA")
)

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

