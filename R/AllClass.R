# This is an abstract base class

#' @export
setClass("SigexModelComponent",
	slots = c(
		model_class = "character",
		epithet = "character"
	),
	prototype = list(
		model_class = "NULL",
		epithet = "NULL"
	)
)

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
		gcd_list = "list",
		ts_param_list = "list",
		reg_param = "numeric"
	),#
	prototype = list(
	#	gcd_list = list(),
	#	ts_param_list = NA_real_,
		reg_param = NA_real_
	)
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
		mdl = "list",
		components = "list"
	),
	prototype = list(
		mdl = NULL,
		components = list()
	)
)

