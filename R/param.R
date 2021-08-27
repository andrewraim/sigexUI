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
setValidity("SigexParam", function(object) {
	# TBD: Do some checking
	return(TRUE)
})

#' @export
SigexParam = function(gcd_list, ts_param_list, reg_param = NA_real_) {
	new("SigexParam", gcd_list = gcd_list, ts_param_list = ts_param_list, reg_param = reg_param)
}

#' @export
setMethod("show", "SigexParam", function(object) {
	K = length(object@gcd_list)
	N = dim(object@gcd_list[[1]])[1]
	printf("Param for a K=%d component Sigex model on N=%d dimensional series\n", K, N)

	printf("--- Components of GCD list ---\n")
	print(object@gcd_list)
	printf("\n")

	printf("--- Components of TS parameter list ---\n")
	for (k in 1:K) {
		lab = modelname(object@ts_param_list[[k]])
		printf("%d: %s\n", k, lab)
	}
	printf("\n")

	printf("--- Regression parameter vector ---\n")
	print(object@reg_param)
})
