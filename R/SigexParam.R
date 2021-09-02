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

	printf("--- Components of TS parameter list ---\n")
	for (k in 1:K) {
		lab = modelClass(object@ts_param_list[[k]])
		printf("%d: %s\n", k, lab)
	}
	printf("\n")

	printf("--- Regression parameter vector ---\n")
	print(object@reg_param)
})
