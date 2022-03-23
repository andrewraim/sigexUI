# Constructor for class SigexFit object
#' @export
SigexFit = function(N) {
	new("SigexFit",
		N = N,
		gcds = list(),
		ts_params = list(),
		reg_param = numeric(0),
		optim_output = list())
}


