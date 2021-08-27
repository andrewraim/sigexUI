
# par is a list of four things:
# 1. A list of K lower-triangular matrices L[[1]], ..., L[[K]]
# 2. A list of K vectors D[[1], ..., D[[K]]
# 3. The time series parameters whose exact form depends on the class of this latent process
# 4. Regression parameters
get_par = function(L_list, D_list, ts_par, beta, mdl_type)
{
	N = dim(ts_par)[1]
	K = length(L_list)
	stopifnot(K == length(D_list))

	for (k in 1:K) {
		stopifnot(N == nrow(L_list[[k]]))
		stopifnot(N == ncol(L_list[[k]]))
		stopifnot(N == length(D_list[[k]]))
	}

	# TBD: should we check for some condition on beta?

	structure(list(
		L_list = L_list,
		D_list = D_list,
		ts_par = get_ts_par(ts_par, mdl_type)
		beta = beta),
	class = "sigex_par")
}

# Go through and check everything to make sure it's in the right format.
# TBD: What seems more appropriate are separate functions like: get_arma_par and get_varma_par
# Is there a way to declare that they have a common base class?
# Check out Wickham's chapters on OOP in R: https://adv-r.hadley.nz/oo.html
get_ts_par = function(ts_par, mdl_type)
{
	N = dim(ts_par)[1]
	mdl_class = mdl_type[[1]]
	mdl_order = mdl_type[[2]]
	mdl_bounds = mdl_type[[3]]

	# ARMA
	if (mdl_class %in% c("arma")) {
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		stopifnot(dim(ts_par) == c(N, p.order + q.order))
	}

	# Stabilized ARMA
	if (mdl_class %in% c("arma.stab")) {
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		stopifnot(dim(ts_par) == c(p.order + q.order))
	}

	# SARMA
	if (mdl_class %in% c("sarma")) {
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		ps.order = mdl_order[3]
		qs.order = mdl_order[4]
		s.period = mdl_order[5]
		stopifnot(dim(ts_par) == c(N, p.order + q.order + ps.order + qs.order))
	}

	# Stabilized SARMA
	if (mdl_class %in% c("sarma.stab")) {
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		ps.order = mdl_order[3]
		qs.order = mdl_order[4]
		s.period = mdl_order[5]
		stopifnot(dim(ts_par) == c(p.order + q.order + ps.order + qs.order))
	}

	# VARMA
	if (mdl_class %in% c("varma")) {
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		stopifnot(dim(ts_par) == c(N, N, p.order + q.order))
	}

	# SVARMA
	if (mdl_class %in% c("svarma"))
	{
		p.order = mdl_order[1]
		q.order = mdl_order[2]
		ps.order = mdl_order[3]
		qs.order = mdl_order[4]
		s.period = mdl_order[5]
		stopifnot(dim(ts_par) == c(N, N, p.order + q.order + ps.order + qs.order))
	}

	# cycles
	if (mdl_class %in% c("bw","bw.stab","bal","bal.stab"))
	{
		low.rho = mdl_bounds[1]
		upp.rho = mdl_bounds[2]
		low.omega = mdl_bounds[3]
		upp.omega = mdl_bounds[4]
		stopifnot(length(ts_par) == 2)
	}

	# Damped trend
	if (mdl_class %in% c("damped"))
	{
		low = mdl_bounds[1]
		upp = mdl_bounds[2]
		stopifnot(length(ts_par) == 1)
	}

	return(ts_par)
}
