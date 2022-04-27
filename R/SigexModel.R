#' @export
SigexModel = function(N) {
	N = as.integer(N)
	new("SigexModel", N = N, mdl = list(), components = list())
}

#' @export
setValidity("SigexModel", function(object) {
	if (object@N < 0) {
		stop(sprintf("N = %d is not a valid dimension for series", N))
	}

	return(TRUE)
})

#' @export
setMethod("show", "SigexModel", function(object) {
	N = object@N
	K = length(object@components)
	printf("SigexModel for %d-dimensional series with %d component(s)\n", N, K)
	for (k in seq_len(K)) {
		printf("%d) ", k)
		show(object@components[[k]])
	}
	if (!is.null(object@mdl$regress)) {
		printf("Regression model is set\n")
	}
	return(invisible(NULL))
})

# TBD: Most of the add method code would be pretty similar for other kinds of
# time series models. Do we want to try to improve that? E.g., I think all
# types of components could provide a vector of orders that are meaningful to
# themselves, and that would solve the problem.

# TBD: Should vrank and bounds also be members of SigexComponents? If they were,
# one benefit would be that it would be easier to print them in the show method.

# Below, object and component are required arguments. vrank and bounds may be
# omitted; if they are, we will try to put something reasonable there.

#' @export
setMethod("addComponent",
	c(object = "SigexModel", component = "SigexModelComponent", vrank = "ANY", bounds = "ANY"),
	function(object, component, vrank, bounds) {
		# Set a default value for vrank if missing
		if (missing(vrank)) {
			vrank = seq(1, object@N)
		}
		stopifnot(is.numeric(vrank))
		stopifnot(all(vrank %in% 1:object@N))

		# Set a default value for bounds if missing
		if (missing(bounds)) {
			bounds = c(-Inf, Inf, -Inf, Inf)
		}

		stopifnot(is.numeric(bounds))
		stopifnot(length(bounds) == 4)
		if (bounds[1] >= bounds[2]) {
			stop("Require bounds[1] < bounds[2]")
		}
		if (bounds[3] >= bounds[4]) {
			stop("Require bounds[3] < bounds[4]")
		}

		# Add component to internal mdl structure
		K = length(object@components)
		order = orderVector(component)
		if (K == 0) {
			# If mdl slot is an empty list, pass NULL instead as the initial model
			object@mdl = sigex.add(NULL, vrank, component@model_class, order,
				bounds, component@epithet, component@delta)
		} else {
			object@mdl = sigex.add(object@mdl, vrank, component@model_class,
				order, bounds, component@epithet, component@delta)
		}

		# Add description of component to list
		object@components[[K + 1]] = component

		invisible(object)
	}
)

#' @export
setMethod("setRegComponent",
	c(object = "SigexModel", data_ts = "ts", d = "numeric"),
	function(object, data_ts, d) {
		object@mdl = sigex.meaninit(object@mdl, data.ts = data_ts, d = d)
		invisible(object)
	}
)



#' @export
setMethod("to_sigex",
		  c(object = "SigexModel"),
		  function(object) {
		  	out <- vector(mode = 'list', length = 4)

		  	# ranks comp of mdl
		  	out[[1]] <- object@mdl$ranks

		  	# type comp of mdl
		  	out[[2]] <- object@mdl$type

		  	# delta comp of mdl
		  	out[[3]] <- object@mdl$diffop

		  	# regressor comp of  mdl
		  	out[[4]] <- object@mdl$regress

		  	return(out)
		  }
)


# convert to SigexModel from sigex mdl
#' @export
setMethod("asSigexModel",
		  c(mdlObject = "list"),
		  function(mdlObject) {

		  	N <- length(mdlObject$ranks[[1]])
		  	K <- length(mdlObject[[1]]) # number of model components

		  	outSigexModel <- SigexModel(N)

		  	for(k in K){
		  		compTyp <- mdlObject$type[[k]][[1]]
		  		compPar <- mdlObject$type[[k]][[2]]
		  		compEpi <- mdlObject$type[[k]][[4]]
		  		diffOp  <- mdlObject$diffop[[k]]
		  		# switch to create output of the correct class
		  		# Right now only support: "arma" and "varma"
		  		if (compTyp == "arma"){

		  			outSigexModel <- outSigexModel %>%
		  				addComponent(SigexModelComponentARMA(
		  							  	p = compPar[1],
		  							  	q = compPar[1],
		  							  	epithet = compEpi,
		  							  	delta = diffOp))

		  		} else if (compTyp == "varma"){

		  			outSigexModel <- outSigexModel %>%
		  				addComponent(SigexModelComponentVARMA(
		  					p = compPar[1],
		  					q = compPar[1],
		  					epithet = compEpi,
		  					delta = diffOp))

		  		} else {
		  			msg <- paste("components of type", compTyp, "not yet supports")
		  			stop(msg)
		  		}
		  	}

		  	return(outSigexModel)

		  }
)
