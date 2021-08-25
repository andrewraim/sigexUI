
#' Constraint
#'
#' Create a constraint of the form C psi = Q.
get_constraint = function(C, Q)
{
  cat("TBD: check that dimensions are correct")
  ret = cbind(Q, C)
  class(ret) = "constraint"
  return(ret)
}

