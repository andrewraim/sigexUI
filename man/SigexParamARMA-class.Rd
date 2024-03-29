% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClass.R
\docType{class}
\name{SigexParamARMA-class}
\alias{SigexParamARMA-class}
\title{SigexParamARMA}
\description{
Parameters structure for an \\eqn{N}-dimensional series with independent
\eqn{\text{ARMA}(p,q)} coordinates.
}
\details{
TBD: What about independent series with different orders?
}
\section{Slots}{

\describe{
\item{\code{ar}}{An \eqn{N \times p} matrix, where element \eqn{(i,j)} represents
the \eqn{j}th AR coefficient for the \eqn{i}th series.}

\item{\code{ma}}{An \eqn{N \times q} matrix, where element \eqn{(i,j)} represents
the \eqn{j}th AR coefficient for the \eqn{i}th series.}
}}

