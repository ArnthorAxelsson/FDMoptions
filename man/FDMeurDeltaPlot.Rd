\name{FDMeurDeltaPlot}
\alias{FDMeurDeltaPlot}
\title{Delta plot of European option using implicit finite difference approximation}
\description{
  Plots option delta as price described by underlying asset..
}
\usage{
FDMeur.DeltaPlot(K, Rf, sigma, TypeFlag = 'call', t=0, Time=1, S.max=2*K)
}
\arguments{
  \item{K}{Strike of the option.}
  \item{Rf}{Risk-free rate.}
  \item{sigma}{Measure of volatility.}
  \item{TypeFlag}{Decides whether a call or put is priced. Has to be either 'call' or 'put'. Default is 'call'.}
  \item{t}{Values option at time t, where (T-t) is time to maturity. Default is t = 0.}
  \item{Time}{Time at maturity.}
  \item{S.max}{Largest value of underlying asset to perform analysis on. Default is S.max = 2*K.}
}
\return{
  Delta plot of the option over (0, S.max).
}
\author{Arnthor Axelsson \email{arnthor@nyu.edu} }
\seealso{
  \code{\link{FDMeurThetaPlot}}
}
\examples{
FDMeur.DeltaPlot(50, .01, .25)  # call
FDMeur.DeltaPlot(50, .01, .25, TypeFlag = 'put')  # put
}
\keyword{math, finance}
