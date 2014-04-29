\name{FDMeurThetaPlot}
\alias{FDMeurThetaPlot}
\title{Theta plot of European option using implicit finite difference approximation}
\description{
  Plots option theta as negative price interval described by time to maturity.
}
\usage{
FDMeur.ThetaPlot(S.0, K, Rf, sigma, TypeFlag = 'call', Time=1, S.max=2*K)
}
\arguments{
  \item{S.0}{Value of underlying asset.}
  \item{K}{Strike of the option.}
  \item{Rf}{Risk-free rate.}
  \item{sigma}{Measure of volatility.}
  \item{TypeFlag}{Decides whether a call or put is priced. Has to be either 'call' or 'put'. Default is 'call'.}
  \item{Time}{Time at maturity.}
  \item{S.max}{Largest value of underlying asset to perform analysis on. Default is S.max = 2*K.}
}
\return{
  Theta plot of the option over (0, T).
}
\author{Arnthor Axelsson \email{arnthor@nyu.edu} }
\seealso{
  \code{\link{FDMeurDeltaPlot}}
}
\examples{
FDMeur.ThetaPlot(70, 50, .01, .25)  # call
FDMeur.ThetaPlot(70, 50, .01, .25, TypeFlag = 'put')  # put
}
\keyword{math, finance}
