\name{FDMeurError}
\alias{FDMeurError}
\title{Error term of a European option at (S.0, t) using implicit finite difference}
\description{
  Compares a FDM approximated option price with the closed form Black-Scholes formula and returns the percentage difference.
}
\usage{
FDMeur.price(S.0, K, Rf, sigma, TypeFlag = 'call', t=0, Time=1, S.max=2*max(S.0,K), dim.t=10*Time, dim.S=S.max)
}
\arguments{
  \item{S.0}{Value of underlying asset.}
  \item{K}{Strike of the option.}
  \item{Rf}{Risk-free rate.}
  \item{sigma}{Measure of volatility.}
  \item{TypeFlag}{Decides whether a call or put is priced. Has to be either 'call' or 'put'. Default is 'call'.}
  \item{t}{Values option at time t, where (T-t) is time to maturity. Default is t = 0.}
  \item{Time}{Time at maturity.}
  \item{S.max}{Largest value of underlying asset to perform analysis on. Default is S.max = 2*K.}
  \item{dim.t}{Number of time intervals of mesh. Default is dim.t = 10*Time.}
  \item{dim.S}{Number of spatial intervals of mesh. Default is dim. S = S.max.}
}
\return{
  Percentage error of PDE from Black-Scholes.
}
\author{Arnthor Axelsson \email{arnthor@nyu.edu} }
\seealso{
  \code{\link{FDMeurPrice}}
}
\examples{
FDMeur.error(70, 50, .01, .25)  # call
FDMeur.error(70, 50, .01, .25, TypeFlag = 'put')  # put
}
\keyword{math, finance}
