\name{FDMeurPrice}
\alias{FDMeurPrice}
\title{Price a European option at (S.0, t) using implicit finite difference}
\description{
  Valuates European options at (S.0,t) using finite difference numerical approximation of Black-Scholes in PDE form.
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
  \item{Time}{Time to maturity.}
  \item{S.max}{Largest value of underlying asset to perform analysis on. Default is S.max = 2*K.}
  \item{dim.t}{Number of time intervals of mesh. Default is dim.t = 10*Time.}
  \item{dim.S}{Number of spatial intervals of mesh. Default is dim. S = S.max.}
}
\return{
  Option price at (S.0, t).
}
\author{Arnthor Axelsson \email{arnthor@nyu.edu} }
\seealso{
  \code{\link{FDMeurCall}}, \code{\link{FDMeurPut}}, \code{\link{FDMeurError}}
}
\examples{
FDMeur.price(70, 50, .01, .25)  # call
FDMeur.price(70, 50, .01, .25, TypeFlag = 'put')  # put
}
\keyword{math, finance}
