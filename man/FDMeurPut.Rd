\name{FDMeurPut}
\alias{FDMeurPut}
\title{European put price matrix using implicit finite difference}
\description{
  Valuates European Put options using finite difference numerical approximation of Black-Scholes in PDE form
}
\usage{
FDMeurPut.table(K, Rf, sigma, Time=1, S.max=2*K, dim.t=10*Time, dim.S=S.max)
}
\arguments{
  \item{K}{Strike of the option.}
  \item{Rf}{Risk-free rate.}
  \item{sigma}{Measure of volatility.}
  \item{Time}{Time to maturity.}
  \item{S.max}{Largest value of underlying asset to perform analysis on. Default is S.max = 2*K.}
  \item{dim.t}{Number of time intervals of mesh. Default is dim.t = 10*Time.}
  \item{dim.S}{Number of spatial intervals of mesh. Default is dim. S = S.max.}
}
\return{
  Matrix p(S_t, t) of size (dim.t+1)x(dim.S+1). Prompting put[(S_t+1),(t+1)] gives the option price at time t for the underlying asset S_t.
}
\author{Arnthor Axelsson \email{arnthor@nyu.edu} }
\seealso{
  \code{\link{FDMeurCall}}
}
\examples{
FDMeurPut.table(50, .01, .25)
#higher spatial resolution
FDMeurPut.table(50, .01, .25, dim.S=200)
}
\keyword{math, finance}
