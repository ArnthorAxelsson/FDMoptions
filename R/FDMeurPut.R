FDMeurPut.table <- function(K, Rf, sigma, Time=1, S.max=2*K, 
                            dim.t=10*Time, dim.S=S.max){
  #' Produce a price matrix for a European put using FDM
  #' 
  #' Valuates European put options using finite difference
  #' numerical approximation of Black-Scholes in PDE form
  #' 
  #' @param K Strike of the option
     #' @param Rf Risk-free rate
     #' @param sigma Measure of volatility
     #' @param Time Time to maturity
     #' @param S.max Largest value of underlying asset to perform analysis
     #'   on. Default is S.max = 2*K
     #' @param dim.t Number of time intervals of mesh. Default is dim.t = 10*Time
     #' @param dim.S Number of spatial intervals of mesh. Default is dim. S = S.max
     #' 
     #' @return Matrix p(S_t, t) of size (dim.t+1)x(dim.S+1). Prompting
     #' put[(S_t+1),(t+1)] gives the option price at time t for
     #' the underlying asset S_t.
     #' 
     #' @export
     #' 
     #' @examples
     #' FDMeurPut.table(50, .01, .25)
     #' #higher spatial resolution
     #' FDMeurPut.table(50, .01, .25, dim.S=200)
     #' 
     #' @seealso \code{\link{FDMeurCall.table}}
     
     del.t <- Time/dim.t
     del.S <- S.max/dim.S
     call <- FDMeurCall.table(K, Rf, sigma, Time, S.max, dim.t, dim.S)
     S <- matrix(rep(seq(0,S.max,del.S), (dim.t +1)), ncol=(dim.t +1), byrow=F)
     PV.K <- exp(-Rf*matrix(rep(seq(Time,0,-del.t), (dim.S + 1)), 
                            nrow=(dim.S +1), byrow=T))
     put <- call - S + K*PV.K
     return(put)
}
