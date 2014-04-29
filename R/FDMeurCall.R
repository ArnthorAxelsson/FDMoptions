FDMeurCall.table <- function(K, Rf, sigma, Time=1, S.max=2*K, 
                             dim.t=10*Time, dim.S=S.max){
  #' Produce a price matrix for a European call using FDM
  #' 
  #' Valuates European Call options using finite difference
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
     #' @return Matrix c(S_t, t) of size (dim.t+1)x(dim.S+1). Prompting
     #' call[(S_t+1),(t+1)] gives the option price at time t for
     #' the underlying asset S_t.
     #' 
     #' @import limSolve
     #' @export
     #' 
     #' @examples
     #' FDMeurCall.table(50, .01, .25)
     #' #higher spatial resolution
     #' FDMeurCall.table(50, .01, .25, dim.S=200)
     #' 
     #' @seealso \code{\link{FDMeurPut.table}}
     
     # Error handling
     stopifnot(K > 0, Time > 0, Rf >= 0, sigma >= 0, 
               S.max > 0, dim.t%%1==0, dim.S%%1==0)
     # Computation of variables
     del.t <- Time/dim.t
     del.S <- S.max/dim.S
     # construct vectors for tridiagonal matrix A.h
     i <- 1:(dim.S-1)
     a.i <- 0.5*Rf*del.t*i - 0.5*sigma^2*del.t*i^2
     b.i <- 1 + sigma^2*del.t*i^2 + Rf*del.t
     c.i <- -0.5*Rf*del.t*i - 0.5*sigma^2*del.t*i^2
     # Initiate matrix and populate with boundary values
     call <- matrix(0, ncol=(dim.t+1), nrow=(dim.S+1))
     call[,(dim.t+1)] <- pmax(seq(0,S.max,del.S)-K, 0)
     call[(dim.S+1), ] <- (S.max-K)*exp(-Rf*seq(Time,0,-del.t))
     # Solve internal matrix in reversu using implicit finite difference
     for(n in dim.t:1){
       B.h <- c(call[2:(dim.S-1),(n+1)], (call[dim.S,(n+1)]-a.i[1]*K))
       call[2:dim.S,n] <- Solve.tridiag(a.i[2:(dim.S-1)],b.i,c.i[1:(dim.S-2)],B.h)
       call[,n] <- pmax(seq(0,S.max,del.S)-K, call[,n])
     }
     return(call)
}