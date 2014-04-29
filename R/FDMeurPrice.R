FDMeur.price <- function(S.0, K, Rf, sigma, TypeFlag = 'call', t=0, Time=1, 
                         S.max=2*max(S.0,K), dim.t=10*Time, dim.S=S.max){
  #' Price a European option at (S.0, t) using FDM
  #' 
  #' Valuates European options at (S.0,t) using finite difference
  #' numerical approximation of Black-Scholes in PDE form
  #' 
  #' @param S.0 Value of underlying asset
     #' @param K Strike of the option
     #' @param Rf Risk-free rate
     #' @param sigma Measure of volatility
     #' @param TypeFlag Decides whether a call or put is priced. Has to be either
     #'        'call' or 'put'. Default is 'call'
     #' @param t Values option at time t, where (T-t) is time to maturity.
     #'        Default is t = 0
     #' @param Time Time to maturity
     #' @param S.max Largest value of underlying asset to perform analysis
     #'   on. Default is S.max = 2*max(S.0,K)
     #' @param dim.t Number of time intervals of mesh. Default is dim.t = 10*Time
     #' @param dim.S Number of spatial intervals of mesh. Default is dim. S = S.max
     #' 
     #' @return Option price at (S.0, t)
     #' 
     #' @export
     #' 
     #' @examples
     #' FDMeur.price(70, 50, .01, .25)  # call
     #' FDMeur.price(70, 50, .01, .25, TypeFlag = 'put')  # put
     #' 
     #' @seealso \code{\link{FDMeurCall.table}}, \code{\link{FDMeurPut.table}}, 
     #'          \code{\link{FDMeur.error}}, \code{\link{FDMeur.DeltaPlot}},
     #'          \code{\link{FDMeur.ThetaPlot}}
     
     # Error handling
     stopifnot(TypeFlag == 'call'|| TypeFlag =='put')
     # Values for 'call' or 'put'
     if(TypeFlag == 'call'){
       table <- FDMeurCall.table(K, Rf, sigma, Time, S.max, dim.t, dim.S)
     } else{
       table <- FDMeurPut.table(K, Rf, sigma, Time, S.max, dim.t, dim.S)
     }
     # Returns option value at (S.0, t)
     return(table[(S.0*dim.S/S.max + 1),(t*dim.t/Time + 1)])
}