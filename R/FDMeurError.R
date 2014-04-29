FDMeur.error <- function(S.0, K, Rf, sigma, TypeFlag = 'call', t=0, Time=1, 
                         S.max=2*max(S.0,K), dim.t=10*Time, dim.S=S.max){
  #' Price a European option at (S.0, t) using FDM
  #' 
  #' Compares a FDM approximated option price with the closed form 
  #' Black-Scholes formula and returns the percentage difference
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
     #' @return Percentage error of PDE from Black-Scholes
     #' 
     #' @examples
     #' FDMeur.error(70, 50, .01, .25)  # call
     #' FDMeur.error(70, 50, .01, .25, TypeFlag = 'put')  # put
     #' 
     #' @seealso \code{\link{FDMeurCall.table}}, \code{\link{FDMeurPut.table}}, 
     #'          \code{\link{FDMeur.price}}, \code{\link{FDMeur.DeltaPlot}},
     #'          \code{\link{FDMeur.ThetaPlot}}
     
     # Error handling
     stopifnot(TypeFlag == 'call'|| TypeFlag =='put')
     # Compute variables for Black-Scholes
     d.1 <- (log(S.0/K)+(Rf+sigma^2/2)*(Time-t))/(sigma*sqrt(Time-t))
     d.2 <- d.1 - sigma*sqrt(Time-t)
     # Get FDM valuation price and Black-Scholes price
     if(TypeFlag == 'call'){
       FDM.price <- FDMeur.price(S.0, K, Rf, sigma, TypeFlag, t, Time, 
                                 S.max, dim.t, dim.S)
       BS.price <- pnorm(d.1)*S.0-pnorm(d.2)*K*exp(-Rf*(Time-t))
     } else{
       FDM.price <- FDMeur.price(S.0, K, Rf, sigma, TypeFlag, t, Time, 
                                 S.max, dim.t, dim.S)
       BS.price <- pnorm(-d.2)*K*exp(-Rf*(Time-t))-pnorm(-d.1)*S.0
     }
     # Return the %-error of the PDE approximation
     return((1-FDM.price/BS.price)*100)
}