FDMeur.DeltaPlot <- function(K, Rf, sigma, TypeFlag = 'call', 
                             t=0, Time=1, S.max=2*K){
  #' Plot the option delta
  #' 
  #' Plots option delta as price described by underlying asset.
  #' 
  #' @param K Strike of the option
     #' @param Rf Risk-free rate
     #' @param sigma Measure of volatility
     #' @param TypeFlag Decides whether a call or put is priced. Has to be either
     #'        'call' or 'put'. Default is 'call'
     #' @param t Values option at time t, where (T-t) is time to maturity.
     #'        Default is t = 0
     #' @param Time Time to maturity
     #' @param S.max Largest value of underlying asset to perform analysis
     #'   on. Default is S.max = 2*K
     #' 
     #' @return Delta plot of the option over (0, S.max)
     #' 
     #' @examples
     #' FDMeur.DeltaPlot(50, .01, .25)  # call
     #' FDMeur.DeltaPlot(50, .01, .25, TypeFlag = 'put')  # put
     #' 
     #' @seealso \code{\link{FDMeurCall.table}}, \code{\link{FDMeurPut.table}}, 
     #'          \code{\link{FDMeur.price}}, \code{\link{FDMeur.error}},
     #'          \code{\link{FDMeur.ThetaPlot}}
     
     # Choose 'call' or 'put'
     if(TypeFlag == 'call'){
       table <- FDMeurCall.table(K, Rf, sigma, Time, S.max)
     } else{
       table <- FDMeurPut.table(K, Rf, sigma, Time, S.max)
     }
     plot(table[, (t*10 + 1)], type='l', 
          main=paste('EUR ', TypeFlag, ' - delta plot\n',
                     'K =',K,', Rf =',Rf,', sigma =',sigma,', T =',Time), 
          cex.main = 0.9, xlab='S', ylab='c(S,0)', 
          panel.first=c(abline(v=K, lty=2, col='red'),abline(h=0, col='gray')))
}