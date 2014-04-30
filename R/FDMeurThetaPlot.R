FDMeur.ThetaPlot <- function(S.0, K, Rf, sigma, TypeFlag = 'call', 
                             Time=1, S.max=2*K){
  #' Plot the option theta
  #' 
  #' Plots option theta as price described by underlying asset.
  #' 
  #' @param S.0 Value of underlying asset
     #' @param K Strike of the option
     #' @param Rf Risk-free rate
     #' @param sigma Measure of volatility
     #' @param TypeFlag Decides whether a call or put is priced. Has to be either
     #'        'call' or 'put'. Default is 'call'
     #' @param Time Time to maturity
     #' @param S.max Largest value of underlying asset to perform analysis
     #'   on. Default is S.max = 2*K
     #' 
     #' @return Theta plot of the option over (0, T)
     #' 
     #' @examples
     #' FDMeur.ThetaPlot(70, 50, .01, .25)  # call
     #' FDMeur.ThetaPlot(70, 50, .01, .25, TypeFlag = 'put')  # put
     #' 
     #' @seealso \code{\link{FDMeurCall.table}}, \code{\link{FDMeurPut.table}}, 
     #'          \code{\link{FDMeur.price}}, \code{\link{FDMeur.error}},
     #'          \code{\link{FDMeur.DeltaPlot}}
     
     price <-  FDMeur.price(S.0, K, Rf, sigma, TypeFlag, seq(0,Time,0.1), Time, S.max)
     print(price)
     plot(diff(price, lag=1)/.1, type='l', 
          main=paste('EUR ', TypeFlag, ' - theta plot\n', 'S.0 =', S.0, 
                     ', K =',K,', Rf =',Rf,', sigma =',sigma,', T =',Time), 
          cex.main = 0.9, xlab='t', ylab='Theta', xaxt='n',  
          panel.first=abline(h=0, col='gray'))
     axis(side=1, at=c(2,4,6,8,10)*Time, lab=c(.2,.4,.6,.8,1)*Time)
}