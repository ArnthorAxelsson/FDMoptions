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
     
     price <-  FDMeur.price(0:S.max, K, Rf, sigma, TypeFlag='call')
     #par(mar=c(5,4,4,4)+.1)
     #plot(1:(S.max+1), c(0,diff(price, lag=1)),type="l",col="blue",axes=F,xlab="",ylab="")
     #axis(4)
     #mtext("Delta",side=4,line=3)
     #par(new=T)
     plot(price, type='l', cex.main = 0.9, xlab='S', ylab='Option price', 
          main=paste('EUR ', TypeFlag, ' - delta plot\n',
                     'K =',K,', Rf =',Rf,', sigma =',sigma,', T =',Time), 
          panel.first=c(abline(v=K, lty=2, col='red'),abline(h=0, col='gray')))
     #legend("topleft",col=c("black","blue","red"),lty=c(1,1,2),legend=c("Option","Delta", "K"), cex=.8)
}