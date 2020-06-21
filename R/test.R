# if(1){
#   N <- 100
#   p<- 100
#   set.seed(100)
#   
#   epsilon <- rnorm(N)
#   xcov <- matrix(runif(N*p), nrow=N)
#   y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon
#   
#   epsilontest <- rnorm(N)
#   xcovtest <- matrix(runif(N*p), nrow=N)
#   ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+5*xcovtest[,5]+epsilontest
#   
#   
#   bart_bma <- bartBMA(x.train = xcov,y.train=y,x.test=xcovtest)
#   foo <- pred_intervals(bart_bma,num_iter = 5000, burnin = 1000, l_quant = .25, u_quant = .75)
#   # contains posterior draws of sigma for both sums of trees 
#   bar <- foo[[2]]
#   sigma.1 <- bar[1]
#   sigma.2 <- bar[2]
#   plot(density(unlist(sigma.1)^2))
#   plot(density(unlist(sigma.2)^2))
#   
#   # now need to obtain draws of M and store in a matrix in a list
#   
# }
