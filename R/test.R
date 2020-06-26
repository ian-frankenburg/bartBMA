# library(loo)
# library(rstanarm)
# library(mvnfast)
# library(randomForest)
# library(Metrics)
# if(0){
#   N <- 25
#   p<- 1000
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
#   library(mlbench)
#   data(BostonHousing)
#   y <- BostonHousing$medv
#   X <- BostonHousing[,-which(colnames(BostonHousing)=="medv")]
#   X$chas <- as.numeric(X$chas)
#   dat <- cbind(y,X)
#   train_ind = sample(seq_len(nrow(dat)),size = floor(.8*nrow(dat)), replace=F)
#   train = dat[train_ind,]
#   test = dat[-train_ind,]
#   ytest = as.vector(test$y)
#   start.time <- Sys.time()
#   xcov <- as.matrix(train[,-1]); y <- as.vector(train$y)
#   xcovtest <- as.matrix(test[,-1])
# 
#   bart_bma <- bartBMA(x.train = xcov,y.train=y,x.test=xcovtest)
#   bmapred <- predict(bart_bma, newdata = xcovtest)
#   rf <- predict(randomForest(x = xcov, y = y), newdata = xcovtest)
#   rmse(ytest, bmapred)
#   rmse(ytest, rf)
#   foo <- pred_intervals(bart_bma, num_iter = 5000, burnin = 1000, l_quant = .25, u_quant = .75)
#   #  get posterior draws of sigma and beta
#   sigma_chains <- foo[[2]]
#   beta_chains <- foo[[3]]
# }
# ### BUILD SUM-OF-TREE LIKELIHOOD IN ANOVA FORM
# parseTreeSums <- function(bart_bma){
#   foo <- bart_bma$sumoftrees
#   bar <- bart_bma$obs_to_termNodesMatrix
#   Js <- leafParameters <- W <- O <- list()
#   w <- 0
#   i=j=m=1
#   for(i in 1:length(bar)){
#     # get a single sum of trees ie a single set of trees
#     foo2 <- foo[[i]]
#     # get set of matrices representing tree topologies within sum
#     bar2 <- bar[[i]]
#     # create list to store multiple sums of trees
#     holder <- list()
#     holder2 <- c()
#     for(j in 1:length(bar2)){
#       # get individual tree within a single sum of trees
#       mat <- bar2[[j]]
#       # get leaf node identifiers
#       tnodes <- as.data.frame(foo2[[j]])
#       # gets leaves
#       leaflocs <- which(tnodes$status==-1)
#       # get leaf nodes parameters
#       leaf.mu <- tnodes$mean[leaflocs]
#       tnodes <- rownames(tnodes[leaflocs, ])
#       # X will be ANOVA matrix
#       J <- matrix(0, nrow=nrow(mat), ncol=length(tnodes))
#       colnames(J) <- tnodes
#       for(m in 1:nrow(mat)){
#         path <- mat[m,]
#         path <- path[which(path!=0)]
#         for(l in 1:(length(path))){
#           if(as.character(path[l]) %in% tnodes){
#             J[m, as.character(path[l])] <- 1
#           }
#         }
#       }
#       holder[[j]] <- J
#       holder2[[j]] <- leaf.mu
#       # get terminal node predicted values
#     }
#     Wmat <- c()
#     for(k in 1:length(holder)){
#       Wmat <- cbind(Wmat,matrix(unlist(holder[k]), nrow=nrow(J), ncol=length(holder2[[k]])))
#     }
#     #W[[i]] <- matrix(unlist(holder), nrow=nrow(J), ncol=length(holder))
#     W[[i]] <- Wmat
#     O[[i]] <- unlist(holder2)
#     Js[[i]] <- holder
#     leafParameters[[i]] <- holder2
#   }
#   return(
#     list(
#       "leafParameters" = leafParameters,
#       "anovas" = Js,
#       "W" = W,
#       "O" = O
#     )
#   )
# }
# parsed <- parseTreeSums(bart_bma)
# 
# #### FOR TESTING POINTWISE LOG-LIKELIHOOD BATCH COMPUTING CODE
# # mean <- rep(0,length(y))
# # i=j=1
# # mean_list <- list()
# # # trying to just construct mean function by defintion
# meanbuilder <- function(anovas, beta, mcmcdraw){
#   for(i in 1:length(anovas)){
#     sum_trees <- parsed$anovas[[i]]
#     params <- beta_chains[[i]]
#     for(j in 1:length(sum_trees)){
#       temp <- as.matrix(sum_trees[[j]])
#       mean = mean + temp %*% params[mcmcdraw, 1:ncol(temp)]
#       params <- params[,-c(1:ncol(temp))]
#       j=j+1
#     }
#     mean_list[[i]] = mean
#   }
#   return(mean_list)
# }
# a <- rep(0,4000)
# for(i in 1000:5000){
# mcmcdraw=i
# t = 1
# obs = 2
# mu <- meanbuilder(parsed$anovas, beta_chains, mcmcdraw)
# sigma <- sigma_chains[[t]][mcmcdraw]
# a[i] <- dnorm(y[obs], mu[[t]][obs], sd = sigma, log=T)
# }
# plot(density(a))
# View(log_like_mat[[t]])
# 
# ### USE POSTERIOR DRAWS TO EVALUATE POINTWISE LIKELIHOOD NEEDED FOR STACKING
# temp <- rep(0, length(sigma_chains))
# log_like_mat <- loolist <- list()
# length(log_like_mat) <- length(bart_bma$sumoftrees)
# length(loolist) <- length(bart_bma$sumoftrees)
# i=j=n=1
# for(i in 1:length(sigma_chains)){
#   sigma_mcmc <- sigma_chains[[i]]
#   beta_mcmc <- beta_chains[[i]]
#   w <- parsed$W[[i]]
#   loglik <- matrix(0, nrow=length(sigma_mcmc), ncol=length(y))
#   beta = parsed$O[[i]]
#   for(j in 1:(nrow(beta_mcmc))){
#     for(n in 1:length(y)){
#       loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta_mcmc[j,], sd = sigma_mcmc[j], log=T)
#       #loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta, sd = sigma_mcmc[j], log=T)
#     }
#   }
#   log_like_mat[[i]] <- loglik
#   loolist[[i]] <- loo(loglik)
# }
# loolist
# loo_model_weights(loolist, method = "stacking")
