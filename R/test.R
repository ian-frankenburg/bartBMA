# library(loo)
# library(mvnfast)
# library(gtools)
# library(randomForest)
# library(Metrics)
# set.seed(314)
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
# 
# 
# # #####################
# # DATA PROCESSING
# #####################
# # library(mlbench)
# # data(BostonHousing)
# # y <- BostonHousing$medv
# # X <- BostonHousing[,-which(colnames(BostonHousing)=="medv")]
# # X$chas <- as.numeric(X$chas)
# # dat <- cbind(y,X)
# 
# 
# # #####################
# # DATA PROCESSING
# #####################
# # abalone <- read.csv("~/Downloads/abalone.data", header=FALSE)
# # abalone <- abalone[complete.cases(abalone),]
# # X <- abalone[,-which(colnames(abalone)=="V9")]
# # X[,1] <- as.numeric(as.factor(X[,1]))
# # y <- abalone$V9
# # dat <- cbind(y,X)
# 
# # #####################
# # DATA PROCESSING
# #####################
# # xcov <- read.table("~/Downloads/Arcene/arcene_train.data", quote="\"", comment.char="")
# # y <- read.table("~/Downloads/Arcene/arcene_train.labels", quote="\"", comment.char="")
# # y = as.numeric(y==1)
# # dat <- cbind(y,xcov)
# # xcovtest <-  read.table("~/Downloads/Arcene/arcene_valid.data", quote="\"", comment.char="")
# # ytest <- read.table("~/Downloads/arcene_valid.labels", quote="\"", comment.char="")
# # ytest = as.numeric(ytest==1)
# #
# # xcov=xcov[,1:1000]
# # xcovtest=xcovtest[,1:1000]
# # #####################
# # DATA PROCESSING
# #####################
# # library(ISLR)
# data("Auto")
# dat <- Auto[complete.cases(Auto),]
# dat <- dat[,-ncol(dat)]
# colnames(dat)[1] <- "y"
# scol = 10
# spur <- matrix(runif(nrow(dat)*scol,0,1),nrow=nrow(dat),ncol=scol)
# xcov <- as.matrix(cbind(dat[,-1],spur))
# y = dat[,1]
# 
# ## add more spurious
# scol = 500
# spur <- matrix(runif(nrow(dat)*scol,0,1),nrow=nrow(dat),ncol=scol)
# xcov <- as.matrix(cbind(xcov,spur))
# 
# # N=1000; p=1000
# # epsilon <- rnorm(N)
# # xcov <- matrix(runif(N*p), nrow=N)
# # y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon+
# #   (xcov[,6]^3-2)^3 + xcov[,7]*3
# 
# smp_size <- floor(0.75 * nrow(xcov))
# 
# train_ind <- sample(seq_len(nrow(xcov)), size = smp_size)
# 
# xcovtrain <- xcov[train_ind, ]
# ytrain <- y[train_ind]
# 
# xcovtest <- xcov[-train_ind, ]
# ytest <- y[-train_ind]
# 
# {
# start.time2 <- Sys.time()
# bart_bma_fit <- bartBMA.default(x.train = xcovtrain,y.train=ytrain,x.test=xcovtest,
#                                  gridpoint = 1, c=5000, pen = 10, maxOWsize = 20)
# end.time2 <- Sys.time(); end.time2 - start.time2
# }
# length(bart_bma_fit$sumoftrees)
# 
# clas <- predict.bartBMA(bart_bma_fit, newdata = xcovtest)
# mse(ytest,clas)
# 
# start.time2 <- Sys.time()
# rf=randomForest(x = as.matrix(xcovtrain), y=ytrain)
# time.taken2 <- end.time2 - start.time2; time.taken2
# mse(ytest,predict(rf, newdata = as.matrix(xcovtest)))
# end.time2 <- Sys.time(); end.time2 - start.time2
# 
# vars=colnames(xcovtrain)
# which(varImpScores(bart_bma_fit, bart_bma_fit$bic)>0)
# varImpPlot(rf)
# 
# foo <- pred_intervals(bart_bma_fit, num_iter = 5000, burnin = 1000,0.25,0.75)
# #  get posterior draws of sigma and beta
# sigma_chains <- foo[[2]]
# beta_chains <- foo[[3]]
# 
# 
# parsed <- parseTreeSums(bart_bma_fit)
# 
# ### USE POSTERIOR DRAWS TO EVALUATE POINT-WISE LIKELIHOOD NEEDED FOR STACKING
# temp <- rep(0, length(sigma_chains))
# log_like_mat <- loolist <- list()
# length(log_like_mat) <- length(bart_bma_fit$sumoftrees)
# length(loolist) <- length(bart_bma_fit$sumoftrees)
# i=j=n=1
# for(i in 1:length(sigma_chains)){
#   sigma_mcmc <- sigma_chains[[i]]
#   beta_mcmc <- beta_chains[[i]]
#   w <- parsed$W[[i]]
#   loglik <- matrix(0, nrow=length(sigma_mcmc), ncol=length(ytrain))
#   beta = parsed$O[[i]]
#   for(j in 1:(nrow(beta_mcmc))){
#     for(n in 1:length(ytrain)){
#       loglik[j,n] = dnorm(ytrain[n], mean = w[n,]%*%beta_mcmc[j,], sd = 1/sigma_mcmc[j], log=T)
#       #loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta, sd = sigma_mcmc[j], log=T)
#     }
#   }
#   log_like_mat[[i]] <- loglik
#   loolist[[i]] <- loo(loglik)#, r_eff = exp(colSums(loglik)))
# }
# wts = loo_model_weights(loolist, method = "stacking")
# b <- exp(bart_bma_fit$bic-max(bart_bma_fit$bic))/sum(exp(bart_bma_fit$bic-max(bart_bma_fit$bic)))
# 
# stackwts = wts
# bmawts = b
# 
# bmapred <- predict.bartBMA(bart_bma_fit, newdata = xcovtest)
# stackpred <- predict.bartStack(bart_bma_fit, newdata = xcovtest, wts)
# #pseudpred <- predict_bartPseud(bart_bma, newdata = xcovtest)
# mse(ytest, bmapred)
# sum(abs(ytest- bmapred))
# mse(ytest, stackpred)
# sum(abs(ytest- stackpred))
# 1-sum(abs(as.numeric(wts)-round(as.numeric(wts),6)))
# sum(round(as.numeric(wts),5)>0)
# sum(round(as.numeric(b),5)>0)
# 
# yes = bart_bma_fit$sumoftrees[which(as.numeric(wts)>1e-4)]
# which(varImpScoresStack(p,wts,yes)>0)
# 
# yes = bart_bma_fit$sumoftrees[which(as.numeric(b)>1e-4)]
# which(varImpScoresStack(p,b,yes)>0)
# #mse3[q] <- mse(ytest, pseudpred)
# # start.time2 <- Sys.time()
# # rf <- predict(randomForest(x = xcov, y = y), newdata = xcovtest)
# # end.time2 <- Sys.time()
# # time.taken2 <- end.time2 - start.time2
# # time.taken2
# 
# # mse4[q] <- mse(ytest, rf)
# # print(q)
# # #}
# # end.time <- Sys.time()
# # time.taken <- end.time - start.time
# # time.taken
# # as.data.frame(cbind(
# #   "BMA" = mse1,
# #   "Stacking" = mse2,
# #   "RF" = mse4)
# # )
# 
# 
# # epsilon <- rnorm(N)
# # xcov <- matrix(runif(N*p), nrow=N)
# # y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon
# #
# # library(ISLR)
# # data("Auto")
# # dat <- Auto[complete.cases(Auto),]
# # dat <- dat[,-ncol(dat)]
# # colnames(dat)[1] <- "y"
# # scol = 5000
# # spur <- matrix(runif(nrow(dat)*scol,0,1),nrow=nrow(dat),ncol=scol)
# # xcov <- as.matrix(cbind(dat[,-1],spur))
# # y = dat[,1]
# #
# # bart_bma_example <- bartBMA.default(x.train = xcov,y.train=y,x.test=xcov, pen=10,gridpoint = 1)
# # length(bart_bma_example$sum)
# # a<-pred_intervals(bart_bma_example,num_iter = 2000,burnin = 500,0.25,0.75)
# #
