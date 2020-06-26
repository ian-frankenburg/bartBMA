library(loo)
library(rstanarm)
library(mvnfast)
library(gtools)
library(randomForest)
library(Metrics)
N <- 200
p<- 5000

# #####################
# DATA PROCESSING
#####################
# epsilon <- rnorm(N)
# xcov <- matrix(runif(N*p), nrow=N)
# y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon
# 
# epsilontest <- rnorm(N)
# xcovtest <- matrix(runif(N*p), nrow=N)
# ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+5*xcovtest[,5]+epsilontest

# #####################
# DATA PROCESSING
#####################
# library(mlbench)
# data(BostonHousing)
# y <- BostonHousing$medv
# X <- BostonHousing[,-which(colnames(BostonHousing)=="medv")]
# X$chas <- as.numeric(X$chas)
# dat <- cbind(y,X)

# #####################
# DATA PROCESSING
#####################
# auto data
library(ISLR)
data("Auto")
dat <- Auto[complete.cases(Auto),]
dat <- dat[,-ncol(dat)]
colnames(dat)[1] <- "y"

start.time <- Sys.time()
### INCLUDE SPURIOUS
M <- 1000
colnames(dat)[1] <- "y"
spurious <- matrix(runif(nrow(dat)*M,0,1), nrow(dat), M)
colnames(spurious) <- paste0("X", 1:M)
dat <- cbind(dat, spurious)


runs <- 2
mse1=mse2=mse3=mse4=sizes=rep(0,runs)
stackwts=bmawts=list()
for(q in 1:runs){
train_ind = sample(seq_len(nrow(dat)),size = floor(.8*nrow(dat)), replace=F)
train = dat[train_ind,]
test = dat[-train_ind,]
ytest = as.vector(test$y)
xcov <- as.matrix(train[,-1]); y <- as.vector(train$y)
xcovtest <- as.matrix(test[,-1])
bart_bma <- bartBMA(x.train = xcov, y.train=y, x.test=xcovtest)
while(length(bart_bma$sumoftrees)==1){
  print("1 model error")
  train_ind = sample(seq_len(nrow(dat)),size = floor(.8*nrow(dat)), replace=F)
  train = dat[train_ind,]
  test = dat[-train_ind,]
  ytest = as.vector(test$y)
  xcov <- as.matrix(train[,-1]); y <- as.vector(train$y)
  xcovtest <- as.matrix(test[,-1])
  bart_bma <- bartBMA(x.train = xcov, y.train=y, x.test=xcovtest)
}
foo <- pred_intervals(bart_bma, num_iter = 5000, burnin = 1000, l_quant = .25, u_quant = .75)
#  get posterior draws of sigma and beta
sigma_chains <- foo[[2]]
beta_chains <- foo[[3]]

### BUILD SUM-OF-TREE LIKELIHOOD IN ANOVA FORM
parseTreeSums <- function(bart_bma){
  foo <- bart_bma$sumoftrees
  bar <- bart_bma$obs_to_termNodesMatrix
  Js <- leafParameters <- W <- O <- list()
  w <- 0
  i=j=m=1
  for(i in 1:length(bar)){
    # get a single sum of trees ie a single set of trees
    foo2 <- foo[[i]]
    # get set of matrices representing tree topologies within sum
    bar2 <- bar[[i]]
    # create list to store multiple sums of trees
    holder <- list()
    holder2 <- c()
    for(j in 1:length(bar2)){
      # get individual tree within a single sum of trees
      mat <- bar2[[j]]
      # get leaf node identifiers
      tnodes <- as.data.frame(foo2[[j]])
      # gets leaves
      leaflocs <- which(tnodes$status==-1)
      # get leaf nodes parameters
      leaf.mu <- tnodes$mean[leaflocs]
      tnodes <- rownames(tnodes[leaflocs, ])
      # X will be ANOVA matrix
      J <- matrix(0, nrow=nrow(mat), ncol=length(tnodes))
      colnames(J) <- tnodes
      for(m in 1:nrow(mat)){
        path <- mat[m,]
        path <- path[which(path!=0)]
        for(l in 1:(length(path))){
          if(as.character(path[l]) %in% tnodes){
            J[m, as.character(path[l])] <- 1
          }
        }
      }
      holder[[j]] <- J
      holder2[[j]] <- leaf.mu
      # get terminal node predicted values
    }
    Wmat <- c()
    for(k in 1:length(holder)){
      Wmat <- cbind(Wmat,matrix(unlist(holder[k]), nrow=nrow(J), ncol=length(holder2[[k]])))
    }
    #W[[i]] <- matrix(unlist(holder), nrow=nrow(J), ncol=length(holder))
    W[[i]] <- Wmat
    O[[i]] <- unlist(holder2)
    Js[[i]] <- holder
    leafParameters[[i]] <- holder2
  }
  return(
    list(
      "leafParameters" = leafParameters,
      "anovas" = Js,
      "W" = W,
      "O" = O
    )
  )
}
parsed <- parseTreeSums(bart_bma)

#### FOR TESTING POINTWISE LOG-LIKELIHOOD BATCH COMPUTING CODE
# mean <- rep(0,length(y))
# i=j=1
# mean_list <- list()
# # trying to just construct mean function by defintion
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

### USE POSTERIOR DRAWS TO EVALUATE POINT-WISE LIKELIHOOD NEEDED FOR STACKING
temp <- rep(0, length(sigma_chains))
log_like_mat <- loolist <- list()
length(log_like_mat) <- length(bart_bma$sumoftrees)
length(loolist) <- length(bart_bma$sumoftrees)
i=j=n=1
for(i in 1:length(sigma_chains)){
  sigma_mcmc <- sigma_chains[[i]]
  beta_mcmc <- beta_chains[[i]]
  w <- parsed$W[[i]]
  loglik <- matrix(0, nrow=length(sigma_mcmc), ncol=length(y))
  beta = parsed$O[[i]]
  for(j in 1:(nrow(beta_mcmc))){
    for(n in 1:length(y)){
      loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta_mcmc[j,], sd = 1/sigma_mcmc[j], log=T)
      #loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta, sd = sigma_mcmc[j], log=T)
    }
  }
  log_like_mat[[i]] <- loglik
  loolist[[i]] <- loo(loglik)#, r_eff = exp(colSums(loglik)))
}
wts = loo_model_weights(loolist, method = "stacking")
#pseud = loo_model_weights(loolist, method = "pseudobma")
b <- exp(bart_bma$bic)/sum(exp(bart_bma$bic))

stackwts[[q]] = wts
bmawts[[q]] = b

sizes[q] <- length(loolist)
bmapred <- predict.bartBMA(bart_bma, newdata = xcovtest)
stackpred <- predict.bartStack(bart_bma, newdata = xcovtest)
#pseudpred <- predict.bartPseud(bart_bma, newdata = xcovtest)
mse1[q] <- mse(ytest, bmapred)
mse2[q] <- mse(ytest, stackpred)
#mse3[q] <- mse(ytest, pseudpred)
rf <- predict(randomForest(x = xcov, y = y, ntree = length(bart_bma$sumoftrees)), newdata = xcovtest)
mse4[q] <- mse(ytest, rf)
print(q)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
as.data.frame(cbind(
  "BMA"=mse1,
  "Stacking"=mse2,
  "RF"=mse4)
)

