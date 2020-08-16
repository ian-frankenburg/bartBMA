library(loo)
library(mvnfast)
library(gtools)
library(randomForest)
library(Metrics)
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

# #####################
# DATA PROCESSING
#####################
N <- 100
p<- 1000

epsilon <- rnorm(N)
xcov <- matrix(runif(N*p), nrow=N)
y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon

epsilontest <- rnorm(N)
xcovtest <- matrix(runif(N*p), nrow=N)
ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+5*xcovtest[,5]+epsilontest


bart_bma_example <- bartBMA.default(x.train = xcov,y.train=y,x.test=xcovtest)
length(bart_bma_example$sumoftrees)
mse(ytest,bart_bma_example$test.preds)
a<-pred_intervals(bart_bma_example,num_iter = 2000,burnin = 500,0.25,0.75)


# #####################
# DATA PROCESSING
#####################
library(mlbench)
data(BostonHousing)
y <- BostonHousing$medv
X <- BostonHousing[,-which(colnames(BostonHousing)=="medv")]
X$chas <- as.numeric(X$chas)
dat <- cbind(y,X)


# #####################
# DATA PROCESSING
#####################
# abalone <- read.csv("~/Downloads/abalone.data", header=FALSE)
# abalone <- abalone[complete.cases(abalone),]
# X <- abalone[,-which(colnames(abalone)=="V9")]
# X[,1] <- as.numeric(as.factor(X[,1]))
# y <- abalone$V9
# dat <- cbind(y,X)

# #####################
# DATA PROCESSING
#####################
# xcov <- read.table("~/Downloads/Arcene/arcene_train.data", quote="\"", comment.char="")
# y <- read.table("~/Downloads/Arcene/arcene_train.labels", quote="\"", comment.char="")
# y = as.numeric(y==1)
# dat <- cbind(y,xcov)
# xcovtest <-  read.table("~/Downloads/Arcene/arcene_valid.data", quote="\"", comment.char="")
# ytest <- read.table("~/Downloads/arcene_valid.labels", quote="\"", comment.char="")
# ytest = as.numeric(ytest==1)
#
# xcov=xcov[,1:1000]
# xcovtest=xcovtest[,1:1000]
# #####################
# DATA PROCESSING
#####################
# auto data
library(ISLR)
data("Auto")
dat <- Auto[complete.cases(Auto),]
dat <- dat[,-ncol(dat)]
colnames(dat)[1] <- "y"

### INCLUDE SPURIOUS
M <- 10
spurious <- matrix(runif(nrow(dat)*M,0,1), nrow(dat), M)
colnames(spurious) <- paste0("X", 1:M)
dat <- cbind(dat, spurious)

runs <- 1
mse1=mse2=mse3=mse4=sizes=rep(0,runs)
stackwts=bmawts=bart_bma=list()
#for(q in 1:runs){
train_ind = sample(seq_len(nrow(dat)),size = floor(.9*nrow(dat)), replace=F)
train = as.data.frame(dat[train_ind,])
test = as.data.frame(dat[-train_ind,])
ytest = as.vector(test$y)
xcov <- as.matrix(train[,-1]); y <- as.vector(train$y)
xcovtest <- as.matrix(test[,-1])
{
  start.time2 <- Sys.time()
  bart_bma <- bartBMA.default(x.train = as.matrix(xcov), y.train=y,
                              x.test=as.matrix(xcovtest), gridpoint = 1, c = 1000)
  print(length(bart_bma$sumoftrees))
  end.time2 <- Sys.time()
  print(end.time2 - start.time2)
}

#clas <- predict.bartBMA(bart_bma, newdata = as.matrix(xcovtest))
mse(ytest,bart_bma$test.preds)

# start.time2 <- Sys.time()
# rf=randomForest(x = as.matrix(xcov), y=y)
# end.time2 <- Sys.time()
# time.taken2 <- end.time2 - start.time2
# time.taken2
# mse(ytest,predict(rf, newdata = as.matrix(xcovtest)))

vars=colnames(dat)[-1]
vars[which(varImpScores(bart_bma, bart_bma$bic)!=0)]
varImpPlot(rf)
rownames(rf$importance)[1:10]
# while(length(bart_bma[[q]]$sumoftrees)==1){
#   print("1 model error")
#   train_ind = sample(seq_len(nrow(dat)),size = floor(.8*nrow(dat)), replace=F)
#   train = dat[train_ind,]
#   test = dat[-train_ind,]
#   ytest = as.vector(test$y)
#   xcov <- as.matrix(train[,-1]); y <- as.vector(train$y)
#   xcovtest <- as.matrix(test[,-1])
#   bart_bma[[q]] <- bartBMA(x.train = xcov, y.train=y, x.test=xcovtest, maxOWsize = 10)
# }
foo <- pred_intervals(bart_bma, num_iter = 2000, burnin = 1000,
                      newdata = xcovtest, l_quant = .25, u_quant = .75)
#  get posterior draws of sigma and beta
sigma_chains <- foo[[2]]
beta_chains <- foo[[3]]

parsed <- parseTreeSums(bart_bma)

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
b <- exp(.5*bart_bma$bic-0*max(bart_bma$bic))/sum(exp(.5*bart_bma$bic-0*max(bart_bma$bic)))

stackwts = wts
bmawts = b
bmapred <- predict.bartBMA(bart_bma, newdata = xcovtest, wts=bart_bma$bic, stack=0)
mse(ytest, bart_bma$test.preds)
stackpred <- predict.bartBMA(bart_bma, newdata = xcovtest, wts=wts, stack=1)
mse(ytest, stackpred)
#mse3[q] <- mse(ytest, pseudpred)
# start.time2 <- Sys.time()
# rf <- predict(randomForest(x = xcov, y = y), newdata = xcovtest)
# end.time2 <- Sys.time()
# time.taken2 <- end.time2 - start.time2
# time.taken2
#
# mse4[q] <- mse(ytest, rf)
# print(q)
# #}
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# as.data.frame(cbind(
#   "BMA" = mse1,
#   "Stacking" = mse2,
#   "RF" = mse4)
# )
