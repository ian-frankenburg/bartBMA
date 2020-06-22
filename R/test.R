if(0){
  N <- 100
  p<- 100
  set.seed(100)

  epsilon <- rnorm(N)
  xcov <- matrix(runif(N*p), nrow=N)
  y <- sin(pi*xcov[,1]*xcov[,2]) + 20*(xcov[,3]-0.5)^2+10*xcov[,4]+5*xcov[,5]+epsilon

  epsilontest <- rnorm(N)
  xcovtest <- matrix(runif(N*p), nrow=N)
  ytest <- sin(pi*xcovtest[,1]*xcovtest[,2]) + 20*(xcovtest[,3]-0.5)^2+10*xcovtest[,4]+5*xcovtest[,5]+epsilontest


  bart_bma <- bartBMA(x.train = xcov,y.train=y,x.test=xcovtest)
  foo <- pred_intervals(bart_bma,num_iter = 5000, burnin = 1000, l_quant = .25, u_quant = .75)
  #  get posterior draws of sigma and beta
  sigma_chains <- foo[[2]]

  beta_chains <- foo[[3]]
  beta_chain1 <- baz[[1]]
  beta_chain2 <- baz[[2]]
}

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



log_like_mat <- loolist <- list()
length(log_like_mat) <- 2
length(loolist) <- 2
i=j=n=1
for(i in 1:length(sigma_chains)){
  sigma_mcmc <- sigma_chains[[i]]
  beta_mcmc <- beta_chains[[i]]
  w <- W[[i]]
  loglik <- matrix(0, nrow=length(sigma_mcmc), ncol=length(y))
  for(j in 1:(nrow(beta_mcmc))){
    for(n in 1:length(y)){
      loglik[j,n] = dnorm(y[n], mean = w[n,]%*%beta_mcmc[j,], sd = sigma_mcmc[j], log=T)
    }
  }
  log_like_mat[[i]] <- loglik
  loolist[[i]] <- loo(loglik)
}
library(loo)
library(rstanarm)
loo_model_weights(loolist, method = "pseudobma")


