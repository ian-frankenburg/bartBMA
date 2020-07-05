varIncProb<-function(object, score, numvars, sumoftrees,...){
  # imp_vars2=get_weighted_var_imp(num_vars=object$numvars,BIC=score,sum_trees=object$sumoftrees)
  # res<-apply(imp_vars2[[4]],2,sum)
  # #create varImpPlot command
  # vIP<-rep(NA,length(res))
  # total_weighted_var_counts<-sum(res)
  # #now get variable inclusion probabilities  
  # vIP<-res/total_weighted_var_counts
  # class(vIP)<-"varIncProb.bartBMA"  
  # vIP
  imp_vars2=get_weighted_var_imp(num_vars=numvars,BIC=score,sum_trees=sumoftrees)
  res<-apply(imp_vars2[[4]],2,sum)
  #create varImpPlot command
  vIP<-rep(NA,length(res))
  total_weighted_var_counts<-sum(res)
  #now get variable inclusion probabilities  
  vIP<-res/total_weighted_var_counts
  class(vIP)<-"varIncProb.bartBMA"  
  vIP
}
