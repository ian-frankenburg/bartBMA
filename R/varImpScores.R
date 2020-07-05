varImpScores<-function(object,scores,...){
  #object will be bartBMA object.
  ### my code: changed BIC=object$bic to scores arg. in function
  imp_vars2=get_weighted_var_imp(num_vars=object$numvars,BIC=scores,sum_trees=object$sumoftrees)
  res<-apply(imp_vars2[[4]],2,sum)
  class(res)<-"varImpScores.bartBMA"  
  res
}



