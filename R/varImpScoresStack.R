varImpScoresStack<-function(numvars, wts, sumoftrees){
  #object will be bartBMA object.
  imp_vars2=get_weighted_var_imp(num_vars=numvars,BIC=wts,sum_trees=sumoftrees)
  res<-apply(imp_vars2[[4]],2,sum)
  class(res)<-"varImpScoresStack.bartBMA"  
  res
}