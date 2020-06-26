predict.bartStack<-function(object,newdata){
  # use stacking weights
  preds<-get_BART_BMA_test_predictions(newdata,as.numeric(wts),object$sumoftrees,object$y_minmax)
  orig_preds<-preds[[1]]
  class(orig_preds)<-"predict.bartBMA"
  orig_preds
}
