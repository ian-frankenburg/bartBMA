predict.bartBMA<-function(object,newdata, stack=0, wts){
  preds<-get_BART_BMA_test_predictions(newdata,as.numeric(wts),object$sumoftrees,object$y_minmax, stack=stack)
  orig_preds<-preds[[1]]
  class(orig_preds)<-"predict.bartBMA"
  orig_preds
}