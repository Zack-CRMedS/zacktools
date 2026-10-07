optcut <- function(prob.predict, actual.class) {
  pred <- prediction(prob.predict,actual.class)
  roc.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
  }
  return(opt.cut(roc.perf, pred))
}
