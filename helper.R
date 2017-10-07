sigmoid <- function(outputs) {
  return(plogis(outputs))
} 

log_loss <- function(y_true,y_pred) {
  y_pred <- pmax(y_pred , 1e-8)
  y_pred <- pmin(y_pred, 0.999999)
  ll = -mean(y_true*log(y_pred) + (1-y_true)*log(1-y_pred))
  return(ll)
}

binary_accuracy <- function(y_true,y_pred) {
  y_pred <- round(y_pred)
  return(length(which(y_true == y_pred))/length(y_true))
}
