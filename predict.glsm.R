predict.glsm <- function(object, newdata, type = c("class", "link", "response", "odd", "OR"), level = 0.95,...) {
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }
  
  if ((length(level) != 1L) || is.na(level) || (level <= 0) || (level >= 1)){
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  type <- match.arg(type)
  
  ws <- object$calc$weights
  bs <- object$calc$bias
  
  if (is.null(newdata)) {
    x <- object$data
  } else {
    x <- as.matrix(newdata)
  }
  
  eta <- sapply(1:length(ws), function(c) {
    x %*% ws[[c]] + bs[[c]]
  })
  
  sigmoid <- function(z) {
    epsilon <- 1e-10
    return(1 / (1 + exp(-z + epsilon)))
  }
  
  sigs <- t(apply(eta, 1, sigmoid))
  
  cl <- apply(sigs, 1, which.max)
  odds <- sigs / (1 - sigs)
  or <- exp(object$coef)
  
  res <- switch (
    type,
    class = cl,
    link = eta,
    response = sigs,
    odd = odds,
    OR = or
  )
  
  return(res)
}