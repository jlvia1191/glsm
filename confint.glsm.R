confint.glsm <- function(object, parm, level = 0.95, ...) {
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }
  
  if ((length(level) != 1L) || is.na(level) || (level <= 0) || (level >= 1)) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  
  if (!missing(parm)) {
    if (!is.character(parm)) {
      stop("`parm` must be a valid string.")
    }
    if (!all(parm %in% names(object$coef))) {
      stop(paste("The coefficient", parm, "is not present in the model."))
    }
  }
  
  alpha <- 1 - level
  Z <- qnorm(1 - alpha / 2)
  
  if (missing(parm)) {
    li <- object$coefficients - Z * object$Std.Error
    ls <- object$coefficients + Z * object$Std.Error
    
    ret <- cbind(li, ls)
    colnames(ret) <- c(paste0("lower ", 100 * (1 - level) / 2, "%"), paste0("upper ", 100 * (1 - (1 - level) / 2), "%"))
    
    sal <- list(confint = ret, OR = exp(ret[-1, ]), level = level * 100)
  } else {
    if (is.na(object$Std.Error[parm])) {
      stop(paste("El error estándar para el coeficiente", parm, "es NA."))
    }
    
    li <- object$coef[parm] - Z * object$Std.Error[parm]
    ls <- object$coef[parm] + Z * object$Std.Error[parm]
    
    ret <- cbind(li, ls)
    colnames(ret) <- c(paste0("lower ", 100 * (1 - level) / 2, "%"), paste0("upper ", 100 * (1 - (1 - level) / 2), "%"))
    
    if (parm == "(Intercept)") {
      sal <- list(confint = ret, OR = "-", level = level * 100)
    } else {
      sal <- list(confint = ret, OR = exp(ret), level = level * 100)
    }
  }
  
  class(sal) <- "confint.glsm"
  return(sal)
}

print.confint.glsm <- function(x, ...) {
  cat( x$level, ".0%", " confidence intervals for coefficients ", "\n", sep = "")
  print(x$confint)
  
  cat("\n", x$level, ".0%", " confidence intervals for Odds Ratios","\n",   sep = "")
  print(x$OR)
}