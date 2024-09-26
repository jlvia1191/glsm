summary.glsm <- function(object, ...){
  
  TC <- cbind(object$coefficients, object$Std.Error,  object$ExpB, object$Wald, object$DF, object$P.value)
  colnames(TC) <- c("Coef(B)", "Std.Error", "Exp(B)", "Wald", "DF",  "P.value")
  
  
  TAB <- cbind(Deviance = c(object$Dev_Null_vs_Logit, object$Dev_Logit_vs_Complete, object$Dev_Logit_vs_Saturate),
               DF = c(object$Df_Null_vs_Logit, object$Df_Logit_vs_Complete, object$Df_Logit_vs_Saturate),
               P.value = c(object$P.v_Null_vs_Logit, object$P.v_Logit_vs_Complete, object$P.v_Logit_vs_Saturate))
  row.names(TAB) <-c("Null vs Logit", "Logit vs Complete", "Logit vs Saturate")
  
  res <- list(Call = object$call, `comparison test`=TAB, coeff = TC)
  class(res) <- "summary.glsm"
  return(res)
}

print.summary.glsm <- function(x, ...){
  cat("\nCall:\n")
  print(x$Call)
  cat("\nCoefficients: \n",  sep = "")
  printCoefmat(x$coef, P.values=TRUE, has.Pvalue=TRUE)
  cat("\nAnalysis of Deviance (Chi-squared): \n")
  printCoefmat(x$`comparison test`, P.values=TRUE, has.Pvalue=TRUE)
}