# glsm.R

#' Estimation of the log Likelihood of the Saturated Model the response variable \code{Y} takes one of R>1 values.

#' @title Estimation of the log Likelihood of the Saturated Model when the response variable \code{Y} takes one of R>1 values.
#' @description When the response variable \code{Y} takes one of R>1 values, the function \code{glsm()} calculates, among others, the values of the maximum likelihood estimates (ML-estimations) of the  corresponding parameters  in the null, complete, saturated and logistic models and also the estimations of the log likelihood in each of this models. The models null and complete are described by Llinas, Arteta and Tilano (2016, ISSN:2389-8976) in sections 2.2 and 2.3.
#' The saturated model is characterized in section 3 by Orozco, Llinas and Fonseca (2020, ISSN:2389-8976) through the assumptions 1 and 2. Finally,  the logistic model and its assumptions are explained in section 5.
#' Additionally, based on asymptotic theory for these ML-estimations and the score vector, the function \code{glsm()} calculates the values of the approximations for different deviations -2 log L, where L is the likelihood function. Based on these approximations, the function obtains the values of statistics for several hypothesis tests (each with an asymptotic chi-squared distribution): Null vs Logit, Logit vs Complete and Logit vs Saturated.
#' With the function \code{glsm()}, it is possible calculate confidence intervals for the logistic parameters and for the corresponding odds ratio.
#' The asymptotic theory was developed for the case of independent, non-identically distributed variables.  If \code{Y} is polychotomous and the data are grouped in \code{J} populations, it is recommended to use the function \code{glsm()} because it works very well for all \code{K}, the number of explanatory variables.


#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param ref An optional function (default multinomial).
#' @param data An optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which \code{glsm()} is called.
#' @return  \code{glsm} returns an object of class "\code{glsm}".
#'
#' An object of class "\code{glsm}" is a list containing at least the
#'  following components:
#'
#' \item{coefficients}{Vector of coefficients estimations (intercepts and slopes).}
#'
#' \item{coef}{Vector of coefficients estimations (intercepts and slopes).}
#'
#' \item{Std.Error}{Vector of the coefficients’s standard error (intercepts and slopes).}
#'
#' \item{ExpB}{Vector with the exponential of the coefficients (intercepts and slopes).}
#'
#' \item{Wald}{Value of the Wald statistic (with chi-squared distribution).}
#'
#' \item{DF}{Degree of freedom for the Chi-squared distribution.}
#'
#' \item{P.value}{P-value calculated with the Chi-squared distribution. }
#'
#' \item{Log_Lik_Complete}{Estimation of the log likelihood in the complete model.}
#'
#' \item{Log_Lik_Null}{Estimation of the log likelihood in the null model.}
#'
#' \item{Log_Lik_Logit}{Estimation of the log likelihood in the logistic model.}
#'
#' \item{Log_Lik_Saturate}{Estimation of the log likelihood in the saturate model.}
#'
#' \item{Populations}{Number of populations in the saturated model.}
#'
#' \item{Dev_Null_vs_Logit }{Value of the test statistic  (Hypothesis: null vs logistic models).}
#'
#' \item{Dev_Logit_vs_Complete}{Value of the test statistic  (Hypothesis:  logistic vs complete models).}
#'
#' \item{Dev_Logit_vs_Saturate}{Value of the test statistic  (Hypothesis: logistic vs saturated models).}
#'
#' \item{Df_Null_vs_Logit }{Degree of freedom for the test statistic’s distribution (Hypothesis: null vs logistic models).}
#'
#' \item{Df_Logit_vs_Complete }{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}
#'
#' \item{Df_Logit_vs_Saturate}{Degree of freedom for the test statistic’s distribution (Hypothesis: logistic vs saturated models).}
#'
#' \item{P.v_Null_vs_Logit}{P-value for the hypothesis test: null vs logistic models.}
#'
#' \item{P.v_Logit_vs_Complete }{P-value for the hypothesis test:  logistic vs complete models.}
#'
#' \item{P.v_Logit_vs_Saturate}{P-value for the hypothesis test: logistic vs saturated models.}
#'
#' \item{Logit_r}{A matrix of the log odds, taking as a reference the r level of the \code{Y}.}
#'
#' \item{p_hat_complete}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the complete model and without the logistic model).}
#'
#' \item{p_hat_null}{Vector with the probabilities that the outcome variable takes the value 1, given the \code{jth} population (estimated with the null model and without the logistic model).}
#'
#' \item{p_rj}{Matrix with the estimation of each \code{prj}, the probability that the outcome variable takes the value r, given the \code{jth} population (estimated with the logistic model).}
#'
#' \item{odd}{Vector with the values of the odd in each \code{jth} population.}
#'
#' \item{OR}{Vector with the values of the odd ratio for each coefficient of the variables.}
#'
#' \item{z_rj}{Vector with the values of each \code{Zrj} (the sum of the observations in the \code{jth} population).}
#'
#' \item{n_j}{Vector with the \code{nj} (the number of the observations in each \code{jth} population).}
#'
#' \item{p_rj_tilde}{Vector with the estimation of each \code{prj}, the probability that the outcome variable takes the value r, given the \code{jth} population (estimated with the saturated model and without estimate the logistic parameters).}
#'
#' \item{v_rj}{Vector with the variance of the Bernoulli variables in the \code{jth} population and category \code{r}.}
#'
#' \item{m_rj}{Vector with the expected values of \code{Zj} in the \code{jth} population and category \code{r}.}
#'
#' \item{V_rj}{Vector with the variances of \code{Zj} in the \code{jth} population and category \code{r}.}
#'
#' \item{V}{Variance and covariance matrix of \code{Z}, the vector that contains all the \code{Zj}.}
#'
#' \item{S_p}{Score vector in the saturated model.}
#'
#' \item{I_p}{Information matrix in the saturated model.}
#'
#' \item{Zast_j}{Vector with the values of the standardized variable of \code{Zj}.}
#'
#' \item{mcov}{Variance and covariance matrix for coefficient estimates.}
#'
#' \item{mcor}{Correlation matrix for coefficient estimates.}
#'
#' \item{Esm}{Data frame with estimates in the saturated model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zrj}, \code{prj} and Log-Likelihood \code{Lj_tilde}.}
#'
#' \item{Elm}{Data frame with estimates in the logistic model. It contains for each population \code{j}: the value of the explanatory variables, \code{nj}, \code{Zrj}, \code{pj}, Log-Likelihood \code{Lj}, \code{Logit_prj} and the variance of logit (\code{var.logit}).}
#'
#' \item{call}{It displays the original call that was used to fit the model lsm.}
#'
#' @encoding UTF-8
#' @details An expression of the form \code{y ~ model} is interpreted as a specification that the response \code{y} is modelled by a linear predictor specified symbolically by \code{model} (systematic component). Such a model consists of a series of terms separated by \code{+} operators. The terms themselves consist of variable and factor names separated by \code{:} operators. Such a term is interpreted as the interaction of all the variables and factors appearing in the term. Here, \code{y} is the outcome variable (binary or dichotomous: its values are 0 or 1).
#'
#' @references \code{[1]}  Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013). Applied Logistic Regression, 3rd ed., New York: Wiley.
#' @references \code{[2]} LLinás, H. J. (2006). Precisiones en la teoría de los modelos logísticos. Revista Colombiana de Estadística, 29(2), 239–265. https://revistas.unal.edu.co/index.php/estad/article/view/29310
#' @references \code{[3]} Llinás, H., & Carreño, C. (2012). The Multinomial Logistic Model for the Case in which the Response Variable Can Assume One of Three Levels and Related Models. Revista Colombiana de Estadística, 35(1), 131-138.
#' @references \code{[4]} Orozco-Acosta, E., LLinás-Solano, H., & Fonseca-Rodríguez, J. (2020). Convergence theorems in multinomial saturated and logistic models. Revista Colombiana de Estadística, 43(2), 211-231.
#' @references \code{[5]} Solano, H. L., Charris, M. A., & Hernández, J. T. (2016). El modelo de regresión logística para el caso en que la variable de respuesta puede asumir uno de tres niveles: estimaciones, pruebas de hipótesis y selección de modelos. Revista de Matemática: Teoría y Aplicaciones, 23(1), 173-197.
#'
#' @author
#' Jorge Villalba (Universidad Tecnológica de Bolívar, Cartagena-Colombia; autor y creador),
#' Humberto Llinas (Universidad del Norte, Barranquilla-Colombia; autor),
#' Jorge Borja (Universidad del Norte, Barranquilla-Colombia; autor),
#' Jorge Tilano (Universidad del Norte, Barranquilla-Colombia; autor).
#'
#' @examples
#'
#' # library(glsm)
#' # library(repmis)
#' # source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
#' # Datos <- hsbdemo
#' # modelo <- glsm(prog ~ ses + gender, data=Datos, ref = "academic")
#' # modelo
#'
#' @export
#'

glsm <- function(formula, data, ref = NaN) {
  xdata <- data
  mf <- model.frame(formula = formula, data = xdata)

  predictors <- colnames(mf)[-1]

  n_data <- as.data.frame(mf)
  if (length(unique(n_data[[1]])) < 3) {
    stop("The dependent variable must have 3 or more levels.\n\nIf you are trying to perform a dichotomous logistic regression model,\nI recommend using the lsm() function from the package of the same name.")
  }

  lvs <- levels(as.factor(n_data[[1]]))
  rw <- nrow(n_data)
  means <- list()

  for (i in lvs){
    n_data[paste0("u_", i)] <- ifelse(n_data[, 1] == i, 1, 0)
  }

  # -----------------------------------------
  #                Null model
  #------------------------------------------

  means_u <- colMeans(n_data[, grepl("^u_", names(n_data))])

  p_u <- means_u

  l <- list()

  for (i in 1:length(means_u)){
    l[[i]] <- means_u[i] * log(p_u[i])
  }

  l <- rw * sum(unlist(l))
  Log_Lik_Null <- l

  # -----------------------------------------
  #             Complete model
  #------------------------------------------

  l <- list()

  for (i in 1:length(lvs)){
    u <- n_data[, grepl("^u_", names(n_data))][i]
    l[[i]] <- ifelse(u == 0, 0, u * log(u))
  }

  l <- sum(unlist(l), na.rm = T)
  Log_Lik_Complete <- l

  # -----------------------------------------
  #             Saturated model
  #------------------------------------------

  ff <- count(data, vars = c(names(mf)[-1]))
  names(ff)[ncol(ff)] <- c("n")
  J <- nrow(ff)

  aa <- split(mf,mf[,names(mf)[1]])
  bb <- lapply(aa, function(x) count(x, vars = c(colnames(x)[-1])))

  for (i in 1:length(bb)) {
    names(bb[[i]])[ncol(bb[[i]])] <- c(paste0("z_", names(bb[i]), "_j"))
  }

  for(i in 1:length(bb)) {
    bb[[i]] <- join(bb[[i]], ff, by = names(mf)[-1])
    bb[[i]][paste0("p_", names(bb[i]), "_j")] <- bb[[i]][paste0("z_", names(bb[i]), "_j")]/bb[[i]]["n"]
  }

  tb <- as.data.frame(bb[[1]])
  tb <- tb[, c(1:(ncol(tb) - 3), ncol(tb) - 1, ncol(tb) - 2, ncol(tb))]

  for(i in bb[-1]){
    tb <- join(tb, i, by = c(names(mf)[-1], "n"), type = "full")
  }

  tb[is.na(tb)] <- 0
  nc <- length(names(mf)[-1]) + 2
  pos <- 0
  l <- numeric(length(bb))

  tb <- as.data.frame(tb)

  for (i in 1:(length(bb))) {
    tb[paste0("l_", names(bb[i]))] <- ifelse(tb[, nc + pos + 1] == 0 | tb[, nc + pos] == 0, 0, tb[, nc + pos] * log(tb[, nc + pos + 1]))
    pos <- pos + 2
  }

  tb["Lp"] <- apply(tb[, grep("^l_", names(tb))], 1, function(x) {
    if(0 %in% x){
      return(0)
    } else{
      return(sum(x))
    }
  })

  tb <- tb[, -grep("^l_", names(tb))]

  l <- sum(tb$Lp)
  Log_Lik_Saturate <- l

  Populations <- J
  Saturated_Table <- tb
  Saturated_List <- bb

  z_rj <- tb[, grep("^z_", names(tb))]
  nj <- tb[, 'n']
  p_rj_tilde <- z_rj/nj

  names(p_rj_tilde) <- gsub("^z_", "p_", names(p_rj_tilde))
  names(p_rj_tilde) <- paste0(names(p_rj_tilde), "_tilde")
  # -----------------------------------------
  #           Model parameters
  #------------------------------------------

  lvs_t <- lvs[-match(ifelse(is.na(ref), lvs[1], ref), lvs)]

  formula_str <- as.formula(paste(as.character(formula)[-1], collapse = " ~ "))
  ref_lvl <- match(ifelse(is.na(ref), lvs[1], ref), lvs)

  model <- vglm(
    formula_str,
    multinomial(refLevel = ref_lvl),
    data = data
  )

  Log_Lik_Logit <- -deviance(model)/2

  coef <- coef(model)

  for (i in seq_along(lvs_t)) {
    names(coef) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), names(coef))
  }

  coefficients <- as.numeric(coef)
  ExpB <- exp(coefficients)

  Std.Error <- sqrt(diag(vcov(model)))
  for (i in seq_along(lvs_t)) {
    names(Std.Error) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), names(Std.Error))
  }

  Wald <- (coefficients/Std.Error)^2
  DF <- rep(1, length(coef))
  P.value <- pchisq(Wald, DF, lower.tail = F)

  Dev_Null_vs_Logit <- 2 * (Log_Lik_Logit - Log_Lik_Null)
  Dev_Logit_vs_Complete <- -2 * Log_Lik_Logit
  Dev_Logit_vs_Saturate <- 2 * (Log_Lik_Saturate - Log_Lik_Logit)

  K <- length(lvs)
  Df_Null_vs_Logit <- 2 * (1 + K) - 2
  Df_Logit_vs_Complete <- 2 * (rw - (1 + K))
  Df_Logit_vs_Saturate <- 2 * (J - (1 + K))

  P.v_Null_vs_Logit <- pchisq(Dev_Null_vs_Logit, Df_Null_vs_Logit, lower.tail = F)
  P.v_Logit_vs_Complete <- pchisq(Dev_Logit_vs_Complete, Df_Logit_vs_Complete, lower.tail = F)
  P.v_Logit_vs_Saturate <- pchisq(Dev_Logit_vs_Saturate, Df_Logit_vs_Saturate, lower.tail = F)

  p_rj <- predict(model, newdata = tb[predictors], type = 'response')

  p_ref <- p_rj[, which(colnames(p_rj) %in% lvs[ref_lvl])]
  odd_p <- p_rj[, setdiff(colnames(p_rj), lvs[ref_lvl])]
  odds <- odd_p / p_ref

  logit_p <- log(odds)
  or <- exp(coef)

  colnames(p_rj) <- paste0("p_", lvs, "_j")

  ltb <- tb[predictors]

  ltb$n <- tb$n
  ltb[colnames(tb[grepl("^z_", names(tb))])] <- tb[grepl("^z_", names(tb))]
  ltb[colnames(p_rj)] <- p_rj
  ltb[colnames(logit_p)] <- logit_p

  m_rj <- ltb[, 'n'] * ltb[, grepl("^p_", names(ltb))]
  colnames(m_rj) <- paste0("m_", lvs, "_j")

  v_rj <- ltb[, grepl("^p_", names(ltb))] * (1 - ltb[, grepl("^p_", names(ltb))])
  colnames(v_rj) <- paste0("v_", lvs, "_j")
  ltb[colnames(v_rj)] <- v_rj

  V_rj <- ltb$n * v_rj

  S_p <- ((tb$n * (p_rj_tilde  - p_rj)))/v_rj
  colnames(S_p) <- paste0("S_", lvs, "(p)")

  cov_m <- vcov(model)

  for (i in seq_along(lvs_t)) {
    row.names(cov_m) = colnames(cov_m) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), row.names(cov_m))
  }

  logi <- list(
    data = n_data,
    coefficients = coefficients,
    coef = coef,
    Std.Error = Std.Error,
    ExpB = ExpB,
    Wald = as.numeric(Wald),
    DF = DF,
    P.value = as.numeric(P.value),
    Log_Lik_Complete = Log_Lik_Complete,
    Log_Lik_Null = Log_Lik_Null,
    Log_Lik_Saturate = Log_Lik_Saturate,
    Log_Lik_Logit = Log_Lik_Logit,
    Populations = Populations,
    Dev_Null_vs_Logit = Dev_Null_vs_Logit,
    Dev_Logit_vs_Complete = Dev_Logit_vs_Complete,
    Dev_Logit_vs_Saturate = Dev_Logit_vs_Saturate,
    Df_Null_vs_Logit = Df_Null_vs_Logit,
    Df_Logit_vs_Complete = Df_Logit_vs_Complete,
    Df_Logit_vs_Saturate = Df_Logit_vs_Saturate,
    P.v_Null_vs_Logit = P.v_Null_vs_Logit,
    P.v_Logit_vs_Complete = P.v_Logit_vs_Complete,
    P.v_Logit_vs_Saturate = P.v_Logit_vs_Saturate,
    Logit_r = logit_p,
    p_logit_complete = n_data[, grepl("^u_", names(n_data))],
    p_hat_null = p_u,
    p_rj = p_rj,
    odd = odds,
    OR = or,
    z_rj = z_rj,
    nj = nj,
    p_rj_tilde = p_rj_tilde,
    v_rj = v_rj,
    m_rj = m_rj,
    V_rj = V_rj,
    V = cov(z_rj),
    S_p = S_p,
    I_p = cov(S_p),
    Zast_j = scale(z_rj),
    mcov = cov_m,
    mcor = cov2cor(cov_m),
    Esm = tb,
    Elm = ltb,
    call = match.call()
  )

  class(logi) <- "glsm"

  return(logi)
}


#' @export


print.glsm <- function(x, ...) {
  TB <- cbind(x$coefficients, x$Std.Error,  x$ExpB)
  colnames(TB) <- c("Coef(B)", "Std.Error", "Exp(B)")

  cat("\nCall:\n")
  print(x$call)

  cat("\nPopulations in Saturate Model: ", x$Populations, "\n", sep = "")

  cat("\nCoefficients: \n",  sep = "")

  if(anyNA(x$coef)==TRUE){
    cat("(", sum(is.na(x$coef)), " not defined because of singularities)\n", sep = "")
  }

  print(TB, P.values=TRUE, has.Pvalue=TRUE)

  cat("\nLog Likelihood: \n")
  LL <- cbind(x$Log_Lik_Complete, x$Log_Lik_Null, x$Log_Lik_Logit, x$Log_Lik_Saturate)
  dimnames(LL) <- list("Estimation", c("Complete", "Null", "Logit", "Saturate"))
  print(t(LL))

  if(anyNA(unlist(x$data))==TRUE){
    cat("(",nrow(x$data) - nrow(na.omit(x$data)) , " observations deleted due to missingness)\n", sep = "")
  }
}
