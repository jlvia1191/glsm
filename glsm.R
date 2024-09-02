glsm <- function(formula, data, ref = NaN, n_iter = 100000, eta = 0.001, lambda = 0.01, tol = 1e-6) {
  xdata <- data
  mf <- model.frame(formula = formula, data = xdata)
  
  predictors <- colnames(mf)[-1]
  target <- colnames(mf)[1]
  
  n_data <- as.data.frame(mf)
  if (length(unique(n_data[[target]])) < 3) {
    stop("The dependent variable must have 3 or more levels.\n\nIf you are trying to perform a dichotomous logistic regression model,\nI recommend using the lsm() function from the package of the same name.")
  }
  
  if (is.ordered(n_data[target])) {
    stop("The values of the dependent variable should not be ordered.")
  }
  
  lvs <- levels(as.factor(n_data[[target]]))
  rw <- nrow(n_data)
  means <- list()
  
  for (i in lvs){
    n_data[paste0("u_", i)] <- ifelse(n_data[, target] == i, 1, 0)
  }
  
  # -----------------------------------------
  #                Null model
  #------------------------------------------
  
  means_u <- colMeans(n_data[, grepl("^u_", names(n_data))])
  
  p_u <- means_u
  
  l <- list()
  
  for(i in 1:length(means_u)){
    l[[i]] <- means_u[i] * log(p_u[i])
  }
  
  l <- rw * sum(unlist(l))
  Log_Lik_Null <- l
  
  # -----------------------------------------
  #             Complete model
  #------------------------------------------
  
  l <- list()
  
  for(i in 1:length(lvs)){
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
  
  formula_str <- as.formula(paste("~", as.character(formula)[3], "- 1"))
  ref_lvl <- match(ifelse(is.na(ref), lvs[1], ref), lvs)
  
  # model <- vglm(
  #   formula_str, 
  #   multinomial(refLevel = ref_lvl), 
  #   data = data
  # )
  
  X <- tb[predictors]
  X <- model.matrix(formula_str)
  X <- scale(X)
  
  y <- tb[target]
  y_oh <- model.matrix(~ y - 1)
  
  model <- .calc_weights(X, y, n_iter, eta, lambda, tol, lvs[ref_lvl])
  
  probs <- model$sigs
  Log_Lik_Logit <- sum(y_oh * log(probs))
  
  coef <- model$coef
  
  coefficients <- as.numeric(coef)
  ExpB <- exp(coefficients)
  
  # Std.Error <- sqrt(diag(vcov(model)))
  # for (i in seq_along(lvs_t)) {
  #   names(Std.Error) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), names(Std.Error))
  # }
  # 
  # Wald <- (coefficients/Std.Error)^2
  # DF <- rep(1, length(coef))
  # P.value <- pchisq(Wald, DF, lower.tail = F)
  # 
  # Dev_Null_vs_Logit <- 2 * (Log_Lik_Logit - Log_Lik_Null)
  # Dev_Logit_vs_Complete <- -2 * Log_Lik_Logit
  # Dev_Logit_vs_Saturate <- 2 * (Log_Lik_Saturate - Log_Lik_Logit)
  # 
  # K <- length(lvs)
  # Df_Null_vs_Logit <- 2 * (1 + K) - 2
  # Df_Logit_vs_Complete <- 2 * (rw - (1 + K))
  # Df_Logit_vs_Saturate <- 2 * (J - (1 + K))
  # 
  # P.v_Null_vs_Logit <- pchisq(Dev_Null_vs_Logit, Df_Null_vs_Logit, lower.tail = F)
  # P.v_Logit_vs_Complete <- pchisq(Dev_Logit_vs_Complete, Df_Logit_vs_Complete, lower.tail = F)
  # P.v_Logit_vs_Saturate <- pchisq(Dev_Logit_vs_Saturate, Df_Logit_vs_Saturate, lower.tail = F)
  # 
  eta <- sapply(1:length(ws), function(c) {
    x %*% ws[[c]] + bs[[c]]
  })

  sigmoid <- function(z) {
    epsilon <- 1e-10
    return(1 / (1 + exp(-z + epsilon)))
  }

  p_rj <- t(apply(eta, 1, sigmoid))
  # 
  # p_rj <- predict(model, newdata = tb[predictors], type = 'response')
  # 
  # p_ref <- p_rj[, which(colnames(p_rj) %in% lvs[ref_lvl])]
  # odd_p <- p_rj[, setdiff(colnames(p_rj), lvs[ref_lvl])]
  # odds <- odd_p / p_ref
  # 
  # logit_p <- log(odds)
  # or <- exp(coef)
  # 
  # colnames(p_rj) <- paste0("p_", lvs, "_j")
  # 
  # ltb <- tb[predictors]
  # 
  # ltb$n <- tb$n
  # ltb[colnames(tb[grepl("^z_", names(tb))])] <- tb[grepl("^z_", names(tb))]
  # ltb[colnames(p_rj)] <- p_rj
  # ltb[colnames(logit_p)] <- logit_p
  # 
  # m_rj <- ltb[, 'n'] * ltb[, grepl("^p_", names(ltb))]
  # colnames(m_rj) <- paste0("m_", lvs, "_j")
  # 
  # v_rj <- ltb[, grepl("^p_", names(ltb))] * (1 - ltb[, grepl("^p_", names(ltb))])
  # colnames(v_rj) <- paste0("v_", lvs, "_j")
  # ltb[colnames(v_rj)] <- v_rj
  # 
  # V_rj <- ltb$n * v_rj
  # 
  # S_p <- ((tb$n * (p_rj_tilde  - p_rj)))/v_rj
  # colnames(S_p) <- paste0("S_", lvs, "(p)")
  # 
  # cov_m <- vcov(model)
  # 
  # for (i in seq_along(lvs_t)) {
  #   row.names(cov_m) = colnames(cov_m) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), row.names(cov_m))
  # }
  # 
  # logi <- list(
  #   data = n_data, 
  #   coefficients = coefficients,
  #   coef = coef,
  #   Std.Error = Std.Error,
  #   ExpB = ExpB,
  #   Wald = as.numeric(Wald),
  #   DF = DF,
  #   P.value = as.numeric(P.value),
  #   Log_Lik_Complete = Log_Lik_Complete, 
  #   Log_Lik_Null = Log_Lik_Null, 
  #   Log_Lik_Saturate = Log_Lik_Saturate,
  #   Log_Lik_Logit = Log_Lik_Logit,
  #   Populations = Populations,
  #   Dev_Null_vs_Logit = Dev_Null_vs_Logit,
  #   Dev_Logit_vs_Complete = Dev_Logit_vs_Complete,
  #   Dev_Logit_vs_Saturate = Dev_Logit_vs_Saturate,
  #   Df_Null_vs_Logit = Df_Null_vs_Logit,
  #   Df_Logit_vs_Complete = Df_Logit_vs_Complete,
  #   Df_Logit_vs_Saturate = Df_Logit_vs_Saturate,
  #   P.v_Null_vs_Logit = P.v_Null_vs_Logit,
  #   P.v_Logit_vs_Complete = P.v_Logit_vs_Complete,
  #   P.v_Logit_vs_Saturate = P.v_Logit_vs_Saturate,
  #   Logit_r = logit_p,
  #   p_logit_complete = n_data[, grepl("^u_", names(n_data))],
  #   p_hat_null = p_u,
  #   p_rj = p_rj,
  #   odd = odds,
  #   OR = or,
  #   z_rj = z_rj,
  #   nj = nj,
  #   p_rj_tilde = p_rj_tilde,
  #   v_rj = v_rj,
  #   m_rj = m_rj,
  #   V_rj = V_rj,
  #   V = cov(z_rj),
  #   S_p = S_p,
  #   I_p = cov(S_p),
  #   Zast_j = scale(z_rj),
  #   mcov = cov_m,
  #   mcor = cov2cor(cov_m),
  #   Esm = tb,
  #   Elm = ltb,
  #   call = match.call()
  # )
  # 
  # class(logi) <- "glsm"
  # 
  # return(logi)
  
  return
}

print.glsm <- function(x, ...) {
  TB <- cbind(x$coefficients, x$Std.Error,  x$ExpB)
  colnames(TB) <- c("Coef(B)", "Std.Error", "Exp(B)")
  
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nPopulations in Saturate Model: ", x$Populations, "\n", sep = "")
  
  cat("\nCoefficients: \n",  sep = "")
  
  if(anyNA(x$coef) == TRUE){
    cat("(", sum(is.na(x$coef)), " not defined because of singularities)\n", sep = "")
  }
  
  print(TB, P.values = TRUE, has.Pvalue = TRUE)
  
  cat("\nLog Likelihood: \n")
  LL <- cbind(x$Log_Lik_Complete, x$Log_Lik_Null, x$Log_Lik_Logit, x$Log_Lik_Saturate)
  dimnames(LL) <- list("Estimation", c("Complete", "Null", "Logit", "Saturate"))
  print(t(LL))
  
  if(anyNA(unlist(x$data)) == TRUE){
    cat("(",nrow(x$data) - nrow(na.omit(x$data)) , " observations deleted due to missingness)\n", sep = "")
  }
}

.calc_weights <- function(X, y, n_iter, eta, lambda, tol, ref) {
  n_class <- ncol(y)
  n_features <- ncol(X)
  n_samples <- nrow(X)
  lvs_t <- colnames(y)
  
  sigmoid <- function(z) {
    epsilon <- 1e-10
    return(1 / (1 + exp(-z + epsilon)))
  }
  
  pred <- function(X, w, b) {
    return(X %*% w + b)
  }
  
  loss <- function(y, sig, w, lambda) {
    log_loss <- -mean(y * log(sig) + (1 - y) * log(1 - sig))
    reg_term <- lambda * sum(w^2)
    return(log_loss + reg_term)
  }
  
  dldw <- function(X, y, sig) {
    return(t(X) %*% (sig - y) / n_samples)
  }
  
  dldb <- function(y, sig) {
    return(mean(sig - y))
  }
  
  update <- function(param, grad, lr) {
    return(param - lr * grad)
  }
  
  pred_prob <- function(x, ws, bs) {
    sigs <- sapply(1:length(ws), function(c) {
      sig <- sigmoid(pred(x, ws[[c]], bs[[c]]))
      return(sig)
    })
    
    return(sigs)
  }
  
  ws <- matrix(NA, nrow = n_features, ncol = n_class)
  bs <- numeric(n_class)
  
  for (c in 1:n_class) {
    w <- rnorm(n_features)
    b <- rnorm(1)
    
    for (i in 1:n_iter) {
      yhat <- pred(X, w, b)
      sig <- sigmoid(yhat)
      grad_w <- dldw(X, y[, c], sig)
      grad_b <- dldb(y[, c], sig)
      w <- update(w, grad_w, eta)
      b <- update(b, grad_b, eta)
      
      current_loss <- loss(y[, c], sig, w, lambda)
      if (current_loss < tol) break
    }
    
    ws[, c] <- w
    bs[c] <- b
  }
  
  c_n <- colnames(y)
  r_n <- colnames(X)
  
  w_s <- as.vector(ws)
  for (c in seq_along(c_n)) {
    for (i in seq_along(r_n)) {
      index <- (c - 1) * length(r_n) + i
      names(w_s)[index] <- paste0(r_n[i], ":", c_n[c])
    }
  }
  
  b_s <- unlist(bs)
  names(b_s) <- paste0('(Intercept):', c_n)
  
  coef <- c(b_s, w_s)
  names(coef) <- gsub(":y", ":", names(coef))
  coef <- coef[!grepl(ref, names(coef))]
  
  sigs_matrix <- pred_prob(X, ws, bs)
  
  return(list(ws = ws, bs = bs, coef = coef, sigs = sigs_matrix))
}


