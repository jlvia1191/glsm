pred <- function(x, w, b) {
  return(x %*% w + b)
}

sigmoid <- function(z) {
  epsilon <- 1e-10
  return(1 / (1 + exp(-z + epsilon)))
}

loss <- function(y, sig, w, lambda) {
  log_loss <- mean(-(y * log(sig) + (1 - y) * log(1 - sig)))
  reg_term <- lambda * sum(w^2)
  return(log_loss + reg_term)
}

dldw <- function(x, y, sig) {
  gradient <- t(x) %*% (sig - y) / nrow(x)
  return(gradient)
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

X = Datos[, c('ses', 'write')]
y = Datos$prog

X <- model.matrix(~ ses + write - 1, data = X)
X <- as.data.frame(X)

X$write <- Datos$write
X <- scale(X)

y_oh <- model.matrix(~ y - 1)
n_class = ncol(y_oh)

eta = 0.001
n_iter = 100000
ws = bs = list()
lambda = 0.01
tol <- 1e-6

for (c in 1:n_class) {
  b <- rnorm(1)
  w <- rnorm(ncol(X))
  
  for (i in 1:n_iter) {
    yhat <- pred(X, w, b)
    sig <- sigmoid(yhat)
    grad_w <- dldw(X, y_oh[, c], sig)
    grad_b <- dldb(y_oh[, c], sig)
    w_new <- update(w, grad_w, eta)
    b_new <- update(b, grad_b, eta)
    
    current_loss <- loss(y_oh[, c], sig, w, lambda)
    
    if (current_loss < tol) {
      break
    }
    
    w <- w_new
    b <- b_new
  }
  
  ws[[c]] <- w
  bs[[c]] <- b
}

sigs_matrix <- pred_prob(X, ws, bs)
sigs_matrix2 <- predict(
  vglm(
    prog ~ ses + write,
    multinomial(ref = "academic"),
    data = Datos
  ),
  newdata = Datos[, c('ses', 'write')],
  type = 'response'
)

y_pred <- apply(sigs_matrix, 1, which.max)
y_pred2 <- apply(sigs_matrix2, 1, which.max)

library(caret)

y_pred <- factor(y_pred, levels = 1:n_class, labels = colnames(y_oh))
y_pred2 <- factor(y_pred2, levels = 1:n_class, labels = colnames(y_oh))
y_true <- factor(apply(y_oh, 1, which.max), levels = 1:n_class, labels = colnames(y_oh))

confusionMatrix(y_pred, y_true)
confusionMatrix(y_pred2, y_true)














# Función para calcular la matriz Hessiana
hessian <- function(X, y, ws, bs, sigs) {
  n_features <- ncol(X)
  n_class <- ncol(y)
  n_params <- (n_features + 1) * n_class
  H <- matrix(0, nrow = n_params, ncol = n_params)
  
  # Agregar término independiente a X
  Xc <- cbind(1, X)
  
  for (c in 1:n_class) {
    sig <- sigs[, c]
    diag_p <- diag(sig * (1 - sig))
    
    # Calcula la matriz Hessiana para la clase c
    Hc <- -t(Xc) %*% diag_p %*% Xc / nrow(X)
    
    # Asignar los valores en la matriz Hessiana global
    start_row <- (c - 1) * (n_features + 1) + 1
    end_row <- c * (n_features + 1)
    start_col <- (c - 1) * (n_features + 1) + 1
    end_col <- c * (n_features + 1)
    
    H[start_row:end_row, start_col:end_col] <- Hc
  }
  
  return(H)
}

# Calcular la información de Fisher
H <- hessian(X, y_oh, ws, bs, sigs_matrix)
inv_H <- solve(H)

# Matriz de varianza-covarianza
vcov_matrix <- inv_H

