library(plyr)
library(repmis)
source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
Datos <- hsbdemo
attach(Datos)

glsm <- function(formula, data) {
  xdata <- data
  mf <- model.frame(formula = formula, data = xdata)
  
  n_data <- as.data.frame(mf)
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
  
  for(i in 1:length(means_u)){
    l[[i]] <- means_u[i] * log(p_u[i])
  }
  
  l <- rw * sum(unlist(l))
  dev_l <- -2 * l
  e_l <- exp(l)
  
  nulo <- list(UBarra = means_u, P_u = p_u, LogNulo = l, DevNulo = dev_l, LNulo = e_l)
  
  # -----------------------------------------
  #             Complete model
  #------------------------------------------
  
  l <- list()
  
  for(i in 1:length(lvs)){
    u <- n_data[, grepl("^u_", names(n_data))][i]
    l[[i]] <- ifelse(u == 0, 0, u * log(u))
  }
  
  l <- sum(unlist(l), na.rm = T)
  dev_l <- -2*l
  e_l <- exp(l)
  
  completo <- list(LogCompleto = l, DevCompleto = dev_l, LCompleto = e_l)
  
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
  dev_l <- -2 * l
  e_l <- exp(l)
  saturado <- list(tabla = tb, niveles = bb, poblaciones = ff, no_poblaciones = J, LogSaturado = l, DevSaturado = dev_l, LSaturado = e_l)
  
  return(list(data = n_data, ModeloCompleto = completo, ModeloNulo = nulo, ModeloSaturado = saturado))
}

m <- glsm(prog ~ gender + read, data = Datos)
