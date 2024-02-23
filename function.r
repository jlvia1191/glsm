library(plyr)
library(repmis)
source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
Datos <- hsbdemo
attach(Datos)
names(Datos)

Datos <- Datos[c("prog", "gender", "ses")]

data <- data.frame(Y=c(1,2,2,3,1,1,1,2,2,3,3),
                   X1=c(30,30,30,30,30,69,69,69,69,69,69),
                   X2=c(20,20,20,20,20,55,55,55,55,55,55))

glsm <- function(formula, data) {
  xdata <- data
  mf <- model.frame(formula = formula, data = xdata)
  
  ff <- count(data, vars = c(names(mf)[-1]))
  names(ff)[ncol(ff)] <- c("n")
  
  aa <- split(xdata,xdata[,names(mf)[1]])
  bb <- lapply(aa, function(x) count(x, vars = c(colnames(x)[-1])))
  
  for (i in 1:length(bb)) {
    names(bb[[i]])[ncol(bb[[i]])] <- c("zj")
  }
  
  # Saturated model
  cc <- lapply(bb, function(x) {
    pj <- x$zj / ff$n
    data.frame(x, pj = pj)
  })
  
  lp <- lapply(cc, function(df) {
    l <- 0
    p <- 1
    n <- nrow(df)
    
    for (j in df$zj) {
      l <- ifelse(
        j == 0 | j == n,
        0,
        l + j * log(df$pj[p])
      )
      p <- p + 1
    }
    
    l = l + ifelse(sum(df$pj) >= 1, 0, ((n - sum(df$zj)) * log(1 - sum(df$pj))))
    return(l)
  })

  l_saturado <- sum(unlist(lp))
  
  saturado <- list(tabla = cc, poblaciones = ff, l_saturado = l_saturado)
  
  
  return(list(saturado = saturado))
}

m <- glsm(prog ~ gender + ses, data = Datos)
m
