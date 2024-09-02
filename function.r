library(plyr)
library(repmis)
library(VGAM)
library(ggplot2)

source_data("https://github.com/hllinas/DatosPublicos/blob/main/hsbdemo.Rdata?raw=false")
Datos <- hsbdemo
attach(Datos)

source("glsm.R")
source("summary.glsm.R")
source("confint.glsm.R")
source("predict.glsm.R")
source("plot.glsm.R")

m <- glsm(prog ~ ses + write, data = Datos)

m
summary(m)

confint(m, "write:vocation")

predict(m)

plot(m, "scatter")
