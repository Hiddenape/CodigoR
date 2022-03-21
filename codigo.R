
## ---------------------------------------
## Programa principal de optimizacion
## de portafolios 
## --------------------------------------

# Cargar las librerias o paquetes
library(quantmod)
library(xts)
library(zoo)
library(quadprog)

# Definir la ruta de trabajo
rm(list=ls())
getwd()
setwd("/Users/oscarreyes/Desktop/EXTERNADO/SCRIPTS/codigoprincipal")

source("codigos/precios.R")
source("codigos/modeloMV.R")
source("codigos/treynor.R")
source("codigos/performance.R")

# Info
fecha1 <- "2016-11-30"
fecha2 <- "2022-01-31"
periodicidad <- "monthly" 
activos <- c("MSFT","T","VZ","PFE","WFC","CLF","HSY","COP","TGT")

precios <- f.precios(activos,fecha1,fecha2,periodicidad)
tclass(precios) <- "Date"
retornos <- diff(log(precios))[-1,]

# Parametros 
rf <- 0
mu <- colMeans(retornos)
cov <- cov(retornos)
var <- diag(cov)
sigma <- sqrt(var)
short <- 0 #1: con cortos; 0: sin cortos
n <- length(mu)

# Modelo MV
MV <- modeloMV(retornos)
# FE
wpo <- MV[[1]]
rpo <- MV[[2]]
sigmapo <- MV[[3]]
# PMVG
wpmvg <- MV[[4]]
rpmvg <- MV[[5]]
sigmapmvg <- MV[[6]]

# PM: Portafolio de Sharpe
wpm <- MV[[7]]
rpm <- MV[[8]]
sigmapm <- MV[[9]]

# Treynor
p.indice <- f.precios(activos = "^GSPC",fecha1,fecha2,periodicidad)
retornos_indice <- diff(log(p.indice))[-1,]

MT <- m.treynor(retornos,retornos_indice)

wtr <- MT[[1]]
rtr <- MT[[2]]
sigmatr <- MT[[3]]

quartz() #windows()
par(mfrow=c(2,2))
barplot(t(wpmvg),main="PMVG")
barplot(t(wpm),main="Sharpe")
barplot(t(wtr),main="Treynor")

quartz() #windows()
plot(sigma,mu,main="Plano Riesgo-Retorno",xlim=c(0,max(sigma)*1.1),ylim=c(min(mu),max(mu)*1.2),
     xlab="Riesgo",ylab="Retorno esperado")
lines(sigmapo,rpo,type="l",col="blue")
text(sigma,mu,labels=activos,cex=0.6,pos=4)
points(sigmapm,rpm,pch=19,col="red")
text(sigmapm,rpm,labels = "M",pos=4)
points(sigmatr,rtr,pch=19,col="red")
text(sigmatr,rtr,labels = "T",pos=4)
points(sigmapmvg,rpmvg,pch=19,col="red")
text(sigmapmvg,rpmvg,labels = "PMVG",pos=2)

# Exportar mu

write.xlsx(retornos, file = "retornos.xlsx", rowNames = FALSE)

# Sortino
library(PerformanceAnalytics)
M_Sortino <- t(SortinoRatio(retornos, prices = NULL, rf = 0))

M_Sortino_ordered <- M_Sortino[order(-M_Sortino[,1]),]

M_Sortino_ordered
