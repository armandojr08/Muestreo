
# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(readxl)
library(funModeling)

# Datos -------------------------------------------------------------------

rm(list = ls())
colegio <- read_xlsx("data/DatosMinedu.xlsx")
dim(colegio)
head(colegio)
tail(colegio)
df_status(colegio)
str(colegio)

# Seleccion de la muestra -------------------------------------------------

N <- dim(colegio)[1]  # longitud de poblacion
n <- 200              # longitud de muestra
attach(colegio)
pik <- n*alumnos/sum(alumnos)
cor(pik,cbind(alumnos,Utilidad,Mensualidad))
par(mfrow=c(1,3))
plot(pik,alumnos, main="correlación alumnos")
plot(pik,Utilidad, main = "correlación utilidad")
plot(pik,Mensualidad,main = "correlación Mensualidad")

str(S.PO)
RNGkind(sample.kind = "Rounding")
set.seed(24)
ind1 <- S.PO(N, pik)
nrow(ind1)
unique(ind1)
muestra1 <- colegio[ind1,]
dim(muestra1)
head(muestra1)
detach(colegio)

# Estimacion de parametros ------------------------------------------------

attach(muestra1)
# longitud final de la muestra
n.s <- nrow(muestra1)
# probabilidades de inclusion de los elementos muestreados
pik.s <- pik[ind1]
est1 <- E.PO(Utilidad,pik.s)
# N : estimacion de la longitud de la poblacion
# Y : estimacion del total de la variable de interes
LI1 <- est1[1,2] - qnorm(1-0.04/2)*est1[2,2]
LS1 <- est1[1,2] + qnorm(1-0.04/2)*est1[2,2]
c(LI1,LS1)
# DEFF = 1.76 : el disenio de muestreo Poisson
# es menos eficiente que el muestreo aleatorio
# simple
detach(muestra1)
rm(list = ls())

# Para la estimacion de la media, se tendrá que
# dividir a est1/N. La columna N del resultado
# de esta division será inservible al igual que
# los 2 ultimos valores de la columna y.
