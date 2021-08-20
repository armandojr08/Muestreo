
# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(sampling)
library(xlsx)
library(data.table)
library(dplyr)
library(tictoc)

# Datos -------------------------------------------------------------------

datos <- read.xlsx("data/DatosMuestreoEstratificado01.xlsx", sheetName = "Hoja1")
colnames(datos)
str(datos)
head(datos)
dim(datos)
datos <- datos %>% rename(Antiguedad = AntigÃ.edad)
str(datos)

# Variables :
# - Ciudad: Ciudad de ubicación del cliente (1: La Libertad, 2: Lima, 3: Cajamarca)  
# - Edad: Edad (en años) del cliente.  
# - Gasto: Gasto (en soles) en pedidos mensual.  
# - Botella: Tamaño de la botella más solicitada (0: 250 ml, 1: 500 ml, 2: 750ml)  
# - Tiendas: Número de tiendas del cliente  
# - Antiguedad: Tiempo de antigüedad en el negocio (0: ≤ 5 años, 1: >5 años). 


# Muestreo aleatorio estratificado ----------------------------------------

# variable de estratificacion -> ciudad
table(datos$Ciudad)
colnames(datos)
with(datos, tapply(Gasto, Ciudad, mean))
tapply(datos$Gasto, datos$Ciudad, mean)
boxplot(datos$Gasto ~ datos$Ciudad)
boxplot(datos$Gasto ~ datos$Tiendas)

# seleccion de la muestra
N <- dim(datos)[1]
Nh <- table(datos$Ciudad)
n <- 200
f <- n/N
nh <- round(f*Nh)
RNGkind(sample.kind = "Rounding")
set.seed(10)
ind <- S.STSI(datos$Ciudad, Nh, nh)
muestra <- datos[ind,]
head(muestra)
muestra <- muestra %>% rename(Antiguedad)

# subgrupos de la muestra
attach(muestra)
colnames(muestra)
# ciudad 1, ciudad 2 y ciudad 3
c1 <- muestra %>% filter(Ciudad == "1")
c2 <- muestra %>% filter(Ciudad == "2")
c3 <- muestra %>% filter(Ciudad == "3")

# Estimacion del gasto total
est1 <- E.STSI(muestra$Ciudad, Nh, nh, muestra$Gasto)

# Estimacion del gasto promedio e IC al 90%
est2 <- est1/N
z <- qnorm(1-0.1/2)
li2 <- est2[1,4,2] - z*est2[2,4,2] 
ls2 <- est2[1,4,2] + z*est2[2,4,2] 
c(li2, ls2)

# Estimacion del gasto total de clientes
# con antiguedad mayor a 5 años
table(muestra$Antiguedad)
dominios <- Domains(muestra$Antiguedad)
antig.si <- dominios[,2]*muestra$Gasto
est3 <- E.STSI(muestra$Ciudad, Nh, nh, antig.si)
z <- qnorm(1-0.1/2)
li3 <- est3[1,4,2] - z*est3[2,4,2] 
ls3 <- est3[1,4,2] + z*est3[2,4,2] 
c(li3, ls3)

# Estimacion de la edad promedio e IC al 90%
est4 <- E.STSI(muestra$Ciudad, Nh, nh, muestra$Edad)/N
est4[1,4,2]
z <- qnorm(1-0.1/2)
li4 <- est4[1,4,2] - z*est4[2,4,2] 
ls4 <- est4[1,4,2] + z*est4[2,4,2] 
c(li4, ls4)

# Estimacion de la edad promedio e IC al 90%
# de los clientes con antiguedad mayor a 5
dominios <- Domains(muestra$Antiguedad)
Antig.Mayor.A5 <- dominios[,2]*muestra$Edad
est5 <- E.STSI(muestra$Ciudad, Nh, nh, Antig.Mayor.A5)
est5[1,4,2]
z <- qnorm(1-0.1/2)
li5 <- est5[1,4,2] - z*est5[2,4,2] 
ls5 <- est5[1,4,2] + z*est5[2,4,2] 
c(li5, ls5)

E.STSI(muestra$Ciudad, Nh, nh, dominios)

# Diseño PPT

Nh <- table(datos$Ciudad)
N <- nrow(datos)
m <- 100
f <- m/N
mh <- round(f*Nh)
mh;sum(mh)

RNGkind(sample.kind = "Rounding")
set.seed(11)
res <- S.STPPS(datos$Ciudad, datos$Tiendas,mh)
ind2 <- res[,1]
muestra2 <- datos[ind2,]
pk <- res[,2]; sum(pk)

