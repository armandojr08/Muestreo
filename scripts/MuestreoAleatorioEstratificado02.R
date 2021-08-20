
# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(readxl)
library(funModeling)

# Datos -------------------------------------------------------------------

agua <- read_xlsx("data/DatosMuestreoEstratificado01.xlsx")
head(agua)
colnames(agua)
df_status(agua)
str(agua)

# Seleccion y estimacion --------------------------------------------------

attach(agua)
Nh <- table(Ciudad)
N <- dim(agua)[1]
n <- 200
f <- n/N
nh <- round(f*Nh)

# Fase de seleccion
RNGkind(sample.kind = "Rounding")
set.seed(10)
ind <- S.STSI(Ciudad,Nh,nh)
muestra <- agua[ind,]
table(muestra$Ciudad) # coincide con nh
head(muestra)
detach(agua)

# dividiendo, la muestra, en grupos
colnames(agua)
c1 <- subset(muestra, Ciudad == "1")
c2 <- subset(muestra, Ciudad == "2")
c3 <- subset(muestra, Ciudad == "3")

# Estimacion del gasto promedio
attach(muestra)
Est <- list(E.STSI(Ciudad,Nh,nh,Gasto))
Est1 <- E.STSI(Ciudad,Nh,nh,Gasto)/N
res <- E.STSI(Ciudad,Nh,nh,Gasto)/N
str(res)
res1 <- res[, ,2]
LI <- res1[1,4]-qnorm(1-0.10/2)*res1[2,4]
LS <- res1[1,4]+qnorm(1-0.10/2)*res1[2,4]
c(LI,LS)

# Estimacion del gasto total en clientes
# con tiempo de antiguedad mayor a 5
Dominios <- Domains(Antigüedad)
Dominios
Anti.si <- Dominios[,2]*Gasto
res2 <- E.STSI(Ciudad,Nh,nh,Anti.si)
LI <- res2[1,4,2]-qnorm(1-0.10/2)*res2[2,4,2]
LS <- res2[1,4,2]+qnorm(1-0.10/2)*res2[2,4,2]
c(LI,LS)

# Estimacion de la edad promedio
Est2 <- list(E.STSI(Ciudad,Nh,nh,Edad))
res3 <- E.STSI(Ciudad,Nh,nh,Edad)/N
res3
LI3 <- res3[1,4,2]-qnorm(1-0.10/2)*res3[2,4,2]
LS3 <- res3[1,4,2]+qnorm(1-0.10/2)*res3[2,4,2]
c(LI3,LS3)

# Estimacion de la edad del cliente cuya
# antiguedad es mayor a 5 anios
Dominios <- Domains(Antigüedad)
Anti.si <- Dominios[,2]*Edad
est4 <- E.STSI(Ciudad,Nh,nh,Anti.si)
LI4 <- est4[1,4,2]-qnorm(1-0.10/2)*est4[2,4,2]
LS4 <- est4[1,4,2]+qnorm(1-0.10/2)*est4[2,4,2]
c(LI4,LS4)


# Estimacion del total total de clientes cuyo
# tamanio de botella más solicitada es de 500ml
Dominios2 <- Domains(muestra$Botella)
Dominios2
res5 <- E.STSI(muestra$Ciudad,Nh,nh,Dominios2)
res5        
LI5 <- res5[,,3][1,4]-qnorm(1-0.10/2)*res5[,,3][2,4]
LS5 <- res5[,,3][1,4]+qnorm(1-0.10/2)*res5[,,3][2,4]
c(LI5,LS5)
c(floor(LI5),ceiling(LS5))

