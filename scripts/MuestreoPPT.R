
# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(readxl)
library(funModeling)

# Datos -------------------------------------------------------------------

colegios <- read_xlsx("data/DatosMinedu.xlsx")
dim(colegios)
head(colegios)
tail(colegios)
df_status(colegios)


# Seleccion de la muestra -------------------------------------------------

attach(colegios)
N <- dim(colegios)[1]
m <- 200
pk <- alumnos/sum(alumnos)

RNGkind(sample.kind = "Rounding")
set.seed(5)
ind1 <- S.PPS(m,alumnos)
muestra1 <- colegios[ind1[,1],]
dim(muestra1)
head(muestra1)
tail(muestra1)
pk.s <- pk[ind1[,1]]
detach(colegios)

# Estimacion de parametros ------------------------------------------------

# Estimacion del total de aulas
attach(muestra1)
est1 <- E.PPS(Aulas, pk.s)
est1
LI1 <- est1[1,2]-qnorm(1-0.04/2)*est1[2,2]
LS1 <- est1[1,2]+qnorm(1-0.04/2)*est1[2,2]
c(LI1,LS1)

# Verificacion de la estrategia de muestreo PPT
# ¿cumple con las condiciones de optimalidad?
par(mfrow=c(1,2))
plot(alumnos/Aulas)
abline(h=mean(alumnos/Aulas),col=2)
plot(alumnos/Profesores)
abline(h=mean(alumnos/Profesores),col=2)

# Efecto del disenio

vPPT <- est1[2,2]^2
vMAS1 <- N^2*(1-m/N)*var(muestra1$Aulas)/m
vMAS2 <- est1[1,1]^2*(1-m/est1[1,1])*var(muestra1$Aulas)/m
vPPT/vMAS1
vPPT/vMAS2
# Se concluye que el muestreo PPT es más eficiente
# que el muestreo aleatorio simple.

detach(muestra1)
