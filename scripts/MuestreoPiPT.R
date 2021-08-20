
# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(readxl)
library(funModeling)

# Datos -------------------------------------------------------------------

rm(list = ls())
colegios <- read_xlsx("data/DatosMinedu.xlsx")
dim(colegios)
head(colegios)
tail(colegios)
df_status(colegios)


# Seleccion y estimacion -------------------------------------------------

# Fase de seleccion
attach(colegios)
N <- nrow(colegios)
m <- 200
pk <- m*alumnos/sum(alumnos)
RNGkind(sample.kind = "Rounding")
set.seed(5)
ind1 <- S.piPS(m, alumnos)
muestra1 <- colegios[ind1[,1],]
head(muestra1)
detach(colegios)

# Fase de estimacion
attach(muestra1)
pk.s1 <- pk[ind1]
est1 <- E.piPS(Profesores,pk.s1)
est1
est1[1,2]
est1[2,2]
