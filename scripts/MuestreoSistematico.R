
# Librerias ---------------------------------------------------------------

#rm(list = ls())
library(TeachingSampling)
library(sampling)
library(funModeling)
library(readxl)


# Datos -------------------------------------------------------------------

datos <- read_xlsx("data/DatosMinedu.xlsx")
head(datos)
tail(datos)
dim(datos)
df_status(datos)

N <- nrow(datos)    # longitud de poblacion
N
a <- 8              # salto
floor(N/a)
grupo <- as.factor(array(1:a,N))
grupo
df <- data.frame(grupo,datos)[1:15,]
df


# Seleccion de la muestra -------------------------------------------------

# En la fase de estmacion se hace uso de la 
# funcion S.SY
str(S.SY)

RNGkind(sample.kind = "Rounding")
set.seed(78)
ind1 <- S.SY(N,a)
muestra <- datos[ind1,]
dim(muestra)
head(muestra)


# Estimacion --------------------------------------------------------------

# En la fase de estimacion se hace uso de la
# función E.SY
str(E.SY)

# Estimación del numero total de profesores
attach(muestra)
colnames(muestra)
est1 <- E.SY(N,a,Profesores) # función E.SY
est1
LI1 <- est1[1,2] - qnorm(1-0.04/2)*est1[2,2]
LS1 <- est1[1,2] + qnorm(1-0.04/2)*est1[2,2]
c(LI1,LS1)

# Estimacion del ingreso total
ingreso <- (Aulas*alumnos)*Mensualidad
ingreso
datos2 <- data.frame(muestra,ingreso)
head(datos2)
est2 <- E.SY(N,a,ingreso)
est2

# Clausura de objetos
detach(muestra)
rm(list = ls())
