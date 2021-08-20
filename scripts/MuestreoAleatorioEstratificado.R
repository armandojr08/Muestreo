

# Librerias ---------------------------------------------------------------

library(TeachingSampling)
library(sampling)


# Datos -------------------------------------------------------------------

data("Lucy")
names(Lucy)
table(Lucy$Level)
tapply(Lucy$Income, Lucy$Level, mean)
boxplot(Lucy$Income ~ Lucy$Level)
# Los estratos deben marcar una diferencia
# se debe elegir un buen criterio de estratificacion
# muestra piloto
N <- nrow(Lucy)
np <- 50
RNGkind(sample.kind = "Rounding")
set.seed(25)
mpiloto <- S.SI(N,np)
mue_pil <- Lucy[mpiloto,]
tapply(mue_pil$Income, mue_pil$Level, mean)
boxplot(mue_pil$Income ~ mue_pil$Level)


# Muestreo aleatorio estratificado ----------------------------------------

N <- nrow(Lucy) 
Nh <- table(Lucy$Level) 
n <- 400 # muestra

# Asignacion proporcional 
f <- n/N
nh <- round(f*Nh) 

# Fase de seleccion
RNGkind(sample.kind = "Rounding")
set.seed(56) 
sam <- S.STSI(Lucy$Level,Nh,nh) # selecciona los indices
# el 1er argumento es la variable de estratificacion
# en la poblacion
str(S.STSI)
muestra <- Lucy[sam,] 
table(muestra$Level) == nh # cumple con la reparticion 
head(muestra)
tail(muestra)

# Fase de estimacion
res1<-E.STSI(muestra$Level,Nh,nh,muestra$Income) 
# el 1er argumento de E.STSI es la variable de estratificacion
# en la muestra; el 4to, la variable de interes en la muestra
# la funcion res1 devuelve 2 listas
# ademas devuelve, en la 2da lista, las estimaciones de los
# totales para cada estrato

# Intervalo de confianza para la media
res2 <- res1/N
LI1<-res2[1,4,2]-qnorm(0.975)*res2[2,4,2] 
LS1<-res2[1,4,2]+qnorm(0.975)*res2[2,4,2] 
c(LI1,LS1)

# comprobacion de la estimacion del total: 
# lo que devuelve E.STSI() vs codgo construido
toh <- (Nh/nh)*tapply(muestra$Income,muestra$Level,sum)
to <- sum(toh)
eetoh <- sqrt((Nh^2/nh)*(1-nh/Nh)*tapply(muestra$Income,muestra$Level,var))
eeto<-sqrt(sum((Nh^2/nh)*(1-nh/Nh)*tapply(muestra$Income,muestra$Level,var)))

# Dominios en un MAE
addmargins(table(muestra$Level, muestra$SPAM))
dominios <- Domains(muestra$SPAM) # tipo variable indicadora
SPAM.si <- dominios[,2]*muestra$Income
# Estimacion del total de empresas que envian correo SPAM
res3 <- E.STSI(muestra$Level, Nh, nh, dominios)
res3
table(Lucy$SPAM) # verdadero valor poblacional
LI <- res3[1,4,3]-qnorm(0.975)*res3[2,4,3] 
LS <- res3[1,4,3]+qnorm(0.975)*res3[2,4,2] 
c(floor(LI),ceiling(LS))

# Estimacion del ingreso total de empresas que envian correo SPAM
res4 <- E.STSI(muestra$Level, Nh, nh, SPAM.si)
res4
LI <- res4[1,4,2]-qnorm(0.975)*res4[2,4,2] 
LS <- res4[1,4,2]+qnorm(0.975)*res4[2,4,2] 
# Intervalo de confianza para el total
c(LI,LS)
# Intervalo de confianza para la media
c(LI,LS)/res3[1,4,3]

# Muestreo con reemplazo

N <- nrow(Lucy)
m <- 400
f <- m/N
Nh <- table(Lucy$Level)
mh <- f*Nh # reparticion de los estratos

# Fase de seleccion 
RNGkind(sample.kind="Rounding") 
set.seed(31) 
ind2 <- S.STPPS(Lucy$Level,Lucy$Taxes,mh) 
head(ind2)
ind3 <- ind2[,1] 
muestra2 <- Lucy[ind3,] 
pk.s <- ind2[,2] # probs de seleccion

# Fase de Estimacion 
est6<-E.STPPS(muestra2$Income,pk.s,mh,muestra2$Level)
est6

# Numero total de empresas 
LI <- est6[1,4,1]-qnorm(0.975)*est6[2,4,1] 
LS <- est6[1,4,1]+qnorm(0.975)*est6[2,4,1] 
c(floor(LI),ceiling(LS)) 

# Total de ingreso 
LI5<-est6[1,4,2]-qnorm(0.975)*est6[2,4,2] 
LS5<-est6[1,4,2]+qnorm(0.975)*est6[2,4,2] 
c(LI5,LS5) 

# Muestreo Estratificado piPT 

# Fase de seleccion
RNGkind(sample.kind="Rounding") 
set.seed(32) 
res6<-S.STpiPS(Lucy$Level,Lucy$Taxes,nh) 
sam3<-res6[,1] 
muestra3<-Lucy[sam3,] 
pik.s<-res6[,2] 
#Fase de EstimaciÃ³n 
res7<-E.STpiPS(muestra3$Income,pik.s,muestra3$Level) 

#Número total de empresas 
LI6<-res7[1,4,1]-qnorm(0.975)*res7[2,4,1] 
LS6<-res7[1,4,1]+qnorm(0.975)*res7[2,4,1] 
c(floor(LI6),ceiling(LS6)) 
#Total de ingreso 
LI7<-res7[1,4,2]-qnorm(0.975)*res7[2,4,2] 
LS7<-res7[1,4,2]+qnorm(0.975)*res7[2,4,2] 
c(LI7,LS7)