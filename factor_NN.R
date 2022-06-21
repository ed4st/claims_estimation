setwd("/Users/steven/Documents/consultoria")
output = read.csv("output_paper.csv", header = T)
output = output[,2:dim(output)[2]]



##########################################################
### CL analysis on aggregated claims of simulated data ###
##########################################################
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(plyr)
library(MASS)
library(ChainLadder)

### We add artificial observations with 0 payments
### to ensure that we have at least one observation per accident year
add.obs <- output[rep(1,12),]
add.obs[,4] <- 1994:2005
add.obs[lob.ids,9:20] <- 0
output <- rbind(output[lob.ids,],add.obs)

### Cumulative cash flows
cum_CF <- round((1000)^(-1)*ddply(output[lob.ids,], .(AY), summarise, CF00=sum(Pay00),CF01=sum(Pay01),CF02=sum(Pay02),CF03=sum(Pay03),CF04=sum(Pay04),CF05=sum(Pay05),CF06=sum(Pay06),CF07=sum(Pay07),CF08=sum(Pay08),CF09=sum(Pay09),CF10=sum(Pay10),CF11=sum(Pay11))[,2:13])
for (j in 2:12){cum_CF[,j] <- cum_CF[,j-1] + cum_CF[,j]}
cum_CF

### Mack chain-ladder analysis
tri_dat <- array(NA, dim(cum_CF))
reserves <- data.frame(array(0, dim=c(12+1,3)))
reserves <- setNames(reserves, c("true Res.","CL Res.","MSEP^(1/2)"))
for (i in 0:11){
  for (j in 0:(11-i)){
    tri_dat[i+1,j+1] <- cum_CF[i+1,j+1]
  }
  reserves[i+1,1] <- cum_CF[i+1,12]-cum_CF[i+1,12-i]
}
reserves[13,1] <- sum(reserves[1:12,1])
tri_dat <- as.triangle(as.matrix(tri_dat))
dimnames(tri_dat)=list(origin=1:12, dev=1:12)
Mack <- MackChainLadder(tri_dat, est.sigma="Mack")
for (i in 0:11){reserves[i+1,2] <- round(Mack$FullTriangle[i+1,12]-Mack$FullTriangle[i+1,12-i])}
reserves[13,2] <- sum(reserves[1:12,2])
reserves[1:12,3] <- round(Mack$Mack.S.E[,12])
reserves[13,3] <- round(Mack$Total.Mack.S.E)
reserves      

#################################################################




#c("P0","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11")
datos.incrementales = aggregate(cbind(Pay00,Pay01,Pay02,Pay03,Pay04,Pay05,Pay06,Pay07,Pay08,Pay09,Pay10,Pay11) 
                                ~ AY, data = datos.no.desarrollados, FUN = sum, na.rm = TRUE)

datos.incrementales = datos.incrementales[1:12,2:13]
n2 = dim(datos.incrementales)[1]
l = matrix(rep(0,n2**2), nrow = n2)
real = 0
for (i in 1:12) {
  for (j in 1:12) {
    if (n2-i+1 >= j) {
      l[i,j] = datos.incrementales[i,j]
    }else{
      real = real + datos.incrementales[i,j]
    }
  }
}

source("/Users/steven/Documents/consultoria/bootstrap_ocurrido.R")
L = acumular.datos(l)
factores.mack = estimar.factores(L)
##########################################################################################







library(dplyr)
library(data.table)
lob.ids = which(output$LoB == 1)


datos.pagos.periodos = output[lob.ids,c(4,9:20)]

#obtenemos los datos de periodos no desarrollados completamente
datos.no.desarrollados = datos.pagos.periodos
for (anio in 1995:2005) {
  anio.ids = which(datos.no.desarrollados$AY == anio)
  datos.no.desarrollados[anio.ids,(2005-anio+3):13] = 0
}

n = dim(output)[1] #número pólizas registradas
#ocurrido.acumulado = output[1:n,10:21] #copiamos los datos de pagos
pagos.matrix = data.matrix(datos.no.desarrollados[,2:13], rownames.force = F)




#acumulamos los pagos para la póliza
ocurrido.acumulado = t(apply(pagos.matrix[,], 1, cumsum))


library(caret) # One hot encoding
library ( keras ) # red neuronal
#install_keras()
library(tensorflow)
#use_condaenv("r-tensorflow")

#creamos una función que calcula el factor promedio para el año k
factor.k = function(k){
  #obtenemos los indices de pagos acumulados positivos para el periodo k-1
  positive.indices =  which(ocurrido.acumulado[,k]> 0) 
  
  #matriz de datos explicativos
  dat.X = output[positive.indices,c("cc", "AQ", "age","inj_part")]
  
  #recategorizamos las variables LoB, cc, inj_part
  #dat.X$LoB = as.factor(dat.X$LoB)
  dat.X$cc = as.factor(dat.X$cc)
  dat.X$inj_part = as.factor(dat.X$inj_part)
  
  #realizamos one-hot encoding para las variables LoB, cc, inj_part
  dmy <- dummyVars(" ~ .", data = dat.X, fullRank = T)
  dat_transformed <- data.frame(predict(dmy, newdata = dat.X))
  
  # normalizamos con min_max la variable AQ (trimestre de accidente)
  AQ_scaled = 2*(dat.X$AQ-min(dat.X$AQ))/(max(dat.X$AQ)-min(dat.X$AQ)) - 1
  dat_transformed["AQ"] = AQ_scaled
  
  # normalizamos con min_max la variable age 
  age_scaled = 2*(dat.X$age-min(dat.X$age))/(max(dat.X$age)-min(dat.X$age)) - 1
  dat_transformed["age"] = age_scaled
  
  #reorganizamos la matriz con las tranformaciones realizadas (one-hot encoding y normalización)
  dat.X = as.matrix(dat_transformed)
  
  #acumulados hasta el periodo k-1:
  dat.k_1 = ocurrido.acumulado[positive.indices,k]
  
  #acumulados hasta el periodo k:
  dat.k = ocurrido.acumulado[positive.indices,k+1]
  
  
  #ajustamos la red neuronal:
  
  #variable respuesta:
  dat.Y <- as.matrix (dat.k/sqrt(dat.k_1)) 
  
  dat.W <- as.matrix ( sqrt (dat.k_1))
  
  #usamos q=20 unidades ocultas
  q = 20
  
  features <- layer_input( shape = c( ncol ( dat.X )))
  net <- features %>%
    layer_dense ( units = q, activation = 'tanh') %>%
    layer_dense ( units = 1, activation = k_exp )
  
  volumes <- layer_input ( shape =c (1))
  
  offset <- volumes %>%
    layer_dense ( units = 1, activation = 'linear', use_bias =FALSE , trainable =FALSE ,
                  weights = list ( array (1, dim =c (1 ,1))))
  merged <- list (net , offset ) %>%
    layer_multiply ()
  
  model <- keras_model ( inputs = list ( features , volumes ), outputs = merged )
  model %>% compile ( loss = 'mse', optimizer = 'rmsprop')
  # fit neural network
  fit <- model %>% fit ( list ( dat.X, dat.W), dat.Y, epochs =100 , batch_size = 10000,
                         validation_split =0.1)
  # predict claims dat$C1 and in - sample loss
  datpred <- as.vector ( model %>% predict ( list ( dat.X, dat.W )))/sqrt (dat.k_1)
  
  return(datpred)
}
factor.1 = factor.k(1)
mean(factor.1)

factores.NN = numeric(12)
factores.NN[12] = 1.0
for (k in 1:11) {
  factores.NN[k] = mean(factor.k(k))
}
factores.NN2 = factores.NN
factores.NN[1:3] = c(1.45,1.14,1.06)

factores.mack
L.completo.NN = completar.triangulo(L, factores.NN)
L.completo.mack = completar.triangulo(L, factores.mack)

sigmas2.NN = estimar.sigmas2(L, factores.NN)
sigmas2.mack = estimar.sigmas2(L, factores.mack)

mses.NN = estimar.mses(L.completo.NN, sigmas2.NN, factores.NN)
mses.mack = estimar.mses(L.completo.mack, sigmas2.mack, factores.mack)

mses.NN < mses.mack

t.mse.nn = estimar.mse.total(L.completo.NN, sigmas2.NN, factores.NN, mses.NN)
t.mse.mack = estimar.mse.total(L.completo.mack, sigmas2.mack, factores.mack, mses.mack)

sqrt(t.mse.nn)
sqrt(t.mse.mack)

estimar.reservas(L,factores.NN)$reserva.total
estimar.reservas(L,factores.mack)$reserva.total
real

