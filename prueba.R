library(dplyr)
library(tidyr)
library(Rcpp)
library(readxl)
library(utils)

source("/Users/steven/Documents/consultoria/funciones_ocurrido.R")


datos.ocurrido = cargar.hoja()
periodo = "2020 T4"
l = generar.triangulo(datos.ocurrido, periodo.revision = periodo)







l = matrix(c(3456, 16696,4202,6869,27000,
             13997,44253,29171,26000,0,
             42287,104456,43579,0,0,
             39824,57528,0,0,0,
             65329,0,0,0,0), nrow = 5, byrow = T)

#acumulamos
L = acumular.datos(l)



L = matrix(c(357848,1124788,1735330,2218270,2745596,3319994,3466336,3606286,3833515,3901463,
             352118,1236139,2170033,3353322,3799067,4120063,4647867,4914039,5339085,0,
             290507,1292306,2218525,3235179,3985995,4132918,4628910,4909315,0,0,
             310608,1418858,2195047,3757447,4029929,4381982,4588268,0,0,0,
             443160,1136350,2128333,2897821,3402672,3873311,0,0,0,0,
             396132,1333217,2180715,2985752,3691712,0,0,0,0,0,
             440832,1288463,2419861,3483130,0,0,0,0,0,0,
             359480,1421128,2864498,0,0,0,0,0,0,0,
             376686,1363294,0,0,0,0,0,0,0,0,
             344014,0,0,0,0,0,0,0,0,0), nrow = 10, byrow = T)



f = estimar.factores(L)
R = estimar.reservas(L, factores = f, tipo = 'ultimate')
R$reserva.total

sigmas2 = estimar.sigmas2(L, f)
L = completar.triangulo(L,f)
mse.reservas = estimar.mses(L, sigmas2, f)
sqrt(mse.reservas) / R



mse.total = estimar.mse.total(L,sigmas2, f)
sqrt(mse.total)/sum(R)

escenario.reserva = bootstrap.reservas(L, f, n.rep = 5000, tipo = 'ultimate')
par(bg = '#fffcfc')
hist(escenario.reserva/1e6, # histogram
     col="#28343c", # column color
     border="#008037",
     prob = TRUE, # show densities instead of frequencies
     xlab = "reserva (millones)",
     main = paste("Escenario de reserva periodo",periodo), breaks = 50)
lines(density(escenario.reserva/1e6), # density plot
      lwd = 2, # thickness of line
      col = "#ff3600")

shapiro.test(escenario.reserva)
qqnorm(escenario.reserva)
mu = mean(escenario.reserva)
sig = sd(escenario.reserva)
qnorm(0.75, mean = mu, sd = sig)/1e6
quantile(escenario.reserva)
