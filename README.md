# claims_estimation
Ejemplo.

## Cargando datos
Importante incluir los paquetes `dplyr`,`tidyr`,`readxl`,`utils` y el archivo con las funciones especificando su ruta:
    
    library(dplyr)
    library(tidyr)
    library(readxl)
    library(utils)

    source("/Users/steven/Documents/consultoria/funciones_ocurrido.R")



Usamos la función `cargar.datos` del archivo `funciones_ocurrido.R` (IMPORTANTE seleccionar el programa!):

    datos.ocurrido = cargar.hoja()
Seleccionamos el periodo de análisis y generamos el triángulo (`generar.triangulo`) de desarrollo hasta ese periodo:
    
    periodo = "2020 T4"
    l = generar.triangulo(datos.ocurrido, periodo.revision = periodo) # l es el triángulo incremental
Acumulamos con la función `acumular.datos`

    L = acumular.datos(l)
## Reservas
Estimamos factores de desarrollo *age-to-age*:
    
    f = estimar.factores(L)
Estimamos la reserva por periodo y la reserva total. En el archivo de excel está como tipo 'ultimate'

    R = estimar.reservas(L, factores = f, tipo = 'ultimate') # tipo = 'ultimate' o 'IBNR'
    R$reserva.total #reserva total
    R$reservas #reserva para cada periodo de desarrollo
## Escenarios con bootstraping
Generamos 5000 escenarios de bootstraping para la reserva:

    escenario.reserva = bootstrap.reservas(L, f, n.rep = 5000, tipo = 'ultimate')
Graficamos

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
          
![bootstrap1](https://github.com/ed4st/claims_estimation/blob/main/bootstrap_2020_T4.png)
