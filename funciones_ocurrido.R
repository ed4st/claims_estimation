# FUNCIONES PARA EVALUAR MOVIMIENTOS DE OCURRIDO


# cargar.hoja: carga los datos de la hoja de excel
#
# Parámetros:
#   -sheet : string - Hoja de la que se obtendrán los datos del ocurrido
#
# Retorno: dataframe con las columnas de trimestre de ocurrencia, trimetre 
#   contable y valor del ocurrido
#
# NOTA: Es importante seleccionar el programa del cual se obtendrán los datos, 
#   e.g., VWIB
cargar.hoja = function(sheet = 'bd'){
 
  #cargamos los datos
  # file.name = "E:/Documentos/Master/semestre4/consultoria/1-Boostrap_VWIB_2021T4.xlsm"
  file.name = file.choose()
  input <- read_excel(file.name , sheet = "bd",
                      range = cell_cols("A:G"))
  input = input[c(1,2,5,7)] # 1: TrimContable, 2: TrimInivig, 5:Programa, 7: Ocurrido
  n = dim(input)[1] #número de datos para todos los trimestres y Programas (e.g. VWIB)
  input = input[2:n,] 
  colnames(input) = c("TrimContable", "TrimIniVig","Programa","Ocurrido") #reajustamos los nombres
  
  
  
  # Dado que sólo trabajamos con el programa de VWIB (Volskwagen Insurance Broker), 
  # filtramos la información por ese programa y nos quedamos con TrimContable,
  # TrimIniVig y Ocurrido
  programa = readline(prompt=paste("INGRESE EL PROGRAMA ENTRE\n ", "(",
                                   paste(unique(input$Programa), collapse = ","), 
                                   "): ", collapse = ""))
  
  input['Ocurrido'] = as.numeric(unlist(input['Ocurrido']))
  
  input.programa = input[which(input$Programa == programa), c(1,2,4)]
  rm(input) # limpiamos memoria
  return(input.programa)
}


# generar.triangulo: genera matriz triangular con los ocurridos sin acumular
#
# Parámetros:
#   -input: dataframe con las columnas de trimestre de ocurrencia, trimetre 
#   contable y valor del ocurrido.
#   -periodo.revision: string con el periodo de revisión, e.g., "2020 T4"
#
# Retorno: triàngulo de desarrollo sin acumular (l)
generar.triangulo = function(input, periodo.revision = "2020 T2"){
  periodo.revision = "2020 T4"
  input = datos.ocurrido
  anio.revision = as.numeric(substr(periodo.revision, 1, 4))
  trim.revision = as.numeric(substr(periodo.revision, 7, 7))
  
  triangle = input %>%
    mutate(year = as.numeric(substr(TrimIniVig, 1, 4))) %>%
    filter(year %in% 2013:(anio.revision + 1)) %>%
    select(TrimIniVig, TrimContable, Ocurrido) %>%
    group_by(TrimIniVig, TrimContable) %>%
    summarise(OcurridoAcumulado = sum(Ocurrido)) %>%
    ungroup() %>%
    spread(key = TrimContable, value = OcurridoAcumulado)
  
  n.periodos = which(triangle$TrimIniVig == periodo.revision) - 1
  
  triangle = triangle %>%
    select(-TrimIniVig) %>%
    as.matrix()
  
  #acumulamos los valores debajo de la diagonal
  triangle = triangle[1:n.periodos, 1:n.periodos]
  triangle[is.na(triangle)] = 0
  for (i in 1:n.periodos) {
    triangle[i,i] = sum(triangle[i,1:i])
  }
  

  #pasamos la matriz al lado izquierdo,
  #de acuerdo a la metodología clásica
  n = dim(triangle)[1]
  l = matrix(rep(0, n*n), nrow = n)
  
  for(i in 1:n){
    l[i,1:(n-i+1)] = triangle[i,i:n]
  }
  
  return(l)
}


# acumular.datos: acumula los datos dado un triángulo de ocurrencia
#
# Parámetros:
#   -l : matriz triangular con los datos sin acumular
#
# Retorno: triàngulo de desarrollo acumulado (L)
acumular.datos = function(l){
  n = dim(l)[1]
  matriz.acumulada = l
  for (j in n:2) {
    if(j == n){
      matriz.acumulada[1:(n-j+1),j] = sum(l[1:(n-j+1),1:j])
    }
    else{
      matriz.acumulada[1:(n-j+1),j] = rowSums(l[1:(n-j+1),1:j])  
    }
  }
  return(matriz.acumulada)
}


# estimar.factores: estima los factores age-to-age 
#
# Parámetros:
#   -L : matriz triangular con los datos acumulada
#
# Retorno: vector con los factores de desarrollo para cada periodo
estimar.factores = function(L){
  # función para estimar factores age-to-age
  # L: matriz acumulada
  n = dim(L)[1]
  factores = numeric(n)
  for (k in 1:(n-1)) {
    indice.no.cero = max(which(L[,k+1] != 0))
    factores[k] = sum(L[,k+1])/sum(L[1:indice.no.cero,k])
  }
  factores[n] = 1
  return(factores)  
}


# estimar.reservas: estima las reservas IBNR, o incluyendo los ultimates para 
# cada trimestre de análisis
#
# Parámetros:
#   -L : matriz triangular con los datos acumulada
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
#   o los estimados con otros métodos como redes neuronales) 
#   -tipo: 'IBNR': estima las reservas restando los ocurridos pagados hasta el 
#           periodo (ultimates) o,
#           'ultimate': estima las reservas contando los ocurridos pagados hasta el 
#           periodo (ultimates) (como se maneja en la hoja de excel)
# Retorno: lista con las resservas para cada periodo y reserva total 
#          (reservas, reserva.total)
estimar.reservas = function(L, factores, tipo = 'IBNR'){
# tipo = 'IBNR' o 'ultimate'
  
  estimada.acumulada = L
  n = dim(L)[1]
  reservas = numeric(n) # reserva ultimate o IBNR dependiendo del tipo para cada año de accidente
  if (tipo == 'IBNR'){
    reservas[1] = 0 
    for(i in 2:n){
      reservas[i] = estimada.acumulada[i,n-i+1] *(prod(factores[(n-i+1):(n-1)]) - 1)
    }
  }
  else{
    reservas[1] = estimada.acumulada[1,n]
    for(i in 2:n){
      reservas[i] = estimada.acumulada[i,n-i+1] *(prod(factores[(n-i+1):(n-1)])) 
    }
  }
  reserva.total.estimada = sum(reservas)
  return(list(reservas = reservas, reserva.total = reserva.total.estimada))
}

# estimar.sigmas2: estima las varianzas para cada factor (Mack 1993)
#
# Parámetros:
#   -L : matriz triangular con los datos acumulada
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
# Retorno: vector con las varianzas estimadas
estimar.sigmas2 = function(L, factores){
  # estima el parámetro de varianza de los factores
  f = factores
  n = dim(L)[1]
  sigmas2 = numeric(n-1)
  
  for (k in 1:(n-2)){
    sum = 0
    for (i in 1:(n-k)){
      sum = sum + L[i,k]*(L[i,k+1]/L[i,k] - f[k])**2
    }
    sigmas2[k] = sum/(n-k-1)
  }
  sigmas2[n-1] = min(c((sigmas2[n-2]**2)/sigmas2[n-3],
                       min(c(sigmas2[n-3], sigmas2[n-2]))))  
  return(sigmas2)
}

# completar.triangulo: estima la diagonal inferior del triángulo acumulado
#
# Parámetros:
#   -L : matriz triangular con los datos acumulada
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
# Retorno: triángulo completo
completar.triangulo = function(L, factores){
  n = dim(L)[1]
  f = factores
  for (i in 1:n) {
    for (k in 1:n) {
      if(k>n+1-i){
        L[i,k] = L[i,n+1-i]*prod(f[(n+1-i):(k-1)])
      }
    }
  }
  diagonal = numeric(n)# la otra diagonal de la mariz, de la forma "/"
  for (i in 1:n) diagonal[i] = L[i,n-i+1]
  
  return(L)
}

# estimar.mses: estima los errores cuadráticos medios de las reservas para cada 
# periodo (Mack 1993)
#
# Parámetros:
#   -L.completo : matriz triangular con los datos acumulada completa
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
#   o los estimados con otros métodos como redes neuronales) 
#   -sigmas2: vector de varianzas de los factores estimadas
# Retorno: lista con los MSEs de las reservas para cada periodo
estimar.mses = function(L.completo, sigmas2, factores){
  # Estimamos el error cuadrático medio para cada reserva  
  L = L.completo
  f = factores
  n = dim(L)[1]
  mse.reservas = numeric(n)
  for (i in 1:n) {
    suma1 = 0.0
    for (k in (n+1-i):(n-1)) {
      #suma2 = 0.0
      #for (j in 1:(n-k)) {
      #  suma2 = sum(suma2,L[j,k])
      #}
      #sum = sum + (sigmas2[k]/(f[k]^2)) * ( (1/L[i,k]) + (1/sum2))
      suma1 = sum(suma1,(sigmas2[k]/(f[k]^2)) * ( (1/L[i,k]) + (1/sum(L[1:(n-k),k]))))
    }
    #print(suma1)
    mse.reservas[i] = (L[i,n]^2)*suma1
  }
  return(mse.reservas)
}



# estimar.mse.total: estima el MSE de la reserva total estimada (Mack 1993)
#
# Parámetros:
#   -L.completo : matriz triangular con los datos acumulada completa
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
#   o los estimados con otros métodos como redes neuronales) 
#   -sigmas2: vector de varianzas de los factores estimadas
#   -mse.reservas: mse de cada reserva en cada periodo
# Retorno: el MSE total de la reserva
estimar.mse.total = function(L.completo, sigmas2, factores, mse.reservas){
  L = L.completo
  f = factores
  n = dim(L)[1]
  #mse.reservas.total 
  suma1 = 0
  for (i in 2:n) {
    suma2 = ifelse(i == n , 1, sum(L[(i+1):n, n]))
    suma3 = 0
    for (k in (n-i+1):(n-1)) {
      suma3 = sum(suma3, (2*sigmas2[k]/f[k]^2)/sum(L[1:(n-k),k]))
    }
    suma1 = sum(suma1, sum(mse.reservas[i], L[i,n]*suma2*suma3) )
  }
  return(suma1)
}

# porcentaje.error: muestra el error estándar como porcentaje de la reserva
#
# Parámetros:
#   -L: matriz triangular con los datos acumulada
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
#   o los estimados con otros métodos como redes neuronales) 
porcentaje.error = function(L, factores){
  n = dim(L)[1]
  L.completo = completar.triangulo(L, factores)
  sigmas2 = estimar.sigmas2(L, factores)
  mse.reservas = estimar.mses(L.completo, sigmas2, factores)
  mse.total = estimar.mse.total(L.completo, sigmas2, factores, mse.reservas)
  R = estimar.reservas(L, factores, 'IBNR')
  sqrt(mse.reservas) / R$reservas
  porcentaje.error = data.frame(
    'periodo' = c(2:n, 'total'),
    'S.E' =round(100* c((sqrt(mse.reservas) / R$reservas)[2:n], sqrt(mse.total) / R$reserva.total))  )
  print(porcentaje.error)
}

# bootstrap.reservas: genera escenario de reservas con bootstraping
#
# Parámetros:
#   -L : matriz triangular con los datos acumulada
#   -factores: factores de desarrollo (pueden ser age-to-age, individuales promediados,
#   o los estimados con otros métodos como redes neuronales) 
#   -n.rep: cantidad de repeticiones de bootstraping (número de escenarios)
#   -tipo: 'IBNR': estima las reservas restando los ocurridos pagados hasta el 
#           periodo (ultimates) o,
#           'ultimate': estima las reservas contando los ocurridos pagados hasta el 
#           periodo (ultimates) (como se maneja en la hoja de excel)
# Retorno: n.rep reservas estimadas con bootstraping
bootstrap.reservas = function(L, factores, n.rep = 1000, tipo = 'IBNR'){
  n = dim(L)[1]
  #estimamos la matriz acumulada
  matriz.estimada = matrix(rep(0,n*n), ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n){
      if (i+j-1 == n){
        estimados.fila.i = c(L[i,j]) 
        for(col in j:1){
          estimado = estimados.fila.i[1]/factores[col-1]
          estimados.fila.i = c(estimado, estimados.fila.i)
        }
        #print(estimados.fila.i)
        matriz.estimada[i,1:j] = estimados.fila.i
      }
    }
  }
  
  #desacumulamos para estimar la matriz de datos incrementales
  #estimamos la matriz desacumulada
  matriz.desacumulada = matrix(rep(0,n*n), ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n){
      if (i+j-1 == n && j > 1){
        desacumulados.fila.i = c(L[i,j]) 
        for(col in j:2){
          matriz.desacumulada[i,col] = matriz.estimada[i,col]-matriz.estimada[i,col-1]
        }
      }
    }
  }
  matriz.desacumulada[1:n, 1] = matriz.estimada[1:n, 1] 
  
  
  #calculo de residuales
  
  #matriz de residuales:
  residuales = l - matriz.desacumulada
  
  
  #residuales al cuadrado:
  residuales2 = residuales**2
  
  plot(residuales)
  
  desviaciones = rep(0.0, n)
  #calculamos las desviaciones por columna
  for (j in 1:(n-1)) {
    g.l = n - j #grados de libertad
    desviaciones[j] = sqrt(sum(residuales2[,j])/g.l)
  }
  
  
  #residuales estandarizados
  sd.residuales = residuales
  for (j in 1:n) {
    if (j == n){
      sd.residuales[,j] = 0
    }
    else{
      sd.residuales[,j] = residuales[,j]/desviaciones[j]
    }
    
  }
  
  #Generamos lista de residuales:
  matrix2vec = c(sd.residuales)
  sd.residuales.lista = matrix2vec[matrix2vec!=0]
  plot(sd.residuales.lista)
  shapiro.test(sd.residuales.lista)
  qqnorm(sd.residuales.lista)
  qqline(sd.residuales.lista, col = 7)
  escenario.reserva = rep(0,n.rep)
  for (k in 1:n.rep){
    
    #resampleamos 
    resample.sd.residuales = sd.residuales
    residuales.no.cero = sd.residuales[sd.residuales != 0]
    
    resample.sd.residuales[sd.residuales != 0] = sample(x = residuales.no.cero,
                                                        size = n*(n+1)/2 - 2,
                                                        replace = T)
    
    
    #multiplicamos por el estimador de la desviación
    resample.residuales = resample.sd.residuales
    for (j in 1:(n-1)) {
      resample.residuales[,j] = resample.residuales[,j]*desviaciones[j]
    }
    
    l.resample = matriz.desacumulada + resample.residuales
    
    
    #reacumulamos:
    L2 = acumular.datos(l.resample)
    
    #calculamos la reserva para la muestra
    escenario.reserva[k] = estimar.reservas(L2, factores, tipo = tipo)$reserva.total
  }
  return(escenario.reserva)
}

