# Cria funcao fitness - GA
fitnessGA = function(C, n, w, time_series = train.set){
  #time_series = train.set; C = 0.7; n = 3; w = 2
  
  n = round(n, 0); w = round(w, 0)
  fuzzy1 = fuzzy.ts2(as.ts(time_series), 
                     C = C, n = n, w = w, 
                     type = "Abbasov-Mamedova", 
                     forecast = 1)  
  
  matriz.previsao = as.data.frame(matrix(nrow = length(fuzzy1$interpolate), ncol = 2))
  names(matriz.previsao) = c("obs", "forecast")
  matriz.previsao$obs = time_series
  matriz.previsao$forecast = fuzzy1$interpolate 
  matriz.previsao = na.omit(matriz.previsao)
  return(getMSE(matriz.previsao$obs, matriz.previsao$forecast))  
}

# Calcula os parametros - GA
getOptGAParameters = function(){
  #time_series = train.set; C = 0.5; n = 5.3; w = 6
  
  # c() - C, n, w, pos_type
  lower = c(0, 02, 02)
  upper = c(1, 20, 10)
  GA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3]),
           lower = lower, upper = upper, 
           pcrossover = 0.9,
           pmutation = 0.1,
           popSize = 30,
           maxiter = 30, 
           seed = 22)
  
  plot(GA)

  C = summary(GA)$solution[1,][1]; n = round(summary(GA)$solution[1,][2]) 
  w = round(summary(GA)$solution[1,][3]); pos_type = round(summary(GA)$solution[1,][4]) 
  result = c(C, n, w, pos_type)
  return(result)
}

# Calcula os parametros - GenSA
getOptGenSAParameters = function(){
  # Cria funcao fitness - GenSA
  fitnessGenSA = function(parameters, time_series = train.set){
    #time_series = train.set; C = 0.5; n = 5.3; w = 6
    
    ftsParameters = list()
    ftsParameters$C = floor(parameters[1])
    ftsParameters$n = floor(parameters[2])
    ftsParameters$w = floor(parameters[3])
    
    n = as.numeric(round(n, 0)); w = round(w, 0)
    fuzzy1 = fuzzy.ts2(as.ts(time_series), 
                       C = ftsParameters$C, n =  ftsParameters$n, w = ftsParameters$w, 
                       type = "Abbasov-Mamedova", 
                       forecast = 1)  
    
    matriz.previsao = as.data.frame(matrix(nrow = length(fuzzy1$interpolate), ncol = 2))
    names(matriz.previsao) = c("obs", "forecast")
    matriz.previsao$obs = time_series
    matriz.previsao$forecast = fuzzy1$interpolate 
    matriz.previsao = na.omit(matriz.previsao)
    return(getMSE(matriz.previsao$obs, matriz.previsao$forecast))  
  }
  # c() - C, n, w, pos_type
  lower = c(0, 02, 02)
  upper = c(1, 20, 10)
  
  GenSA <- GenSA(fn = fitnessGenSA,
                 par = c(0.5, 11, 6),
                 lower = lower, upper = upper, 
                 control = list(max.call = 4000, 
                                max.time=300, 
                                maxit = 1000, 
                                verbose = TRUE, 
                                smooth = FALSE, 
                                seed=-1, 
                                nb.stop.improvement = 40,
                                temperature = 10000))

  C = GenSA$par[1]; n = round(GenSA$par[2], 0); w = round(GenSA$par[3], 0)  
  result = c(C, n, w)
  return(result)
}

# Calcula o modelo FuzzyTS
getFuzzyTS = function(time_series, GAParameters){ #time_series = AirPassengers

  type_alg = c("Abbasov-Mamedova","NFTS")
  fuzzy1 = fuzzy.ts2(as.ts(time_series), 
                     C = GAParameters[1],
                     n = GAParameters[2],
                     w = GAParameters[3],
                     forecast = 1)
  return(fuzzy1$forecast)
}

# Calcula o valor 1-Step ahead
get1StepAheadFuzzyTS = function(train.set, test.set, GAParameters){
  
  time_series_all = train.set
  fts_forecast_all = NULL
  forecast_number = length(test.set)
  
  c=1
  for (i in 1:forecast_number){
   
    fts_forecast = getFuzzyTS(time_series_all, GAParameters) #cria modelo
    fts_forecast_all[i] = as.numeric(fts_forecast) #transforma a previs?o em valor n?m?rico
    time_series_all = c(time_series_all, test.set[i]) #atualiza o vetor utilizado no calculo da previsao 
    print(c)
    c=c+1
  
  }
  
  return(fts_forecast_all)
}


