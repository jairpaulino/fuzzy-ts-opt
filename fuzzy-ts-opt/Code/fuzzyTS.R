# Cria funcao fitness
fitnessGA = function(C, n, w, pos_type, time_series = train.set){
  #time_series = train.set; C = 0.7; n = 17.1; w = 5.3; pos_type = 1.2
  
  C = round(C, 0); n = round(n, 0)
  w = round(w, 0); pos_type = round(pos_type, 0)
  type_alg = c("Abbasov-Mamedova","NFTS")
  fuzzy1 = fuzzy.ts2(as.ts(time_series), 
                     C = C, n = n, w = w, type = type_alg[pos_type], 
                     forecast = 1)  
  
  matriz.previsao = as.data.frame(matrix(nrow = length(fuzzy1$interpolate), ncol = 2))
  names(matriz.previsao) = c("obs", "forecast")
  matriz.previsao$obs = time_series
  matriz.previsao$forecast = fuzzy1$interpolate 
  matriz.previsao = na.omit(matriz.previsao)
  return(getMSE(matriz.previsao$obs, matriz.previsao$forecast))  
}

getOptGAParameters = function(){

  # c() - C, n, w, pos_type
  lower = c(0, 05, 02, 1)
  upper = c(1, 20, 10, 2)
  GA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3], x[4]),
           lower = lower, upper = upper, 
           pcrossover = 0.9,
           pmutation = 0.05,
           popSize = 10,
           maxiter = 10, 
           seed = 156)
  
  plot(GA)

  C = summary(GA)$solution[1,][1]; n = round(summary(GA)$solution[1,][2]) 
  w = round(summary(GA)$solution[1,][3]); pos_type = round(summary(GA)$solution[1,][4]) 
  result = c(C, n, w, pos_type)
  return(result)
}

# Calcula o modelo FuzzyTS
getFuzzyTS = function(time_series, GAParameters){ #time_series = AirPassengers

  type_alg = c("Abbasov-Mamedova","NFTS")
  fuzzy1 = fuzzy.ts2(as.ts(time_series), 
                     C = GAParameters[1],
                     n = GAParameters[2],
                     w = GAParameters[3],  
                     type = type_alg[GAParameters[4]],
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
