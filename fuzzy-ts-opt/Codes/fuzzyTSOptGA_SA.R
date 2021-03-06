getDiscourseUniverse = function(series, D1, D2, n){ 
  #series=dados$target; D1=1800; D2=1100; n=7; C=0.0001
  #series = data_test; D1 = gaParameters[1]; D2 = gaParameters[2]
  #C = gaParameters[3]; n = gaParameters[4]; w = gaParameters[5]

  ts.diff = as.vector(diff(series))
  Vmin = min(ts.diff) - as.numeric(D1)
  Vmax = max(ts.diff) + as.numeric(D2) 
  n = as.numeric(n)
  h = (Vmax - Vmin)/n 
  k = NULL; U = NULL
  k[1] = Vmin
  for (i in 2:(n + 1)) {
    k[i] <- Vmin + (i - 1) * h
    U[i-1] <- paste("u", i-1, sep="")
  }
  set.fuzzy <- data.frame(set = U, low = k[1:n], up = k[2:(n+1)])
  uim <- (1/2) * (set.fuzzy$low + set.fuzzy$up)
  uim.u <- data.frame(set.fuzzy = U, midpoint = uim)
  discourseUniverse <- data.frame(set.fuzzy = U, set.fuzzy$low, set.fuzzy$up, uim.u$midpoint)
  
  DU = NULL
  DU$diff = ts.diff
  DU$discourseUniverse = discourseUniverse
  DU$U = U
  DU$middlePoint = uim
  return(DU)
  #return(list(ts.diff, discourseUniverse, U, uim))
} 

getFuzzification = function(series, D1, D2, n, C){
  
  discourseUniverse = getDiscourseUniverse(series, D1, D2, n)
  
  #time = min(series$time):max(series$time)
  Ai = 1:length(discourseUniverse$diff)
  matrixFuzzy <- matrix(1:(length(discourseUniverse$diff)*n), ncol = n) 
  for (i in 1:(length(discourseUniverse$diff))) {
    for (j in 1:n)
      matrixFuzzy[i,j] <- 1/(1 + (C * (discourseUniverse$diff[i] - discourseUniverse$discourseUniverse[j, 4]))^2) #Gerando a matriz com os valores de associacao de cada variacao
    #rownames(matrixFuzzy) = as.numeric(c((min(series$time)+1):max(series$time))) 
    colnames(matrixFuzzy) = discourseUniverse$U
  }
  
  MF = NULL
  MF$matrixFuzzy = matrixFuzzy
  MF$middlePoint = discourseUniverse$middlePoint
  return(MF)
  #return(list(matrixFuzzy, discourseUniverse$middlePoint))
}

getRelationsMatrixOneStepAhead = function(matrixFuzzy, w, n){
  #w = 7; i = 11
  #matrixFuzzy = matrixFuzzyOneStep
  matrixFuzzy = as.data.frame(matrixFuzzy)
  tamanhoObservacoes = length(matrixFuzzy$u1)
  
  O = matrixFuzzy[(tamanhoObservacoes-w+1):(tamanhoObservacoes-1),] #matriz de operacoes
  K = matrixFuzzy[(tamanhoObservacoes),] #matriz de criterios
  R = O #matriz de relacoes nebulosas
  for (i in 1:(w-1)){ #i=1; j=1
    for (j in 1:n){
      if(K[j] < O[i,j]){
        R[i,j] = K[1, j] 
      }else{
        R[i,j] = O[i,j]
      }
    }
  }
  
  #write.csv2(table4, file = "RelationsMatrix", append = FALSE, sep = "\t", dec = ".",
  #row.names = TRUE, col.names = TRUE)
  RM = NULL
  RM$O = O
  RM$K = K
  RM$R = R
  return(RM)
  #return(list(R, K))
}

getDefuzzificationAndForecastingOneStep = function(R, K, n, uim, timeSeries, pos){
  #R = rm$R; K = rm$K; n = n; uim =  model$middlePoint; timeSeries = dados$target
  Vi = 1:n  #vetor de numeros inteiros
  Ft = NULL #K[[2]] #F(t) valor previsto para o ano t de forma difusa
  for (j in 1:n){
    Ft[j] = max(R[,j])
  }
  Vi = sum((Ft * uim)/sum(Ft)) #funcao de defuzzificacao
  #Vi <- round(Vi, 0)
  #print(Ft)
  forecast = timeSeries[pos+0] + Vi
  #forecast = timeSeries[pos+1] + Vi
  return(forecast)
}

# oneStepAheadForecasting = function(series, D1, D2, n, w, C){ 
#   #series = dados$target; D1 = 1800; D2 = 1100; n = 7; w = 7; C = 0.1
#   model = getFuzzification(series, D1, D2, n, C)
#   
#   forecast = NULL
#   for (i in (w):(length(series)-2)){#i=1
#     
#     matrixFuzzyOneStep = model$matrixFuzzy[(i-w+1):(i),]
#     
#     rm = getRelationsMatrixOneStepAhead(matrixFuzzy = matrixFuzzyOneStep, 
#                                         w = w, n = n)
#     
#     forecast[i+2] = getDefuzzificationAndForecastingOneStep(rm$R, rm$K, n = n, 
#                                                             model$middlePoint, timeSeries = series, 
#                                                             pos = i)
#   }
#   
#   #plot.ts(series, ylim = c(min(series), max(series)*1.1))
#   #lines(forecast, col = 2, lwd = 2)
#   return(forecast)
# }

oneStepAheadForecastingSA = function(series = series, parameters = parameters){ 
  #series = dados$target
  #parameters = list()
  #parameters$D1 = 10; parameters$D2 = 10; 
  #parameters$n = 5; parameters$C = 0.2
  #parameters$w = 2
  
  #D1 = 1800; D2 = 1100; n = 7; w = 7; C = 0.1
  model = getFuzzification(series = series, 
                           D1 = parameters$D1,
                           D2 = parameters$D2,
                           n = parameters$n,
                           C = parameters$C)
  
  forecast = NULL
  for (i in (parameters$w+1):(length(series)-1)){ #i=8
    
    matrixFuzzyOneStep = model$matrixFuzzy[(i-parameters$w+1):(i),]
    
    rm = getRelationsMatrixOneStepAhead(matrixFuzzy = matrixFuzzyOneStep, 
                                        w = parameters$w, n = parameters$n)
    
    forecast[i+1] = getDefuzzificationAndForecastingOneStep(rm$R, rm$K, n = parameters$n, 
                                                            model$middlePoint, timeSeries = series, 
                                                            pos = i)
  }
  
  #plot.ts(series, ylim = c(min(series), max(series)*1.1))
  #lines(forecast, col = 2, lwd = 2)
  return(forecast)
}

# GA ----
# Cria funcao fitness - GA
fitnessGA = function(D1, D2, n, w, C, series = data_train){
  #series = data_train; parameters = c(10, 10, 5, 2, 0.01, 3)
  
  fuzzyParameters = list()
  fuzzyParameters$D1 = parameters[1]
  fuzzyParameters$D2 = parameters[2]
  fuzzyParameters$n = floor(parameters[3])
  fuzzyParameters$w = floor(parameters[4])
  fuzzyParameters$C = parameters[5]
  
  forecast = oneStepAheadForecastingSA(series = series,
                                     D1 = fuzzyParameters$D1, 
                                     D2 = fuzzyParameters$D2, 
                                     n = fuzzyParameters$n, 
                                     w = fuzzyParameters$w, 
                                     C = fuzzyParameters$C)
  
  return(getMSE(forecast, data_train))
}

# Calcula os parametros - GA
getOptGAParameters = function(data_train){
  #time_series = train.set; C = 0.5; n = 5.3; w = 6
  
  procTimeBegin = proc.time()
  
  # if (length(data_train) <= 50) {
  #   nMax = 30; popMax = 30; nRun = 20
  # } else {
  #   if (length(data_train) <= 200){
  #     nMax = 25; popMax = 20; nRun = 15
  #   } else {
  #     nMax = 20; popMax = 10; nRun = 10
  #   }
  # }
  nMax = 20; popMax = 20; nRun = 20
  #amplitude = max(data_train) -min(data_train)
  D1Max = abs(min(data_train)*0.3)
  D2Max = abs(max(data_train)*0.3)
  # c() - D1, D2, C, n, w
  lower = c(0    , 0    , 0,   02, 2)
  upper = c(D1Max, D2Max, 1, nMax, max(round(length(data_train)*0.2),3))
  GA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA(x[1], x[2], x[3], x[4], x[5]),
           lower = lower, upper = upper, 
           pcrossover = 0.9,
           pmutation = 0.05,
           popSize = popMax,
           maxiter = 300,
           run = nRun,
           parallel = TRUE, 
           seed = 22)
  
  procTime = proc.time() - procTimeBegin
  plot(GA)
  result = NULL
  result$D1 = as.numeric((summary(GA)$solution[1,][1]))#, 0))
  result$D2 = as.numeric((summary(GA)$solution[1,][2]))#, 0)) 
  result$C = as.numeric(summary(GA)$solution[1,][3])
  result$n = as.numeric(round(summary(GA)$solution[1,][4],0 ))
  result$w = as.numeric(round(summary(GA)$solution[1,][5], 0))
  result$procTime = as.numeric(procTime[3])
  return(result)
}


# SA ----

fitnessSA = function(parameters){
  
  series = data_train
  
  fuzzyParameters = list()
  fuzzyParameters$D1 = parameters[1] 
  fuzzyParameters$D2 = parameters[2] 
  fuzzyParameters$C = parameters[3] 
  fuzzyParameters$n = floor(parameters[4])#integer in [1, 15]
  fuzzyParameters$w = floor(parameters[5])#integer in [1, 15]
  
  forecast = oneStepAheadForecastingSA(series = series,
                                     parameters = fuzzyParameters)
  
  return(getMSE(forecast, data_train))
}

getOptSAParameters = function(series){
  #series = data_all; C = 0.5; n = 5.3; w = 6
  
  procTimeBegin = proc.time()
  
  nMax = 50; nRun = 20
  D1Max = abs(min(data_train)*0.2)
  D2Max = abs(max(data_train)*0.2)
  
  lower = c(0    , 0    ,  0.0001,   02, 2)
  upper = c(D1Max, D2Max,       1, nMax, max(round(length(data_train)*0.2),3))
  
  SA = GenSA(lower = lower,
             upper = upper,
             fn = fitnessSA,
             control = list(max.time = 100,
                            nb.stop.improvement = 30, 
                            temperature = 10000,
                            verbose = T,
                            seed = 123)
  )
  
  procTime = proc.time() - procTimeBegin
  
  SA$par
  
  result = NULL
  result$D1 = floor(SA$par[1])
  result$D2 = floor(SA$par[2])#, 0)) 
  result$C = SA$par[3]
  result$n = floor(SA$par[4])
  result$w = floor(SA$par[5])
  result$procTime = as.numeric(procTime[3])
  return(result)
}