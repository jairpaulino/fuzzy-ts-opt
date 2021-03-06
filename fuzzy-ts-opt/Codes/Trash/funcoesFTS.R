# Step 1 - Differentiation and Discourse Universe
getDiscourseUniverse = function(time.series, D1, D2, n){ 
  #time.series=dados$target; D1=1800; D2=1100; n=7; C=0.0001
  #time.series = data_test; D1 = gaParameters[1]; D2 = gaParameters[2]
  #C = gaParameters[3]; n = gaParameters[4]; w = gaParameters[5]
  ts.diff <- as.vector(diff(time.series))
  Vmin <- min(ts.diff) - as.numeric(D1)
  Vmax <- max(ts.diff) + as.numeric(D2) 
  n = as.numeric(n)
  h <- (Vmax - Vmin)/n 
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

# Step 2 - Fuzzification
getFuzzification = function(time.series, D1, D2, n, C){
  
  discourseUniverse = getDiscourseUniverse(time.series, D1, D2, n)
  
  #time = min(time.series$time):max(time.series$time)
  Ai = 1:length(discourseUniverse$diff)
  matrixFuzzy <- matrix(1:(length(discourseUniverse$diff)*n), ncol = n) 
  for (i in 1:(length(discourseUniverse$diff))) {
     for (j in 1:n)
      matrixFuzzy[i,j] <- 1/(1 + (C * (discourseUniverse$diff[i] - discourseUniverse$discourseUniverse[j, 4]))^2) #Gerando a matriz com os valores de associacao de cada variacao
      #rownames(matrixFuzzy) = as.numeric(c((min(time.series$time)+1):max(time.series$time))) 
      colnames(matrixFuzzy) = discourseUniverse$U
  }
  
  MF = NULL
  MF$matrixFuzzy = matrixFuzzy
  MF$middlePoint = discourseUniverse$middlePoint
  return(MF)
  #return(list(matrixFuzzy, discourseUniverse$middlePoint))
}

# Step x - 
getRelationsMatrix = function(matrixFuzzy, time.series, w, n){
#w = 7; t = 2002
#rownames(matrixFuzzy) <- as.numeric(c(1981:2001))
#colnames(matrixFuzzy) <- c("u1","u2","u3","u4","u5","u6","u7")
#t <- 2002 #valor usado no paper para previsao; w=7; t=2002
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

#Step xx - 
getDefuzzificationAndForecasting = function(R, K, n, uim, timeSeries){
  Vi <- 1:n #vetor de numeros inteiros
  Ft <- NULL #K[[2]] #F(t) valor previsto para o ano t de forma difusa
  for (i in 1:n){
    Ft[i] <- max(R[,i])
  }
  Vi <- sum((Ft * uim)/sum(Ft)) #funcao de defuzzificacao
  #Vi <- round(Vi, 0)
  #print(Ft)
  forecast = timeSeries[length(timeSeries)] + Vi
  return(forecast)
}

getRelationsMatrixOneStepAhead = function(matrixFuzzy, w, n){
  #w = 7; i = 7
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
  Vi <- 1:n #vetor de numeros inteiros
  Ft <- NULL #K[[2]] #F(t) valor previsto para o ano t de forma difusa
  for (j in 1:n){
    Ft[j] <- max(R[,j])
  }
  Vi <- sum((Ft * uim)/sum(Ft)) #funcao de defuzzificacao
  #Vi <- round(Vi, 0)
  #print(Ft)
  forecast = timeSeries[pos+1] + Vi
  return(forecast)
}

oneStepAheadForecasting = function(time.series, D1, D2, n, w, C){ 
  #time.series = dados$target; D1 = 1800; D2 = 1100; n = 7; w = 7; C = 0.1
  model = getFuzzification(time.series, D1, D2, n, C)
  
  forecast = NULL
  for (i in w:(length(time.series)-1)){ #i=8
    
    matrixFuzzyOneStep = model$matrixFuzzy[(i-w+1):(i),]
    
    rm = getRelationsMatrixOneStepAhead(matrixFuzzy = matrixFuzzyOneStep, 
                                        w = w, n = n)
    
    forecast[i+1] = getDefuzzificationAndForecastingOneStep(rm$R, rm$K, n = n, 
                                                model$middlePoint, timeSeries = time.series, 
                                                pos = i)
  }
  
  plot.ts(time.series, ylim = c(min(time.series), max(time.series)*1.1))
  lines(forecast, col = 2, lwd = 2)
  return(forecast)
}

# GA ----
# Cria funcao fitness - GA
fitnessGA = function(D1, D2, C, n, w, time_series = data_train){
  #time_series = data_train; C = 0.01; n = 3; w = 2; D1 = 10; D2 = 20 
  
  n = round(n, 0); w = round(w, 0)
  D1 = round(D1, 0); D2 = round(D2, 0)
  C = round(C, 0)
  
  forecast = oneStepAheadForecasting(time.series = time_series,
                                  D1 = D1, D2 = D2, n = n, w = w, C = C)
  
  return(getMSE(forecast, data_train))
}

# Calcula os parametros - GA
getOptGAParameters = function(data_train){
  #time_series = train.set; C = 0.5; n = 5.3; w = 6
  
  amplitude = max(data_train) - min(data_train)
  # c() - D1, D2, C, n, w
  lower = c(0             , 0             , 0, 02, 2)
  upper = c(0.15*amplitude, 0.15*amplitude, 1, 50, round(length(data_train)*0.1, 0))
  GA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3], x[4], x[5]),
           lower = lower, upper = upper, 
           pcrossover = 0.9,
           pmutation = 0.1,
           popSize = 10,
           maxiter = 1000,
           run = 10,
           seed = 22)
  
  plot(GA)
  result = NULL
  result$D1 = as.numeric(round(summary(GA)$solution[1,][1], 0))
  result$D2 = as.numeric(round(summary(GA)$solution[1,][2], 0)) 
  result$C = as.numeric(summary(GA)$solution[1,][3])
  result$n = as.numeric(round(summary(GA)$solution[1,][4],0 ))
  result$w = as.numeric(round(summary(GA)$solution[1,][5], 0))

  return(result)
}
