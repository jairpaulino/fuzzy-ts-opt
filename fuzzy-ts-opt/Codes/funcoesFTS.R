# Step 1 - Differentiation and Discourse Universe
getDiscourseUniverse = function(time.series, D1, D2, n){ 
  #time.series=dados; D1=1800; D2=1100; n=7; C=0.0001
  ts.diff <- as.vector(diff(time.series$target))
  Vmin <- min(ts.diff) - D1
  Vmax <- max(ts.diff) + D2 
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
  
  time = min(time.series$time):max(time.series$time)
  Ai = 1:length(discourseUniverse$diff)
  matrixFuzzy <- matrix(1:(length(discourseUniverse$diff)*n), ncol = n) 
  for (i in 1:(length(discourseUniverse$diff))) {
     for (j in 1:n)
      matrixFuzzy[i,j] <- 1/(1 + (C * (discourseUniverse$diff[i] - discourseUniverse$discourseUniverse[j, 4]))^2) #Gerando a matriz com os valores de associacao de cada variacao
      rownames(matrixFuzzy) = as.numeric(c((min(time.series$time)+1):max(time.series$time))) 
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

#Step 4 - Deffuzification of fuzzy values
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
