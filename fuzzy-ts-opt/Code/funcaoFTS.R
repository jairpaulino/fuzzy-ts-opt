getFTS= function(time.series, D1, D2, n, C){ 
  
  # time.series = dados; D1 = 1800; D2 = 1100; n = 7; C = 0.0001
  
  ts.diff <- as.vector(diff(ts.poaz)) #calculando a diferenca a partir ts.poaz
  Vmin <- min(ts.diff) - D1 #calculando o valor maximo e minimo do U
  Vmax <- max(ts.diff) + D2 
  h <- (Vmax - Vmin)/n 
  U <- c(Vmin, Vmax) #Universo de discurso U
  uDiff <- cbind(ts.poaz, c(NA, ts.diff)) #apresenta a serie temporal e a serie diff

  ###Step_2.Division of "U" discourse universe 
  k <- 1:(n + 1) #extremos dos conjuntos difusos
  U <- 1:n #conjuntos difusos
  k[1] <- Vmin
  for (i in 2:(n + 1)) {
    k[i] <- Vmin + (i - 1) * h
    U[i-1] <- paste("u", i-1, sep="")
  }
  set.fuzzy <- data.frame(set = U, low = k[1:n], up = k[2:(n+1)])
  uim <- (1/2) * (set.fuzzy$low + set.fuzzy$up) #c?lculo do ponto m?dio de cada conj. fuzzy uim
  uim.u <- data.frame(set.fuzzy = U, midpoint = uim)
  discourseUniverse <- data.frame(set.fuzzy = U, set.fuzzy$low, set.fuzzy$up, uim.u$midpoint)
  #print(table2)
  
  ###Step_3.Definition of fuzzy sets in the universe of U speech and Fuzzification
  time <- 1980:2001 #per?odo
  Ai <- 1:length(ts.diff) #vetor de n?meros inteiros
  MATRIX <- matrix(1:(length(ts.diff)*n), ncol = n) 
  for (i in 1:length(ts.diff)) {
    temp = ""
    for (j in 1:n) {
      At <- 1/(1 + (C * (ts.diff[i] - uim[j]))^2) #fun??o de associa??o (fuzzifica??o)
      MATRIX[i,j] <- At #Gerando a matriz com os valores de associa??o de cada varia??o
      arred.4 <- round2str(At, r = 4) #Arredondando e convertendo em sequ?ncia de n?meros para o n?mero especificado de casas decimais
      At.j <- paste("(", arred.4, "/u", j, sep = "", ")") #usada para concatenar vetores convertendo-os em car?ter.
      if (j == 1) 
        temp <- paste(temp, At.j, sep = "")
      else temp <- paste(temp, At.j, sep = ",")
    }
    Ai[i] <- paste("A[", time[i+1], "]={", temp, "}", sep = "")
  }
  fuzzification <- c(NA, Ai)
  #print(fuzzification)
  
  
  ###Step_4.Calculation of the fuzzy relations matrix R(t)

  rownames(MATRIX) <- as.numeric(c(1981:2001)) 
  colnames(MATRIX) <- c("u1","u2","u3","u4","u5","u6","u7")
  t <- 1990 #valor usado no paper para previs?o
  O <- round(MATRIX[(t-w-1980):(t-2-1980),], 4) #matriz de opera??es
  K <- round(MATRIX[(t-1-1980),], 4) #matriz de crit?rios
  R <- O
  for (i in 1:(w-1)) 
    for (j in 1:n)
      if (O[i,j] > K[j])
        R[i,j] <- K[j] #matriz de rela??es nebulosas
  
  relationMatrix <- R
  
  ###Step_5.Deffuzification of fuzzy values
  
  Vi <- 1:n #vetor de n?meros inteiros
  F <- K #F(t) valor previsto para o ano t de forma difusa
  for (j in 1:n) F[j] <- max(R[,j])
  #print(F)
  Vi <- sum((F * uim.u$midpoint)/sum(F)) #fun??o de defuzzifica??o
  defuzzicationValues <- Vi
  #print(table5)
  
  ###Step_6.Forecast
  
  forecast <- uDiff[10, 1] + Vi
  #print(forecast)
  
  return(list(uDiff, discourseUniverse, fuzzification, 
              relationMatrix, defuzzicationValues, forecast))
}
 
getDiscourseUniverse = function(time.series, D1, D2, n){ 
  
  # time.series = dados; D1 = 1800; D2 = 1100; n = 6; C = 0.0001
  
  # Step 1 - Differentiation
  ts.diff <- as.vector(diff(time.series$target)) #calculando a diferenca a partir ts.poaz
  Vmin <- min(ts.diff) - D1 #calculando o valor maximo e minimo do U
  Vmax <- max(ts.diff) + D2 
  h <- (Vmax - Vmin)/n 
  #U <- c(Vmin, Vmax) #Universo de discurso U
  #uDiff <- cbind(ts.poaz, c(NA, ts.diff)) #apresenta a serie temporal e a serie diff
  
  # Step 2 - Division of "U" discourse universe 
  #k <- 1:(n + 1) #extremos dos conjuntos difusos
  #U <- 1:n #conjuntos difusos
  k = NULL; U = NULL
  k[1] = Vmin
  for (i in 2:(n + 1)) {
    k[i] <- Vmin + (i - 1) * h
    U[i-1] <- paste("u", i-1, sep="")
  }
  
  set.fuzzy <- data.frame(set = U, low = k[1:n], up = k[2:(n+1)])
  uim <- (1/2) * (set.fuzzy$low + set.fuzzy$up) #c?lculo do ponto m?dio de cada conj. fuzzy uim
  uim.u <- data.frame(set.fuzzy = U, midpoint = uim)
  discourseUniverse <- data.frame(set.fuzzy = U, set.fuzzy$low, set.fuzzy$up, uim.u$midpoint)
  #print(table2)
  
  return(list(ts.diff, discourseUniverse, U))
} 

getfuzzification = function(time.series, D1, D2, n, C){
  #ts.diff, n, C, uim
  
  discourseUniverse = getDiscourseUniverse(time.series, D1, D2, n)
  
  ###Step_3.Definition of fuzzy sets in the universe of U speech and Fuzzification
  time = min(time.series$time):max(time.series$time)
  
  Ai = 1:length(discourseUniverse[[1]]) #vetor de numeros inteiros
  matrizFuzzy <- matrix(1:(length(discourseUniverse[[1]])*n), ncol = n) 
  for (i in 1:(length(discourseUniverse[[1]]))) {
    #temp = ""
    for (j in 1:n) {
      #At <- 1/(1 + (C * (ts.diff[i] - uim[j]))^2) #funcao de associacao (fuzzificacao)
      matrizFuzzy[i,j] <- 1/(1 + (C * (discourseUniverse[[1]][i] - discourseUniverse[[2]][j, 4]))^2) #Gerando a matriz com os valores de associacao de cada variacao
      #arred.4 <- round2str(At, r = 4) #Arredondando e convertendo em sequencia de numeros para o n?mero especificado de casas decimais
      #At.j <- paste("(", arred.4, "/u", j, sep = "", ")") #usada para concatenar vetores convertendo-os em car?ter.
      #if (j == 1) 
      #  temp <- paste(temp, At.j, sep = "")
      #else temp <- paste(temp, At.j, sep = ",")
    }
    #Ai[i] <- paste("A[", time[i+1], "]={", temp, "}", sep = "")
  }
  #fuzzification <- c(NA, Ai)
  #print(fuzzification)
 
  rownames(matrizFuzzy) = as.numeric(c((min(time.series$time)+1):max(time.series$time))) 
  colnames(matrizFuzzy) = discourseUniverse[[3]]

  return(matrizFuzzy)
}

# relationsMatrix = function(time.series, matrizFuzzy, w){
# 
#   ###Step_4.Calculation of the fuzzy relations matrix R(t)
#   w=7
#   t <- 2002 #valor usado no paper para previs?o
#   O <- round(MATRIX[(t-w-1980):(t-2-1980),], 4) #matriz de opera??es
#   K <- round(MATRIX[(t-1-1980),], 4) #matriz de crit?rios
#   R <- O
#   for (i in 1:(w-1))
#     for (j in 1:n)
#       if (O[i,j] > K[j])
#         R[i,j] <- K[j] #matriz de rela??es nebulosas
# 
#   relationMatrix <- R
# }



