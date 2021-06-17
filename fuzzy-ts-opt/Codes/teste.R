source("Codes/fuzzyTSOptGA_SA.R")

series = dados$target; #View(dados)

#obj = 2000, pos = 21
model = getFuzzification(series = series, 
                         D1 = 1800,
                         D2 = 1100,
                         n = 7,
                         C = 0.0001)

#round(model$matrixFuzzy, 2)  

forecast = NULL
parameters = NULL
parameters$w = 7
parameters$n = 7

for (i in (parameters$w+1):(length(series)-1)){ 
  #i=10
  #series[i]; dados[i,]
  matrixFuzzyOneStep = model$matrixFuzzy[(i-parameters$w+0):(i-1),]
  #round(matrixFuzzyOneStep, 2)
  rm = getRelationsMatrixOneStepAhead(matrixFuzzy = round(matrixFuzzyOneStep,2), 
                                      w = parameters$w, n = parameters$n)
  forecast[i+1] = getDefuzzificationAndForecastingOneStep(round(rm$R,2),  
                                                          round(rm$K,2), 
                                                          n = parameters$n, 
                                                          model$middlePoint, 
                                                          timeSeries = series[1:i],
                                                          pos = i
                                                          )
}

a = cbind(dados, forecast = round(forecast/1000,1))
View(a)

length(forecast)

plot.ts(series, lwd = 2, ylim = c((min(series)), max(series*1.01)))
lines(forecast, col = 2, lwd = 2)

#getDefuzzificationAndForecastingOneStep = function(R, K, n, uim, timeSeries, pos){
  R = rm$R; K = rm$K; n = 7; uim = model$middlePoint; timeSeries = series
  pos = i
  Vi = 1:n  #vetor de numeros inteiros
  Ft = NULL #K[[2]] #F(t) valor previsto para o ano t de forma difusa
  for (j in 1:n){#j=1
    Ft[j] = max(R[,j])
  }
  round(Ft, 2)
#Vi = (sum(round(Ft,2) * round(uim))/sum(round(Ft,2))) #funcao de defuzzificacao
 Vi = sum((Ft*uim)/sum(Ft)) #funcao de defuzzificacao
 Vi <- round(Vi, 0)
  #print(Ft)
  forecast = timeSeries[pos+0] + Vi
  length(series)
#  return(forecast)
#}