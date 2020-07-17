rm(list=ls())

source("Codes/funcoesFTS.R")
source("Codes/performanceMetrics.R")

#POPAZ, PIBVI, IGPOG, ONI
dados = read.csv("Data/DEF2.csv", sep = ";"); tail(dados, 5)

D1 = 1800; D2 = 1100; n = 4; C = 0.0001; w = 4 #parametros

fuzzy = getFuzzification(dados$target, D1 = D1, D2 = D2, n = n, C = C); fuzzy

rm = getRelationsMatrix(matrixFuzzy = fuzzy$matrixFuzzy, time.series = dados$target,
                        w = w, n = n); rm

defForecast = getDefuzzificationAndForecasting(R = rm$R, K = rm$K, n = n, 
                                    uim = fuzzy$middlePoint, timeSeries = dados$target); defForecast
#IGPOG
#time.series = dados$target; D1 = 1800; D2 = 1100; n = 7; w = 7; C = 0.0001
#time.series = dados$target; D1 = 1800; D2 = 1100; n = 15; w = 5; C = 0.1
#time.series = dados$target; D1 = 1800; D2 = 1100; n = 15; w = 5; C = 0.01
#time.series = dados$target; D1 = min(dados$target*0.80); D2 = max(dados$target*1.2); n = 5; w = 10; C = 0.001
time.series = dados$target; D1 = 0; D2 = 0; n = 30; w = 2; C = 0.00001

TESTE = oneStepAheadForecasting(time.series = time.series,
                           D1 = D1, D2 = D2, n = n, w = w, C = C)

getMAPE(TESTE, time.series)
getARV(TESTE, time.series)



# calcular delta:
#  D1 = D2 = 30% - Amplitude 

#0.008
# analisar o numero de diferenciacoes a partir de 2
# criar grafico de previsao
# criar funcao que passa o modelo com parametro
