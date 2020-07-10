rm(list=ls())

source("Codes/funcoesFTS.R")

#POPAZ, PIBVI, IGPOG
dados = read.csv("Data/POPAZ.csv", sep = ";"); tail(dados, 5)

D1 = 1800; D2 = 1100; n = 4; C = 0.0001; w = 4 #parametros

fuzzy = getFuzzification(dados, D1 = D1, D2 = D2, n = n, C = C); fuzzy

rm = getRelationsMatrix(matrixFuzzy = fuzzy$matrixFuzzy, time.series = dados$target,
                        w = w, n = n); rm

defForecast = getDefuzzificationAndForecasting(R = rm$R, K = rm$K, n = n, 
                                    uim = fuzzy$middlePoint, timeSeries = dados$target); defForecast

# analisar o numero de diferenciacoes a partir de 2
# criar grafico de previsao
# criar funcao que passa o modelo com parametro
