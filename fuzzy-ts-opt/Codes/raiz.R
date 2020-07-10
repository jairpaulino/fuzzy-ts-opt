rm(list=ls())

source("Codes/funcoesFTS.R")

#POPAZ, PIBVI
dados = read.csv("Data/POPAZ.csv", sep = ";"); tail(dados, 5)

D1 = 1800; D2 = 1100; n = 7; C = 0.0001; w = 7

fuzzy = getfuzzification(dados, D1 = D1, D2 = D2, n = n, C =C)
fuzzy

rm = getRelationsMatrix(matrizFuzzy = fuzzy$matrizFuzzy, time.series = dados$target,
                        w = w, n = n) 
rm

def = getDefuzzificationAndForecasting(R = rm$R, K = rm$K, n = n, 
                                    uim = fuzzy$middlePoint, timeSeries = dados)
def
