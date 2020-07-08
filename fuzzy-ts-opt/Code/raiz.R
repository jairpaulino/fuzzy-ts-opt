rm(list=ls())

source("Code/funcaoFTS.R")

dados = read.csv("Data/PIBPC.csv", sep = ";"); head(dados, 5)

fuzzy = getfuzzification(dados, D1 = 1800, D2 = 1100, n = 7, C = 0.0001)
#View(fuzzy)



