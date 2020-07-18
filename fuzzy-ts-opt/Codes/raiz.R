rm(list=ls())

#install.packages("GA", dependencies = T)
library(GA)
#install.packages("forecast", dependencies = T)
library(forecast) #ARIMA, ETS e NNETAR

#source("Codes/funcoesFTS.R")
source("Codes/fuzzyTSOptGA_SA.R")
source("Codes/performanceMetrics.R")
source("Codes/optimalArimaETS.R")

# PIBBV, LYNX, 
# DEF, PIBPC, MATAL 
# MUC, POPAZ, IGPOG, SUNY
dados = read.csv("Data/ONI.csv", sep = ";"); tail(dados, 5)

data_train = dados$target[1:round((length(dados$target)*0.75))]
data_test = dados$target[(round((length(dados$target)*0.75))+1):length(dados$target)]

# Phase 02 - Training phase (modelling) #####
# Get optimal ARIMA and ANN models, respectively
arima_model = getOptimalARIMA(data_train)
ets_model = getOptimalETS(data_train)
nnar_model = getOptimalNNAR(data_train)

# Phase 03 - Test phase (forecasting) #####
# One-step ahead approach#
onestep_arima = getARIMAForecasts(data_test, arima_model)
onestep_ets = getETSForecasts(data_test, model = ets_model)
onestep_nnar = getNNARForecasts(data_test, model = nnar_model)


gaParameters = getOptGAParameters(data_train)
teste = oneStepAheadForecasting(time.series = data_test,
                                D1 = as.numeric(gaParameters[1]),
                                D2 = as.numeric(gaParameters[2]),
                                C = as.numeric(gaParameters[3]),
                                n = as.numeric(gaParameters[4]),
                               w = as.numeric(gaParameters[5]))

getARV(teste, data_test) #FUZZY
getARV(onestep_arima, data_test) #ARIMA
getARV(onestep_ets, data_test) #ETS
getARV(onestep_nnar, data_test) #NNETAR

getMSE(teste, data_test) #FUZZY
getMSE(onestep_arima, data_test) #ARIMA
getMSE(onestep_ets, data_test) #ETS
getMSE(onestep_nnar, data_test) #NNETAR

getMAPE(teste, data_test) #FUZZY
getMAPE(onestep_arima, data_test) #ARIMA
getMAPE(onestep_ets, data_test) #ETS
getMAPE(onestep_nnar, data_test) #NNETAR

