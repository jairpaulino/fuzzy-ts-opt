rm(list = ls()) # Limpa dados

# Bibliotecas requeridas
library(forecast)
library(AnalyzeTS) #Analisa series temporais fuzzy STF
library(GA)

# Chamando funcoes externas
source("Code/preProcessing.R")
source("Code/fuzzyTS.R")
source("Code/performanceMetrics.R")
source("Code/optimalArima.R")

# Lendo dados
# MATAL; PIPVI; POPAZ
dados = read.csv("Data/LYNX.csv", sep = ";"); head(dados)

# Cria conjuntos de treinamento e teste
tamanho_dados = length(dados$target)
percentual_train = 0.8
train.set = dados$target[1:round((tamanho_dados*percentual_train))] #plot.ts(train.set)
test.set = dados$target[round((tamanho_dados*percentual_train+1)):tamanho_dados] #plot.ts(test.set)

# Calcula previsao 1-Step ahed para o modelo FTS
begin_fuzzy = proc.time()
GAParameters = getOptGAParameters()
fuzzy_forecast = get1StepAheadFuzzyTS(train.set, test.set, GAParameters)
end_fuzzy = proc.time(); tempo_proc_fuzzy = end_fuzzy - begin_fuzzy

# Cria modelo arima e realizar previsao
begin_arima = proc.time()
arima_model = getOptimalARIMA(train.set)
arima_forecast = getARIMAForecasts(test.set, arima_model)
end_arima = proc.time(); tempo_proc_arima= end_arima - begin_arima

# Cria modelo ets e realizar previsao
begin_ets = proc.time()
ets_model = getOptimalETS(train.set)
ets_forecast = getETSForecasts(test.set, ets_model)
end_ets = proc.time(); tempo_proc_ets= end_ets - begin_ets

# Cria modelo ann e realizar previsao
begin_ann = proc.time()
ann_model = getOptimalANN(train.set)
ann_forecast = getANNForecasts(test.set, ann_model)
end_ann = proc.time(); tempo_proc_ann= end_ann - begin_ann; tempo_proc_ann

# Cria tabela com resultados
result_models = as.data.frame(matrix(nrow = length(test.set), ncol = 5))
names(result_models) = c("obs", "fuzzy", "arima", "ets", "ann") 
result_models$obs = test.set
result_models$fuzzy = fuzzy_forecast
result_models$arima = arima_forecast
result_models$ets = ets_forecast
result_models$ann = ann_forecast
#result_models = na.omit(result_models)

result_models_metrics = as.data.frame(matrix(nrow = 3, ncol = 4))
names(result_models_metrics) = c("fuzzy", "arima", "ets", "ann") 
rownames(result_models_metrics) = c("MSE", "MAPE", "Theils' U")

result_models_metrics[1, 1] = getMSE(result_models$obs, result_models$fuzzy)
result_models_metrics[2, 1] = getMAPE(result_models$obs, result_models$fuzzy)
result_models_metrics[3, 1] = getTheil(result_models$obs, result_models$fuzzy)

result_models_metrics[1, 2] = getMSE(result_models$obs, result_models$arima)
result_models_metrics[2, 2] = getMAPE(result_models$obs, result_models$arima)
result_models_metrics[3, 2] = getTheil(result_models$obs, result_models$arima)

result_models_metrics[1, 3] = getMSE(result_models$obs, result_models$ets)
result_models_metrics[2, 3] = getMAPE(result_models$obs, result_models$ets)
result_models_metrics[3, 3] = getTheil(result_models$obs, result_models$ets)

result_models_metrics[1, 4] = getMSE(result_models$obs, result_models$ann)
result_models_metrics[2, 4] = getMAPE(result_models$obs, result_models$ann)
result_models_metrics[3, 4] = getTheil(result_models$obs, result_models$ann)
result_models_metrics

# Cria grafico 
plot.ts(test.set, lwd = 2)
lines(fuzzy_forecast, lwd = 2, col = 2)
lines(arima_forecast, lwd = 2, col = 3)
lines(ets_forecast, lwd = 2, col = 4)
lines(ann_forecast, lwd = 2, col = 5)

options(scipen=999)
result_models_metrics

result_models_time = as.data.frame(matrix(nrow = 1, ncol = 4))
names(result_models_time) = c("fuzzy", "arima", "ets", "ann") 
result_models_time$fuzzy = tempo_proc_fuzzy[3]
result_models_time$arima = tempo_proc_arima[3]
result_models_time$ets = tempo_proc_ets[3]
result_models_time$ann = tempo_proc_ann[3]
result_models_time
