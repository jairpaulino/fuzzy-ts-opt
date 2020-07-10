rm(list = ls()) # Limpa dados
  
# Bibliotecas requeridas
library(forecast)
library(AnalyzeTS) #Analisa series temporais fuzzy STF
library(GA)
library(GenSA)
library(ggplot2)
  
# Chamando funcoes externas
source("Codes/preProcessing.R")
source("Codes/fuzzyTS.R")
source("Codes/performanceMetrics.R")
source("Codes/otherModels.R")
  
# Lendo dados
# POPAZ; PIBVI; PIBPC; IGPOG
names = ("POPAZ")
dados = read.csv("Data/POPAZ.csv", sep = ";"); head(dados)

# Cria conjuntos de treinamento e teste
tamanho_dados = length(dados$target)
percentual_train = 0.75
train.set = dados$target[1:round((tamanho_dados*percentual_train))] 
test.set = dados$target[round((tamanho_dados*percentual_train+1)):tamanho_dados] 
#plot.ts(train.set); plot.ts(test.set)
  
# Calcula previsao 1-Step ahead para o modelo FTS - GA
begin_fuzzy_ga = proc.time()
GAParameters = getOptGAParameters()
fuzzy_forecast_ga = get1StepAheadFuzzyTS(train.set, test.set, GAParameters)
end_fuzzy_ga = proc.time()
tempo_proc_fuzzy_ga = end_fuzzy_ga - begin_fuzzy_ga

# Calcula previsao 1-Step ahead para o modelo FTS - GenSA
begin_fuzzy_gensa = proc.time()
GenSAParameters = getOptGenSAParameters()
fuzzy_forecast_gensa = get1StepAheadFuzzyTS(train.set, test.set, GenSAParameters)
end_fuzzy_gensa = proc.time(); 
tempo_proc_fuzzy_gensa = end_fuzzy_gensa - begin_fuzzy_gensa 
  
# Cria modelo arima e realizar previsao
begin_arima = proc.time()
arima_model = getOptimalARIMA(train.set)
arima_forecast = getARIMAForecasts(test.set, arima_model)
end_arima = proc.time(); tempo_proc_arima = end_arima - begin_arima
  
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
  
  test.set.index = dados$time[round((tamanho_dados*percentual_train+1)):tamanho_dados] 
  # Cria tabela com resultados
  result_models = as.data.frame(matrix(nrow = length(test.set), ncol = 7))
  names(result_models) = c("time", "obs", "fuzzy_sa", "fuzzy_gensa", "arima", "ets", "ann") 
  result_models$time = test.set.index
  result_models$obs = test.set
  result_models$fuzzy_sa = fuzzy_forecast_ga 
  result_models$fuzzy_gensa  = fuzzy_forecast_gensa 
  result_models$arima = arima_forecast
  result_models$ets = ets_forecast
  result_models$ann = ann_forecast
  #result_models = na.omit(result_models)
  
  result_models_metrics = as.data.frame(matrix(nrow = 3, ncol = 5))
  names(result_models_metrics) = c("fuzzy_sa", "fuzzy_gensa", "arima", "ets", "ann") 
  rownames(result_models_metrics) = c("MSE", "MAPE", "NRMSE")
  
  result_models_metrics[1, 1] = getMSE(result_models$obs, result_models$fuzzy_sa)
  result_models_metrics[2, 1] = getMAPE(result_models$obs, result_models$fuzzy_sa)
  result_models_metrics[3, 1] = getNRMSE(result_models$obs, result_models$fuzzy_sa)
  
  result_models_metrics[1, 2] = getMSE(result_models$obs, result_models$fuzzy_gensa)
  result_models_metrics[2, 2] = getMAPE(result_models$obs, result_models$fuzzy_gensa)
  result_models_metrics[3, 2] = getNRMSE(result_models$obs, result_models$fuzzy_gensa)
  
  result_models_metrics[1, 3] = getMSE(result_models$obs, result_models$arima)
  result_models_metrics[2, 3] = getMAPE(result_models$obs, result_models$arima)
  result_models_metrics[3, 3] = getNRMSE(result_models$obs, result_models$arima)
  
  result_models_metrics[1, 4] = getMSE(result_models$obs, result_models$ets)
  result_models_metrics[2, 4] = getMAPE(result_models$obs, result_models$ets)
  result_models_metrics[3, 4] = getNRMSE(result_models$obs, result_models$ets)
  
  result_models_metrics[1, 5] = getMSE(result_models$obs, result_models$ann)
  result_models_metrics[2, 5] = getMAPE(result_models$obs, result_models$ann)
  result_models_metrics[3, 5] = getNRMSE(result_models$obs, result_models$ann)
  
  options(scipen=999)
  result_models_metrics
  
  # Cria grafico 
  plot.ts(test.set, lwd = 2)
  lines(fuzzy_forecast_ga, lwd = 2, col = 2)
  lines(fuzzy_forecast_gensa, lwd = 2, col = 6)
  lines(arima_forecast, lwd = 2, col = 3)
  lines(ets_forecast, lwd = 2, col = 4)
  lines(ann_forecast, lwd = 2, col = 5)
  
  options(scipen=999)
  result_models_metrics
  
  result_models_time = as.data.frame(matrix(nrow = 1, ncol = 5))
  names(result_models_time) = c("fuzzy_ga", "fuzzy_gensa", "arima", "ets", "ann") 
  result_models_time$fuzzy_ga = tempo_proc_fuzzy_ga[3]
  result_models_time$fuzzy_gensa = tempo_proc_fuzzy_gensa[3]
  result_models_time$arima = tempo_proc_arima[3]
  result_models_time$ets = tempo_proc_ets[3]
  result_models_time$ann = tempo_proc_ann[3]
  result_models_time
  
  sink(file = paste("Results/", names[1], "_prints.txt", sep=""))
  print(paste("> **************", names[1], "**************"))
  result_models_metrics
  result_models_time
  sink()
  
  modelo = c("Valores observados", "FTS-GA", "FTS-SA", "ARIMA", "ETS", "RNA")
  cor = c(6, 5, 4, 3, 2, 1) 
  linha = c(1, 2, 3, 4, 5, 6)
  
  jpeg(filename = paste("Results/", names,".Complete_14.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
  gp = ggplot(data = result_models, aes(x = time))
  gp = gp + geom_line(aes(y = obs, color = modelo[1]), size = 1.5) 
  gp = gp + geom_line(aes(y = fuzzy_sa, color = modelo[2]), size = 2, lty = linha[3])#, color = cor[2]) #color = "FTS-GA"
  gp = gp + geom_line(aes(y = fuzzy_sa, color = modelo[2]))#, col = cor[2]) #color = "FTS-GA"
  gp = gp + geom_line(aes(y = fuzzy_gensa, color = modelo[3]), size = 1.5, lty = linha[3])#, color = cor[3]) #color = "FTS-SA"
  gp = gp + geom_line(aes(y = fuzzy_gensa, color = modelo[3]))#, color = cor[3]) #color = "FTS-GA"
  gp = gp + geom_line(aes(y = arima, color = modelo[4]), size = 1.5, lty = linha[3])#, color = cor[4]) #color = "ARIMA"
  gp = gp + geom_line(aes(y = arima, color = modelo[4]))#, color = cor[4]) #color = "FTS-GA"
  gp = gp + geom_line(aes(y = ets, color = modelo[5]), size = 1.5, lty = linha[3])#, color = cor[5]) #color = "ETS"
  gp = gp + geom_line(aes(y = ets, color = modelo[5]))#, color = cor[5]) #color = "FTS-GA"
  gp = gp + geom_line(aes(y = ann, color = modelo[6]), size = 1.5, lty = linha[3])#, color = cor[6]) #color = "RNA"
  gp = gp + geom_line(aes(y = ann, color = modelo[6]))#, color = cor[6])
  
  gp = gp + labs(x = "Data", y = paste(names)) + 
    theme_bw() + 
    theme(legend.position = "top") + 
    scale_color_manual(values = cor) +
    guides(color=guide_legend(title = NULL)) +
    theme(legend.text=element_text(size=14))
  
  gp
  dev.off()
  
  

