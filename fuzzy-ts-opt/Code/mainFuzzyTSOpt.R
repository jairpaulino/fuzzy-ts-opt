  rm(list = ls()) # Limpa dados
  
  # Bibliotecas requeridas
  library(forecast)
  library(AnalyzeTS) #Analisa series temporais fuzzy STF
  library(GA)
  library(GenSA)
  
  # Chamando funcoes externas
  source("Code/preProcessing.R")
  source("Code/fuzzyTS.R")
  source("Code/performanceMetrics.R")
  source("Code/otherModels.R")
  
  # Lendo dados
  # MATAL; PIBVI; POPAZ
  names = ("IPCAB")
  dados = read.csv("Data/IPCAB.csv", sep = ";"); head(dados)
  
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
  result_models = as.data.frame(matrix(nrow = length(test.set), ncol = 6))
  names(result_models) = c("obs", "fuzzy_sa", "fuzzy_gensa", "arima", "ets", "ann") 
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
