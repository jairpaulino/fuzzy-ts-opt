getOptimalARIMA = function(training_test){
  arima_model = auto.arima(training_test, ic = 'bic', nmodels = 5000)
  return(arima_model)
}

getARIMAForecasts = function(test_set, model){
  onestep_arima = fitted(Arima(test_set, model = model))
  return(onestep_arima)
}

getOptimalETS = function(training_test){
  ets_model = ets(training_test, ic = 'bic')
  return(ets_model)
}

getETSForecasts = function(test_set, model){
  onestep_ets = fitted(ets(test_set, model = model))
  return(onestep_ets)
}

getOptimalANN = function(training_test){
  ann_model = nnetar(training_test)
  return(ann_model)
}

getANNForecasts = function(test_set, model){
  onestep_ann = fitted(nnetar(test_set, model = model))
  return(onestep_ann)
}
