getOptimalARIMA = function(training_test){
  procTimeBegin = proc.time()
  arima_model = auto.arima(training_test, ic = 'bic', nmodels = 5000)
  procTime = proc.time() - procTimeBegin
  
  rtrn = NULL
  rtrn$model = arima_model
  rtrn$procTime = procTime
  return(rtrn)
}

getARIMAForecasts = function(test_set, model){
  onestep_arima = fitted(Arima(test_set, model = model))
  #plot(onestep_arima, lwd = 2)
  #lines(test_set, col = 2, lwd = 2)
  return(onestep_arima)
}

getOptimalETS = function(training_test){
  procTimeBegin = proc.time()
  ets_model = ets(training_test, ic = 'bic')
  procTime = proc.time() - procTimeBegin
  rtrn = NULL
  rtrn$model = ets_model
  rtrn$procTime = procTime  
  return(rtrn)
}

getETSForecasts = function(test_set, model){
  onestep_ets = fitted(ets(test_set, model = model))
  #plot(onestep_ets, lwd = 2)
  #lines(test_set, col = 2, lwd = 2)
  return(onestep_ets)
}

getOptimalNNAR = function(training_test){
  procTimeBegin = proc.time()
  nnar_model = nnetar(training_test)
  procTime = proc.time() - procTimeBegin
  rtrn = NULL
  rtrn$model = nnar_model
  rtrn$procTime = procTime
  return(rtrn)
}

getNNARForecasts = function(test_set, model){
  onestep_nnar = fitted(nnetar(test_set, model = model))
  #plot(onestep_nnar, lwd = 2)
  #lines(test_set, col = 2, lwd = 2)
  return(onestep_nnar)
}

