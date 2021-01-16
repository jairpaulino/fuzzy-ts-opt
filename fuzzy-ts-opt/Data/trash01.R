getOptGAParameters = function(data_train){

  nMax = 20; popMax = 20; nRun = 20
  D1Max = abs(min(data_train)*0.3)
  D2Max = abs(max(data_train)*0.3)

  lower = c(0    , 0    , 0, 05  , 2)
  upper = c(D1Max, D2Max, 1, nMax, round(length(data_train)*0.2))
  GA <- ga(type = "real-valued", 
           fitness =  function(x) -fitnessGA (x[1], x[2], x[3], x[4], x[5]),
           lower = lower, upper = upper, 
           pcrossover = 0.9,
           pmutation = 0.05,
           popSize = popMax,
           maxiter = 300,
           run = nRun,
           parallel = TRUE, 
           seed = 22)
  
  plot(GA)
  return(GA)
}