rm(list=ls())
graphics.off()

#install.packages("GA", dependencies = T)
library(GA)
#install.packages("forecast", dependencies = T)
library(forecast) #ARIMA, ETS e NNETAR
library(ggplot2)
#source("Codes/funcoesFTS.R")
source("Codes/fuzzyTSOptGA_SA.R")
source("Codes/performanceMetrics.R")
source("Codes/optimalArimaETS.R")

# Phase 01 - Preprocessing ----

# PIBBV, LYNX, IGPOG
# DEF, PIBPC, MATAL 
# MUC, POPAZ, SUNY
names = "PNVEIC"
dados = read.csv(paste("Data/", names, ".csv", sep=""), sep = ";"); tail(dados, 5)

data_train = dados$target[1:round((length(dados$target)*0.75))]
data_test = dados$target[(round((length(dados$target)*0.75))+1):length(dados$target)]

# Phase 02 - Training (modelling) ----
# Get optimal ARIMA, ETS, ARNN and FTS models, respectively
arima_model = getOptimalARIMA(data_train)
ets_model = getOptimalETS(data_train)
nnar_model = getOptimalNNAR(data_train)
gaParameters = getOptGAParameters(data_train)

# Phase 03 - Test (forecasting) ----
# One-step ahead approach#
onestep_arima = getARIMAForecasts(data_test, arima_model)
onestep_ets = getETSForecasts(data_test, model = ets_model)
onestep_nnar = getNNARForecasts(data_test, model = nnar_model)
onestep_ftsga = oneStepAheadForecasting(time.series = data_test,
                                D1 = as.numeric(gaParameters[1]),
                                D2 = as.numeric(gaParameters[2]),
                                C = as.numeric(gaParameters[3]),
                                n = as.numeric(gaParameters[4]),
                               w = as.numeric(gaParameters[5]))

results = data.frame(matrix(nrow = length(data_test), ncol = 5))
names(results) = c("OBS", "ARIMA", "ETS", "NNAR", "FTS_GA")
results$OBS = data_test
results$ARIMA = onestep_arima
results$ETS = onestep_ets
results$NNAR = onestep_nnar
results$FTS_GA = onestep_ftsga
write.csv(results, file = paste("Results/",names, "_onestep", ".txt", sep=""))

# Phase 04 - Metrics and plots #####
metrics = calculateMetrics(results)
write.csv(metrics, file = paste("Results/", names, "_metrics",".txt", sep=""))

cor = c(6, 5, 4, 3, 2, 1) 
linha = c(1, 2, 3, 4, 5, 6)

results$IND = 1:length(results[[1]])


modelo = c("Time series", "FTS-GA", "FTS-SA", "ARIMA", "ETS", "RNA")
cor = c(5, 4, 3, 2, 1) 
linha = c(1, 2, 3, 4, 5, 6)

jpeg(filename = paste("Results/", names,"_onestep_teste.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
gp = ggplot(data = results, aes(x = IND))
gp = gp + geom_line(aes(y = OBS, color = modelo[1]), size = 1.5) 
gp = gp + geom_line(aes(y = FTS_GA, color = modelo[2]), size = 2, lty = linha[3])#, color = cor[2]) #color = "FTS-GA"
gp = gp + geom_line(aes(y = FTS_GA, color = modelo[2]))#, col = cor[2]) #color = "FTS-GA"
gp = gp + geom_line(aes(y = ARIMA, color = modelo[4]), size = 1.5, lty = linha[3])#, color = cor[4]) #color = "ARIMA"
gp = gp + geom_line(aes(y = ARIMA, color = modelo[4]))#, color = cor[4]) #color = "FTS-GA"
gp = gp + geom_line(aes(y = ETS, color = modelo[5]), size = 1.5, lty = linha[3])#, color = cor[5]) #color = "ETS"
gp = gp + geom_line(aes(y = ETS, color = modelo[5]))#, color = cor[5]) #color = "FTS-GA"
gp = gp + geom_line(aes(y = NNAR, color = modelo[6]), size = 1.5, lty = linha[3])#, color = cor[6]) #color = "RNA"
gp = gp + geom_line(aes(y = NNAR, color = modelo[6]))#, color = cor[6])

gp = gp + labs(x = "Index (test set)", y = paste(names)) + 
  theme_bw() + 
  theme(legend.position = "top") + 
  scale_color_manual(values = cor) +
  guides(color=guide_legend(title = NULL)) +
  theme(legend.text=element_text(size=14))

gp
dev.off()

