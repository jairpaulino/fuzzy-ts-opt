rm(list=ls())
#graphics.off()

#install.packages("GA", dependencies = T)
library(GA)
#install.packages("forecast", dependencies = T)
library(forecast) #ARIMA, ETS e NNETAR
library(ggplot2)
#source("Codes/funcoesFTS.R")
source("Codes/fuzzyTSOptGA_SA.R")
source("Codes/performanceMetrics.R")
source("Codes/optimalArimaETS.R")

# MATAL, POPAZ, PIBPC, IGPOG, DEFOR
# PIBBV, LYNX, SUNY, MUC, PNVEI
names = "PNVEI"
dados = read.csv(paste("Data/", names[1], ".csv", sep=""), sep = ";"); tail(dados, 5)

# Phase 01 - Preprocessing ----

data_train = dados$target[1:round((length(dados$target)*0.75))]
data_test = dados$target[(round((length(dados$target)*0.75))+1):length(dados$target)]
nnetar(AirPassengers)


# Phase 02 - Training (modelling) ----
# Get optimal ARIMA, ETS, NNAR and FTS models 
arima_model = getOptimalARIMA(data_train)
ets_model = getOptimalETS(data_train)
nnar_model = getOptimalNNAR(data_train)
gaParameters = getOptGAParameters(data_train)
write.csv(gaParameters, file = paste("Results/", names, "_gaParameters",".txt", sep=""))
nnar_model$model$p+nnar_model$model$m

# Phase 03 - Test (forecasting) ----
# One-step ahead approach#
onestep_arima = getARIMAForecasts(data_test, arima_model$model)
onestep_ets = getETSForecasts(data_test, model = ets_model$model)
#NNAR
n = nnar_model$model$p; nnarDataTest = c(data_train[(length(data_train)-n+1):length(data_train)], data_test)
onestep_nnar = getNNARForecasts(nnarDataTest, model = nnar_model$model)
onestep_nnar = as.numeric(na.omit(onestep_nnar))
#FTS-GA
w = gaParameters$w
ftsgaDataTest = c(data_train[(length(data_train)-w+1):length(data_train)], data_test)
onestep_ftsga = oneStepAheadForecasting(time.series = ftsgaDataTest,
                                D1 = as.numeric(gaParameters[1]),
                                D2 = as.numeric(gaParameters[2]),
                                C = as.numeric(gaParameters[3]),
                                n = as.numeric(gaParameters[4]),
                               w = as.numeric(gaParameters[5]))
onestep_ftsga = as.numeric(na.omit(onestep_ftsga))

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

procTime = as.data.frame(matrix(nrow = 1, ncol = 4))
colnames(procTime) = c("ARIMA", "ETS", "NNAR", "FTS_GA")
rownames(procTime) = "procTime"
procTime$ARIMA = arima_model$procTime[3]
procTime$ETS = ets_model$procTime[3]
procTime$NNAR = nnar_model$procTime[3]
procTime$FTS_GA = gaParameters$procTime
write.csv(procTime, file = paste("Results/", names, "_proctime", ".txt", sep=""))


cor = c(1, "#32CD32", "#0000FF", "#1E90FF", 2) 
linha = c(1, 2, 3, 4, 5, 6)
simbolo = c(NA, 15, 16, 17, 18, 19)
legenda = c("Observed values", "ARIMA", "ETS", "NNAR", "FTS-GA")

jpeg(filename = paste("Results/", names,"_onestep_teste.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
plot.ts(results$OBS, lwd = 2, xlab = "Index (test set)", 
        ylab = names, ylim = c(min(results)*0, max(results)*1.15))
# ARIMA
lines(results$ARIMA, lwd = 2, col = cor[2], lty = linha[2], pch = simbolo[2])
points(results$ARIMA, col = cor[2], pch = simbolo[2])
# ETS
lines(results$ETS, lwd = 2, col = cor[3], lty = linha[3], pch = simbolo[3])
points(results$ETS, col = cor[3], pch = simbolo[3])
# NNAR
lines(results$NNAR, lwd = 2, col = cor[4], lty = linha[4], pch = simbolo[4])
points(results$NNAR, col = cor[4], pch = simbolo[4])
# FTS-GA
lines(results$FTS_GA, lwd = 2, col = cor[5], lty = linha[5], pch = simbolo[5])
points(results$FTS_GA, col = cor[5], pch = simbolo[5])
# legenda
legend("topleft", legenda, col = cor, horiz = F,
       cex = 0.9, lty = linha, lwd = 2, border = T,
       bty = "o", pch = simbolo, inset = 0.01,
       bg = "white", box.col = "white")
dev.off()


#results$IND = 1:length(results[[1]])
# gp = ggplot(data = results, aes(x = IND))
# gp = gp + geom_line(aes(y = OBS, color = modelo[1]), size = 1.5) 
# gp = gp + geom_line(aes(y = ARIMA, color = modelo[4]), size = 1.5, lty = linha[3])#, color = cor[4]) #color = "ARIMA"
# gp = gp + geom_line(aes(y = ARIMA, color = modelo[4]))#, color = cor[4]) #color = "FTS-GA"
# gp = gp + geom_line(aes(y = ETS, color = modelo[5]), size = 1.5, lty = linha[3])#, color = cor[5]) #color = "ETS"
# gp = gp + geom_line(aes(y = ETS, color = modelo[5]))#, color = cor[5]) #color = "FTS-GA"
# gp = gp + geom_line(aes(y = NNAR, color = modelo[6]), size = 1.5, lty = linha[3])#, color = cor[6]) #color = "RNA"
# gp = gp + geom_line(aes(y = NNAR, color = modelo[6]))#, color = cor[6])
# gp = gp + geom_line(aes(y = FTS_GA, color = modelo[2]), size = 2, lty = linha[3])#, color = cor[2]) #color = "FTS-GA"
# gp = gp + geom_line(aes(y = FTS_GA, color = modelo[2]))#, col = cor[2]) #color = "FTS-GA"
# 
# gp = gp + labs(x = "Index (test set)", y = paste(names)) + 
#   theme_bw() + 
#   theme(legend.position = "top") + 
#   scale_color_manual(values = cor) +
#   guides(color=guide_legend(title = NULL)) +
#   theme(legend.text=element_text(size=14))
# 
# gp

