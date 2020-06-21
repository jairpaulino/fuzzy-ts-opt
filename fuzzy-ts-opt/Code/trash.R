modelo = c("Valores observados", "FTS-GA", "FTS-SA", "ARIMA", "ETS", "RNA")
cor = c(6, 5, 4, 3, 2, 1) 
linha = c(1, 2, 3, 4, 5, 6)

#jpeg(filename = paste("Results/", names,".Complete.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
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
  theme(legend.position = "top") + 
  scale_color_manual(values = cor) +
  guides(color=guide_legend("Modelos")) 
gp
#dev.off()

#PARA A SÉRIE PIBBV
vetor_3 = ts(dados$target, start = c(1997, 1), frequency = 4)
plot(vetor_3)
time(vetor_3)
dados$time = time(vetor_3)