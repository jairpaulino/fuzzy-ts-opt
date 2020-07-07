source("Code/funcaoFTS.R")
library(AnalyzeTS)

dados = read.csv("Data/POPAZ.csv", sep = ";"); head(dados, 5)

#U = getFTS(dados, D1 = 500, D2 = 1000, n = 7, C = 0.3)

dif = getSerieDiff(dados, D1 = 1800, D2 = 1100, n = 7)

ud = getDiscourseUniverse(n = 7, Vmin = dif[[2]])

fuzzy = getfuzzification(time.series = dados, ts.diff = dif[[1]][,2],
                         n = 7, uim = ud$uim.u.midpoint, C = 0.0001)

length(dif[[1]])



str(dif)



