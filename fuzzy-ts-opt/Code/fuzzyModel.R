rm(list = ls()) # Limpa dados

poaz = read.csv2('Data/POPAZ.csv', sep=";") 
ts.poaz =  as.ts(poaz$target)
#ts.poaz = ts(poaz$target, start = 1980, end = 2001)
#autoplot(ts.poaz, xlab="Time", ylab="ts.poaz", main = "Time series to population Azerbaij?o")

D1 <- 1800 #s?o n?meros positivos usados no ajuste dos limites do universo de discurso U
D2 <- 1100
n <- 7 #n?mero de conjuntos fuzzy
C <- 0.0001 #constante escolhida de forma a garantir a convers?o de valores quantitativos em difusos 
w <- 7 #n?mero inteiro positivo

###Step_1.Definition of the universe of discourse "U"
getDiscourseUniverse = function(time.series, D1, D2, n){
  ts.diff <- as.vector(diff(ts.poaz)) #calculando a diferen?a a partir ts.poaz
  Vmin <- min(ts.diff) - D1 #calculando o valor m?ximo e m?nimo do U
  Vmax <- max(ts.diff) + D2
  h <- (Vmax - Vmin)/n 
  U <- c(Vmin, Vmax) #Universo de discurso U
  uTable <- cbind(ts.poaz, c(NA, ts.diff)) #apresenta a serie temporal e a serie diff
  #print(uTable)
  return(uTable)
}

#a = getDiscourseUniverse(time.series = ts.poaz, D1 = 1800, D2 = 1100, n = 7)
