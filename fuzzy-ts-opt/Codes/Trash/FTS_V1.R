#fts.am <- function(ts, n, w, D1, D2, forecast, plot)

#To clean Workspace
rm(list=ls())

library(forecast)
library(AnalyzeTS)

setwd('C:/Users/Eduardo/OneDrive - Instituto Leao Sampaio de Ensino Universitario Ltda/Artigos_2020/Code_R/POAZ') #Importando o conjunto de dados
poaz <- read.csv2('poaz1.csv', sep=";") 
ts.poaz = ts(poaz$target, start = 1980, end = 2001)
autoplot(ts.poaz, xlab="Time", ylab="ts.poaz", main = "Time series to population Azerbaij?o")

#Parameters of the Abbasov-Mamedova model (D1, D2, n, C e w) defined according to the paper

D1 <- 1800 #s?o n?meros positivos usados no ajuste dos limites do universo de discurso U
D2 <- 1100
n <- 7 #n?mero de conjuntos fuzzy
C <- 0.0001 #constante escolhida de forma a garantir a convers?o de valores quantitativos em difusos 
w <- 7 #n?mero inteiro positivo

###Step_1.Definition of the universe of discourse "U"

ts.diff <- as.vector(diff(ts.poaz)) #calculando a diferen?a a partir ts.poaz
Vmin <- min(ts.diff) - D1 #calculando o valor m?ximo e m?nimo do U
Vmax <- max(ts.diff) + D2
h <- (Vmax - Vmin)/n 
U <- c(Vmin, Vmax) #Universo de discurso U
table1 <- data.frame(time = c(1980:2001), ts.poaz = ts.poaz, 
                     ts.diff = c(NA, ts.diff)) #apresenta a serie temporal e a serie diff
print(table1)

###Step_2.Division of "U" discourse universe 

k <- 1:(n + 1) #extremos dos conjuntos difusos
U <- 1:n #conjuntos difusos
k[1] <- Vmin
for (i in 2:(n + 1)) {
  k[i] <- Vmin + (i - 1) * h
    U[i-1] <- paste("u", i-1, sep="")
}
set.fuzzy <- data.frame(set = U, low = k[1:n], up = k[2:(n+1)])
uim <- (1/2) * (set.fuzzy$low + set.fuzzy$up) #c?lculo do ponto m?dio de cada conj. fuzzy uim
uim.u <- data.frame(set.fuzzy = U, midpoint = uim)
table2 <- data.frame(set.fuzzy = U, set.fuzzy$low, set.fuzzy$up, uim.u$midpoint)
print(table2)

###Step_3.Definition of fuzzy sets in the universe of U speech and Fuzzification

time <- 1980:2001 #per?odo
Ai <- 1:length(ts.diff) #vetor de n?meros inteiros
MATRIX <- matrix(1:(length(ts.diff)*n), ncol = n) 
for (i in 1:length(ts.diff)) {
  temp = ""
  for (j in 1:n) {
    At <- 1/(1 + (C * (ts.diff[i] - uim[j]))^2) #fun??o de associa??o (fuzzifica??o)
      MATRIX[i,j] <- At #Gerando a matriz com os valores de associa??o de cada varia??o
        arred.4 <- round2str(At, r = 4) #Arredondando e convertendo em sequ?ncia de n?meros para o n?mero especificado de casas decimais
          At.j <- paste("(", arred.4, "/u", j, sep = "", ")") #usada para concatenar vetores convertendo-os em car?ter.
    if (j == 1) 
      temp <- paste(temp, At.j, sep = "")
    else temp <- paste(temp, At.j, sep = ",")
  }
  Ai[i] <- paste("A[", time[i+1], "]={", temp, "}", sep = "")
}
table3 <- c(NA, Ai)
print(table3)

###Step_4.Calculation of the fuzzy relations matrix R(t)

MATRIX
rownames(MATRIX) <- as.numeric(c(1981:2001)) 
colnames(MATRIX) <- c("u1","u2","u3","u4","u5","u6","u7")
t <- 1990 #valor usado no paper para previs?o
O <- round(MATRIX[(t-w-1980):(t-2-1980),], 4) #matriz de opera??es
K <- round(MATRIX[(t-1-1980),], 4) #matriz de crit?rios
R <- O
for (i in 1:(w-1)) 
  for (j in 1:n)
    if (O[i,j] > K[j])
      R[i,j] <- K[j] #matriz de rela??es nebulosas
table4 <- R
print(table4)

###Step_5.Deffuzification of fuzzy values

Vi <- 1:n #vetor de n?meros inteiros
F <- K #F(t) valor previsto para o ano t de forma difusa
for (j in 1:n) F[j] <- max(R[,j])
  print(F)
Vi <- sum((F * uim.u$midpoint)/sum(F)) #fun??o de defuzzifica??o
table5 <- Vi
print(table5)

###Step_6.Forecast

forecast <- table1$ts.poaz[10] + Vi
print(forecast)

##################################################################################