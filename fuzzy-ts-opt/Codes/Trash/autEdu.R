library(forecast)
# Criando a série temporal
###########################################################################
ts.poaz = ts(dados$target, start = 1980, end = 2001)
autoplot(ts.poaz)
length(ts.poaz)

#########################

#fts.am <- function(ts, n, w, D1, D2, forecast, plot)


###Etapa_1.	Definição do conjunto universal U
###########################################################################

#Calculando a variação entre os dados da série temporal
ts.dif <- diff(ts.poaz) 
autoplot(ts.dif)


#Encontrando o menor e o maior valor de variação
max <- max(ts.dif)
min <- min(ts.dif)

#Definindo os valores das constantes D1 e D2

D1 <- 1800
D2 <- 1100

Vmin <- min - D1
Vmax <- max + D2

U <- c(Vmin, Vmax)

n <- 7

h <- (Vmax - Vmin)/n

K0 <- Vmin + 0*h
k1 <- Vmin + 1*h
k2 <- Vmin + 2*h
k3 <- Vmin + 3*h
k4 <- Vmin + 4*h
k5 <- Vmin + 5*h
k6 <- Vmin + 6*h
k7 <- Vmin + 7*h

diff(ts.poaz)
str(diff(ts.poaz))

ts1 <- abs(as.vector(diff(ts.poaz)))
Vmin <- min(ts1) - D1
Vmax <- max(ts1) + D2
h <- (Vmax - Vmin)/n 

#u[i] <- c(k[i], k[i+1])

k <- 1:(n + 1)

U <- 1:n

for (i in 1:(n + 1)) {
  if (i==1) {
    k[i] <- Vmin
    U[i] <- paste("u", i, sep="")
  }
  else{
    k[i] <- Vmin + (i - 1) * h
    U[i] <- paste("u", i, sep="")
  }
}

u = as.data.frame(matrix(nrow = 7, ncol = 2))
rownames(u) = U[1:7]; colnames(u) = c("Vmin", "Vmax")
c = 1
for (i in 1:7) {
  for (j in 1:2) {
    u[i, j] <- k[c]
    c = c + 1
  }
  c = c - 1
}

