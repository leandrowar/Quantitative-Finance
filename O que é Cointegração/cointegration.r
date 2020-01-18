#Long & Short por Cointegracao - Parte 1 - O que é cointegracao
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra - Outspoken Market

#Bibliotecas
library(data.table)
library(ggplot2)
library(tseries)
library(lubridate)

#####
#Carregando o Dataset e ajustando a data
GBP_EUR <- fread("GBP_EUR_COINTEGRATION.csv", header = TRUE, sep = ";")
GBP_EUR <- data.frame(GBP_EUR)
names(GBP_EUR)
GBP_EUR$DATE <- as.POSIXct(strptime(GBP_EUR$DATE, format= "%d/%m/%Y"))

#####
#Plotando o Grafico
ggplot(GBP_EUR, aes(DATE)) + 
  geom_line(aes(y = EUR_USD, colour = "EUR_USD")) + 
  geom_line(aes(y = GBP_USD, colour = "GBP_USD")) +
  xlab("Date") + 
  ylab("Price") + 
  labs(colour = 'Pair', title = "Entendendo o que é cointegraçao - Outspoken Market") 

#####
#Ajustando a regressao
regression <- lm(EUR_USD ~ GBP_USD, data = GBP_EUR)
summary(regression)

#####
#Aplicando o teste de Dickey-Fuller
#Calculando os residuos
res <- residuals(regression)
plot(res,type = "l", col = "red", main = "Residuos da regressao EUR/USD - GBP/USD", ylab = "Residuo", xlab = "Amostras")

tseries::adf.test(res)

#Aplicando o teste de Engle - Granger
po.test(log(GBP_EUR[,2:3]))
