#Long & Short por Cointegracao - Parte 2 - O Trading System
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra - Outspoken Market

#Bibliotecas
library(data.table)
library(ggplot2)
library(tseries)
library(lubridate)

#####
#Carregando o Dataset e ajustando a data
pairs <- fread("Forex_Pairs.csv", header = TRUE, sep = ";")
pairs <- data.frame(pairs)
names(pairs)
pairs$DATE <- as.POSIXct(strptime(pairs$DATE, format= "%d/%m/%Y"))

#####
#Plotando o Grafico
ggplot(pairs, aes(DATE)) + 
  geom_line(aes(y = GBPUSD, colour = "GBPUSD")) + 
  geom_line(aes(y = EURAUD, colour = "EURAUD")) +
  geom_line(aes(y = EURCAD, colour = "EURCAD")) +
  geom_line(aes(y = EURGBP, colour = "EURGBP")) +
  geom_line(aes(y = EURUSD, colour = "EURUSD")) +
  geom_line(aes(y = GBPAUD, colour = "GBPAUD")) +
  geom_line(aes(y = GBPCHF, colour = "GBPCHF")) +
  xlab("Date") + 
  ylab("Price") + 
  labs(colour = 'Pair', title = "Entendendo o que é cointegraçao - Outspoken Market") 

#Nao sao cointegrados
#GBPUSD com EURAUD - EURCAD - EURGBP - EURUSD - GBPAUD
#EURAUD com GBPUSD - EURCAD - EURGBP - EURUSD - GBPAUD - GBPCHF
#EURCAD com GBPUSD - EURAUD - EURGBP - EURUSD - GBPAUD - GBPCHF
#EURGBP com GBPUSD - EURCAD - EURAUD - EURUSD - GBPAUD - GBPCHF
#EURUSD com GBPUSD - EURAUD - EURCAD - EURGBP - GBPAUD - GBPCHF
#GBPAUD com GBPUSD - EURAUD - EURCAD - EURGBP - EURUSD - GBPCHF
#GBPCHF com GBPUSD - EURAUD - EURCAD - EURGBP - EURUSD - GBPAUD

#Sao cointegrados
#GBPUSD com GBPCHF


#####
#Ajustando a regressao
reg1 <- lm(GBPUSD ~  EURUSD + EURGBP, data = pairs)
summary(reg1)

#####
#Aplicando o teste de Dickey-Fuller - aceitando a cointegracao apenas com p-value > 0.05
#Calculando os residuos
res1 <- residuals(reg1)
plot(res1,type = "l", col = "purple", lwd = 2 ,main = "Residuos", ylab = "Residuo", xlab = "Observaçoes")

tseries::adf.test(res1)

#####
#Montando o trading system
reg1$coefficients

