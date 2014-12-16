library("quantmod") #Carrega

#Seleção do período de análise
startDate = as.Date("2014-05-01") 
endDate = as.Date("2014-11-21")

#Seleção das ações
tickers <- c("PETR4.SA")

#Download dos dados do período
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Mostra os primeiros 5 registros para as ações da Petrobras
head(PETR4.SA,5)


#Gráfico de candlestick da Petrobras
candleChart(PETR4.SA)

#Adicionando o SAR
addSAR(accel = c(0.01, 0.2), col = "blue")
addSAR(accel = c(0.02, 0.2), col = "red")
addSAR(accel = c(0.03, 0.2), col = "green")
addSAR(accel = c(0.04, 0.2), col = "white")

#Salva os valores na variável valorSAR
valorSAR <- SAR(PETR4.SA,accel = c(0.02, 0.2))
