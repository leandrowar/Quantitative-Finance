library(Quantmod) 
library(moments)  # to get skew & kurtosis

#Seleção do período de análise
startDate = as.Date("2008-01-01") 
endDate = as.Date("2014-12-19")

#Seleção das ações
tickers = c('^BVSP','BBDC4.SA')

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Cálculo dos retornos
BVSP_RET <- dailyReturn(BVSP)
BBDC4_RET <- dailyReturn(BBDC4.SA)

#Funções auxiliares
index(BVSP_RET) #Retorna as datas (que estão como índice das linhas)
coredata(BVSP_RET) #Retorna os dados principais, ou seja, os valores
length(BVSP_RET) #1732
length(BBDC4_RET) #1662
length(BVSP_RET[index(BVSP_RET)%in%index(BBDC4_RET)]) #1662

### Para plotar os mesmos dias dos retornos
#Atribui os valores que são conjuntos
BVSP_SAME <- BVSP_RET[index(BVSP_RET)%in%index(BBDC4_RET)]
plot(coredata(BVSP_SAME),coredata(BBDC4_RET))
fit_RET_BVSP_BBDC4 <- lm(BVSP_SAME~BBDC4_RET)
summary(fit_RET_BVSP_BBDC4)
abline(v=median(BVSP_SAME), col = "blue")
abline(h=median(BBDC4_RET), col = "red")
abline(fit_RET_BVSP_BBDC4, col="green") 

#Retornos acumulados
BVSP_ACCUM <- cumsum(BVSP_SAME)
plot(index(BVSP_ACCUM),coredata(BVSP_ACCUM),
     type = 'h',
     col=ifelse(coredata(BVSP_ACCUM)>0,"blue", "red"),
     main = "Ibov acumulado"
)

BBDC4_ACCUM <- cumsum(BBDC4_RET)
plot(index(BBDC4_ACCUM),coredata(BBDC4_ACCUM),
     type = 'h',
     col=ifelse(coredata(BBDC4_ACCUM)>0,"blue", "red"),
     main = "BBDC4 acumulado"
)


#Subseting dos retornos por ano
BBDC4_RET_2008 <- BBDC4_RET[index(BBDC4_RET)>'2008-01-01' & index(BBDC4_RET)<'2009-01-01']
BBDC4_RET_2009 <- BBDC4_RET[index(BBDC4_RET)>'2009-01-01' & index(BBDC4_RET)<'2010-01-01']
BBDC4_RET_2010 <- BBDC4_RET[index(BBDC4_RET)>'2010-01-01' & index(BBDC4_RET)<'2011-01-01']
BBDC4_RET_2011 <- BBDC4_RET[index(BBDC4_RET)>'2011-01-01' & index(BBDC4_RET)<'2012-01-01']
BBDC4_RET_2012 <- BBDC4_RET[index(BBDC4_RET)>'2012-01-01' & index(BBDC4_RET)<'2013-01-01']
BBDC4_RET_2013 <- BBDC4_RET[index(BBDC4_RET)>'2013-01-01' & index(BBDC4_RET)<'2014-01-01']
BBDC4_RET_2014 <- BBDC4_RET[index(BBDC4_RET)>'2014-01-01' & index(BBDC4_RET)<'2014-12-19']

#Calculo dos retornos acumulado por ano
BBDC4_ACCUM_2008 <- cumsum(BBDC4_RET_2008)
BBDC4_ACCUM_2009 <- cumsum(BBDC4_RET_2009)
BBDC4_ACCUM_2010 <- cumsum(BBDC4_RET_2010)
BBDC4_ACCUM_2011 <- cumsum(BBDC4_RET_2011)
BBDC4_ACCUM_2012 <- cumsum(BBDC4_RET_2012)
BBDC4_ACCUM_2013 <- cumsum(BBDC4_RET_2013)
BBDC4_ACCUM_2014 <- cumsum(BBDC4_RET_2014)

#Retorna os dias da semana dos retornos
weekdays(index(BBDC4_RET_2014))

#Retornos das segundas-feiras de 2013
BBDC4_SEG_2013 <- BBDC4_RET_2013[weekdays(index(BBDC4_RET_2013)) == "segunda-feira"]

#Retornos das segundas-feiras de 2014
BBDC4_SEG_2014 <- BBDC4_RET_2014[weekdays(index(BBDC4_RET_2014)) == "segunda-feira"]
max(cumsum(BBDC4_SEG_2013))
par(mfrow=c(2,1))
plot(cumsum(BBDC4_SEG_2013), main = 'Retornos das 2ªs de 2013')
plot(cumsum(BBDC4_SEG_2014), main = 'Retornos das 2ªs de 2014')

#Quantidade de retornos acima de 0%
table(BBDC4_SEG_2014 > 0)

#Retonar percentis de 5% e 95%
quantile(BBDC4_RET_2014,probs=c(0.05,0.95))
#Ou seja, 5% dos retornos da BBDC4 são menores que -8,14% e 95% dos retornos são menores que 6,16%

#VaR BBDC4 e BVSP
BVSP_RET_2014 <- BVSP_RET[index(BVSP_RET)>'2014-01-01' & index(BVSP_RET)<'2014-12-19']
Investimento <- 10000
quantile(BBDC4_RET_2014,probs=c(0.01,0.05))
BBDC4_VaR_2014 <- Investimento * quantile(BBDC4_RET_2014,probs=c(0.01,0.05))
BBDC4_VaR_2014

quantile(BVSP_RET_2014,probs=c(0.01,0.05))
BVSP_VaR_2014 <- Investimento * quantile(BVSP_RET_2014,probs=c(0.01,0.05))
BVSP_VaR_2014

#Baseada na distribuição dos retornos diarios, R$10.000 investidos na BBDC4
#resultariam numa perda de R$849,70 ou mais com 1% de probabilidade ou R$578,23
#ou mais com 5% de probabilidade. Os mesmos valores para o IBovespa são
#R$361,36 e R$247,99. Ou seja, é mais arriscado investir em BBDC4 do que no
#índice como um todo

#Identificação de tendência com ADX
valorADX <- ADX(BBDC4.SA,n = 11, maType="EMA")

WEAK_HIGH_TREND <- ifelse(valorADX$ADX > 0 & valorADX$ADX <= 25 & valorADX$DIp > valorADX$DIn,1,0)
STRONG_HIGH_TREND <- ifelse(valorADX$ADX > 25 & valorADX$ADX <= 50 & valorADX$DIp > valorADX$DIn,1,0)
VERY_STRONG_HIGH_TREND <- ifelse(valorADX$ADX > 50 & valorADX$ADX <= 75 & valorADX$DIp > valorADX$DIn,1,0)
EXTREMILY_STRONG_HIGH_TREND <- ifelse(valorADX$ADX > 75 & valorADX$DIp > valorADX$DIn,1,0)
WEAK_LOW_TREND <- ifelse(valorADX$ADX > 0 & valorADX$ADX <= 25 & valorADX$DIn > valorADX$DIp,1,0)
STRONG_LOW_TREND <- ifelse(valorADX$ADX > 25 & valorADX$ADX <= 50 & valorADX$DIn > valorADX$DIp,1,0)
VERY_STRONG_LOW_TREND <- ifelse(valorADX$ADX > 50 & valorADX$ADX <= 75 & valorADX$DIn > valorADX$DIp,1,0)
EXTREMILY_STRONG_LOW_TREND <- ifelse(valorADX$ADX > 75 & valorADX$DIn > valorADX$DIp,1,0)

table(STRONG_LOW_TREND)/sum(table(STRONG_LOW_TREND))

#ADX Value  Trend Strength
#0-25  Absent or Weak Trend
#25-50  Strong Trend
#50-75	Very Strong Trend
#75-100	Extremely Strong Trend
