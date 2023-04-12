library(quantmod) 
library(moments)

#Seleção do período de análise
startDate = as.Date("2008-01-01") 
endDate = as.Date("2020-03-16")

#Seleção das ações
tickers = c('^BVSP','ITUB4.SA')

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

ITUB4.SA <- na.omit(ITUB4.SA)
BVSP <- na.omit(BVSP)

#Cálculo dos retornos
BVSP_RET <- dailyReturn(BVSP)
ITUB4_RET <- dailyReturn(ITUB4.SA)

#Funções auxiliares
index(BVSP_RET) #Retorna as datas (que estão como índice das linhas)
coredata(BVSP_RET) #Retorna os dados principais, ou seja, os valores
length(BVSP_RET) #3016
length(ITUB4_RET) #3008

length(BVSP_RET[index(BVSP_RET)%in%index(ITUB4_RET)]) #3002

### Para plotar os mesmos dias dos retornos
#Atribui os valores que são conjuntos
BVSP_RET <- BVSP_RET[index(BVSP_RET)%in%index(ITUB4_RET)]
ITUB4_RET <- ITUB4_RET[index(ITUB4_RET)%in%index(BVSP_RET)]
length(BVSP_RET) 
length(ITUB4_RET)
plot(coredata(BVSP_RET),coredata(ITUB4_RET))
fit_RET_BVSP_ITUB4 <- lm(BVSP_RET~ITUB4_RET)
summary(fit_RET_BVSP_ITUB4)
abline(v=median(BVSP_RET), col = "blue")
abline(h=median(ITUB4_RET), col = "red")
abline(fit_RET_BVSP_ITUB4, col="green") 

#Retornos acumulados
BVSP_ACCUM <- cumsum(BVSP_RET)
plot(index(BVSP_ACCUM),coredata(BVSP_ACCUM),
     type = 'h',
     col=ifelse(coredata(BVSP_ACCUM)>0,"blue", "red"),
     main = "Ibov acumulado"
)

ITUB4_ACCUM <- cumsum(ITUB4_RET)
plot(index(ITUB4_ACCUM),coredata(ITUB4_ACCUM),
     type = 'h',
     col=ifelse(coredata(ITUB4_ACCUM)>0,"blue", "red"),
     main = "ITUB4 acumulado"
)


#Subseting dos retornos por ano
ITUB4_RET_2008 <- ITUB4_RET[index(ITUB4_RET)>'2008-01-01' & index(ITUB4_RET)<'2009-01-01']
ITUB4_RET_2009 <- ITUB4_RET[index(ITUB4_RET)>'2009-01-01' & index(ITUB4_RET)<'2010-01-01']
ITUB4_RET_2010 <- ITUB4_RET[index(ITUB4_RET)>'2010-01-01' & index(ITUB4_RET)<'2011-01-01']
ITUB4_RET_2011 <- ITUB4_RET[index(ITUB4_RET)>'2011-01-01' & index(ITUB4_RET)<'2012-01-01']
ITUB4_RET_2012 <- ITUB4_RET[index(ITUB4_RET)>'2012-01-01' & index(ITUB4_RET)<'2013-01-01']
ITUB4_RET_2013 <- ITUB4_RET[index(ITUB4_RET)>'2013-01-01' & index(ITUB4_RET)<'2014-01-01']
ITUB4_RET_2014 <- ITUB4_RET[index(ITUB4_RET)>'2014-01-01' & index(ITUB4_RET)<'2015-01-01']
ITUB4_RET_2015 <- ITUB4_RET[index(ITUB4_RET)>'2015-01-01' & index(ITUB4_RET)<'2016-01-01']
ITUB4_RET_2016 <- ITUB4_RET[index(ITUB4_RET)>'2016-01-01' & index(ITUB4_RET)<'2017-01-01']
ITUB4_RET_2017 <- ITUB4_RET[index(ITUB4_RET)>'2017-01-01' & index(ITUB4_RET)<'2018-01-01']
ITUB4_RET_2018 <- ITUB4_RET[index(ITUB4_RET)>'2018-01-01' & index(ITUB4_RET)<'2019-01-01']
ITUB4_RET_2019 <- ITUB4_RET[index(ITUB4_RET)>'2019-01-01' & index(ITUB4_RET)<'2020-01-01']
ITUB4_RET_2020 <- ITUB4_RET[index(ITUB4_RET)>'2020-01-01' & index(ITUB4_RET)<'2020-03-16']

#Calculo dos retornos acumulado por ano
ITUB4_ACCUM_2008 <- cumsum(ITUB4_RET_2008)
ITUB4_ACCUM_2009 <- cumsum(ITUB4_RET_2009)
ITUB4_ACCUM_2010 <- cumsum(ITUB4_RET_2010)
ITUB4_ACCUM_2011 <- cumsum(ITUB4_RET_2011)
ITUB4_ACCUM_2012 <- cumsum(ITUB4_RET_2012)
ITUB4_ACCUM_2013 <- cumsum(ITUB4_RET_2013)
ITUB4_ACCUM_2014 <- cumsum(ITUB4_RET_2014)
ITUB4_ACCUM_2015 <- cumsum(ITUB4_RET_2015)
ITUB4_ACCUM_2016 <- cumsum(ITUB4_RET_2016)
ITUB4_ACCUM_2017 <- cumsum(ITUB4_RET_2017)
ITUB4_ACCUM_2018 <- cumsum(ITUB4_RET_2018)
ITUB4_ACCUM_2019 <- cumsum(ITUB4_RET_2019)
ITUB4_ACCUM_2020 <- cumsum(ITUB4_RET_2020)

#Retorna os dias da semana dos retornos
weekdays(index(ITUB4_RET_2014))

#Retornos das segundas-feiras de 2013
ITUB4_SEG_2013 <- ITUB4_RET_2013[weekdays(index(ITUB4_RET_2013)) == "lunedì"]
max(cumsum(ITUB4_SEG_2013))*100

#Retornos das segundas-feiras de 2014
ITUB4_SEG_2014 <- ITUB4_RET_2014[weekdays(index(ITUB4_RET_2014)) == "lunedì"]
max(cumsum(ITUB4_SEG_2014))*100

#Retornos das segundas-feiras de 2019
ITUB4_SEG_2019 <- ITUB4_RET_2019[weekdays(index(ITUB4_RET_2019)) == "lunedì"]
max(cumsum(ITUB4_SEG_2019))*100


par(mfrow=c(3,1))
plot(cumsum(ITUB4_SEG_2013), main = 'Retornos das 2ªs de 2013')
plot(cumsum(ITUB4_SEG_2014), main = 'Retornos das 2ªs de 2014')
plot(cumsum(ITUB4_SEG_2019), main = 'Retornos das 2ªs de 2019')

#Quantidade de retornos acima de 0%
table(ITUB4_SEG_2014 > 0)
table(ITUB4_SEG_2019 > 0)

#Retonar percentis de 5% e 95%
quantile(ITUB4_RET_2014,probs=c(0.05,0.95))
#Ou seja, 5% dos retornos da ITUB4 são menores que -3.16% e 95% dos retornos são menores que 3.64%

quantile(ITUB4_RET_2019,probs=c(0.05,0.95))
#Ou seja, 5% dos retornos da ITUB4 são menores que -2.31% e 95% dos retornos são menores que 2.66%

#VaR ITUB4 e BVSP
BVSP_RET_2019 <- BVSP_RET[index(BVSP_RET)>'2019-01-01' & index(BVSP_RET)<'2020-01-01']
Investimento <- 10000
quantile(ITUB4_RET_2019,probs=c(0.01,0.05))
ITUB4_VaR_2019 <- Investimento * quantile(ITUB4_RET_2019,probs=c(0.01,0.05))
ITUB4_VaR_2019

quantile(BVSP_RET_2019,probs=c(0.01,0.05))
BVSP_VaR_2019 <- Investimento * quantile(BVSP_RET_2019,probs=c(0.01,0.05))
BVSP_VaR_2019

#Baseada na distribuição dos retornos diarios, R$10.000 investidos na ITUB4
#resultariam numa perda de R$849,70 ou mais com 1% de probabilidade ou R$578,23
#ou mais com 5% de probabilidade. Os mesmos valores para o IBovespa são
#R$361,36 e R$247,99. Ou seja, é mais arriscado investir em ITUB4 do que no
#índice como um todo

#Identificação de tendência com ADX
valorADX <- ADX(ITUB4.SA,n = 14, maType="EMA")

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
