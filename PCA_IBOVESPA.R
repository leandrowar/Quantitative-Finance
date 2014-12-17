library("quantmod") 
install.packages("moments")
library(moments)  # to get skew & kurtosis

#Seleção do período de análise
startDate = as.Date("2014-01-01") 
endDate = as.Date("2014-12-15")

#Seleção das ações
tickers = c('^BVSP','GGBR4.SA','USIM5.SA','VALE5.SA','PETR4.SA','BBDC4.SA','ITUB4.SA')
tickers_setor = c('Mercado','Siderugira','Siderurgia','Mineracao','Petroleo','Banco','Banco')

getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

BVSP_RET <- dailyReturn(BVSP)
GGBR4_RET <- dailyReturn(GGBR4.SA)
USIM5_RET <- dailyReturn(USIM5.SA)
VALE5_RET <- dailyReturn(VALE5.SA)
PETR4_RET <- dailyReturn(PETR4.SA)
BBDC4_RET <- dailyReturn(BBDC4.SA)
ITUB4_RET <- dailyReturn(ITUB4.SA)
plot(BVSP_RET)


cor(GGBR4_RET,USIM5_RET)
hist(GGBR4_RET)

#install.packages("nortest")
#library(nortest)

shapiro.test(as.numeric(GGBR4_RET))
#ad.test(GGBR4_RET)
qqnorm(GGBR4_RET)
qqline(GGBR4_RET, col = 2)

statNames <- c("Media", "Desvio", "Inclinacao", "Curtose")
GGBR4_STATS <- c(mean(GGBR4_RET), sd(GGBR4_RET), skewness(GGBR4_RET), kurtosis(GGBR4_RET))
names(GGBR4_STATS) <- statNames
GGBR4_STATS

grupo_ret <- cbind(BVSP_RET,GGBR4_RET,USIM5_RET,VALE5_RET,PETR4_RET,BBDC4_RET,ITUB4_RET)
PCA <- prcomp(grupo_ret,center = TRUE, scale. = TRUE)
plot(PCA)
summary(PCA)
PCA_VAR_EXPLAINED = PCA$sdev^2 / sum(PCA$sdev^2)
barplot(100*PCA_VAR_EXPLAINED, las=2, xlab='', ylab='% Variância Explicada', main = "Variância por PC")

p = princomp(na.omit(grupo_ret))
loadings = p$loadings[]
PC1 = loadings[,1]
PC2 = loadings[,2]

# PC1 em função de PC2 para entender a relação dos retornos diários
plot(PC1, PC2, type='p', pch=20, 
     xlab='Componente 1', 
     ylab='Componente 2', 
     ylim = c(-0.75,0.45), 
     xlim = c(-0.7,-0.1),
     main = 'PCA para o IBovespa')
text(PC1, PC2, tickers, cex=.7, pos=4)
