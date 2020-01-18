library("quantmod")
library("lubridate")
library("e1071")
library(data.table)
library(gridExtra)
library(fTrading)
library(TTR)
library(fastDummies)
library(dplyr)
library(ncar)
library(pracma)
library(scales)
library(ggplot2)
library(useful)

#Machine Learning para Trading
#Classificador Naive Bayes
#https://www.outspokenmarket.com
#Leandro Guerra




euro <- fread("EURUSD_H1.csv", header = TRUE, sep = ";")
euro <- as.data.frame(euro)
euro$Hour <- as.numeric(substr(euro$Date, 12, 13))
euro$Years <- as.numeric(substr(euro$Date, 7, 10))
euro$Pips <- (euro$Close - euro$Open)*10000
euro <- shift.column(data=euro, columns="Pips", len=1)
euro$Today <- ifelse(euro$Pips > 0,"Alta","Baixa")
euro$Dir <- ifelse(euro$Pips.Shifted>0,"Alta","Baixa")

euro$RSI <- RSI(euro$Close,n = 14)
euro$CCI <- CCI(euro$Close,n = 20)
euro$HoraAtual <- as.factor(euro$Hour)
euro$RSI_Status <- as.factor(ifelse(euro$RSI > 70,"Sobrecomprado",ifelse(euro$RSI < 30,"Sobrevendido","Neutro")))
euro$CCI_Status <- as.factor(ifelse(euro$CCI > 100,"Sobrecomprado",ifelse(euro$CCI < -100,"Sobrevendido","Neutro")))

table(euro$Dir,euro$RSI_Status)

training <- subset(euro,euro$Years <= 2017) 
testing <-subset(euro,euro$Years == 2018) 


bayes <- naiveBayes(as.factor(Dir) ~ CCI_Status + Hour, data= training, na.action = na.omit,method="class")
bayes

table(testing$Dir,testing$CCI_Status)

testing$Pred <- predict(bayes,testing)

table(testing$Pred,testing$Dir,dnn=list('Previsto','Real'))

(table(testing$Pred,testing$Dir,dnn=list('Previsto','Real'))[4]+table(testing$Pred,testing$Dir,dnn=list('Previsto','Real'))[1])/dim(testing)[1]

testing$Resultado <- ifelse(as.character(testing$Pred) == "Baixa",-1*testing$Pips.Shifted ,testing$Pips.Shifted )
cumulative_buy_pips <- cumsum(testing$Resultado)

df <- data.frame(seq.int(dim(testing)[1]),cumulative_buy_pips)
g <- ggplot(df, aes(seq.int(dim(testing)[1])))
g <- g + geom_line(aes(y=cumulative_buy_pips,linetype="Pips"), colour="blue")
g <- g + ggtitle("Naive Bayes")+labs(x="Resultados de 2018",
                                         y="Pips Totais") 
g <- g + annotate("text", x = dim(testing)[1]- 10, y = tail(cumulative_buy_pips,1)-45, label = paste(round(tail(cumulative_buy_pips,1),2)))
g <- g + theme(legend.position=c(.35, .95), legend.justification = c(1, 1),
               legend.background = element_rect(color = "black",fill = "grey90", size = 1, linetype = "solid")) + scale_shape_discrete(name="")
g <- g + labs(linetype="Resultado acumulado")
g
