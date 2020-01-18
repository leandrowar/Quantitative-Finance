# Meu primeiro programa
# Calculando o risco da minha conta
# Bibliografia - The Theory of Stochastic Processes - D.R. Cox, H.D. Miller
# 19/08/2018
# www.outspokenmarket.com
# Leandro Guerra 


#Variaveis
RetornoMedio <- 0.052
DesvioPadrao <- 0.102


#Formula do Risco da Conta
RiscoConta <- 2.71828^(-2*RetornoMedio/DesvioPadrao)
RiscoConta*100

#Calculando o Risco de Perder 50% da conta
RiscoGeral <-	0.25
RiscoGeralF <- 2.71828^(-(2*RetornoMedio*RiscoGeral)/(DesvioPadrao*DesvioPadrao))
RiscoGeralF*100

#0.67%