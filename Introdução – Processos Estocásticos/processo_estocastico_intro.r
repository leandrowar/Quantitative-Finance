
#Leandro Guerra - Outspoken Market
#Simples processo estatocastico para simular o movimento de uma açao

#Distribuiçao normal, 255 valores, com media 0 e desvio p. 1
dist <- rnorm(255,0,1)

#Parametros do processo

#Retorno anual esperado - 23%
u <- 0.23              

#Desvio padrao anual esperado - 15%
sd <- 0.15             

#Parametros da açao simulada

#Preco inicial
p <- 10 

#Vetor para acumular os preços ao longo do tempo
preco <- c(p)         

#Parametros do tempo
aux <- 2                
periodo <- 1:256           

#Simulaçao

for(i in dist)
{
    P = p + p*(u/255 + sd/sqrt(255)*i)
    preco[aux] <- P                        
    p = P                                 
    aux = aux + 1
}


#Visualizaçao da simulaçao

plot(periodo,preco,main="Simulaçao de uma açao",xlab="Tempo",ylab="Preço", type="l",col="green")
summary(preco)

#Estatisticas da simulaçao

estatisticas <- c(sd(preco),mean(preco),(preco[256]-preco[1])/preco[1]*100)
names(estatisticas) <- c("Volatilidade","Preço Medio","Retorno em %")
print(estatisticas)


