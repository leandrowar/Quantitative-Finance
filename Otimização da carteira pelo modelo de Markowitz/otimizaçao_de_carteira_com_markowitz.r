
install.packages("quadprog")

install.packages("PerformanceAnalytics")

install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(IntroCompFinR)

# Inserindo os retornos 2015-2019

ativos <- c("ITUB4", "ABEV3", "NATU3", "PETR4")

itub_ret <- c(-0.13, 0.49, 0.30, 0.32, 0.0235)
abev_ret <- c(0.1207, -0.496, 0.3310, -0.2559, 0.2172)
natu_ret <- c(-0.2184, -0.004, 0.4511, 0.3749, 0.4771)
petr_ret <- c(-0.33,1.21,0.08,0.46,0.07)


# Criando a matriz de retornos

retornos <- cbind(itub_ret, abev_ret, natu_ret, petr_ret)

retornos

# Construindo a matriz de retorno medio

retorno_medio <- rbind(mean(retornos[,1]), mean(retornos[,2]), mean(retornos[,3]), mean(retornos[,4]))

rownames(retorno_medio) <- ativos

retorno_medio

matriz_cov <- cov(retornos)

rownames(matriz_cov) <- ativos
colnames(matriz_cov) <- ativos

matriz_cov

tx_livre_risco <- 0.06

short_selling <- FALSE

# Vamos calcular a nossa carteira mais eficiente - chamado de Tangency Portfolio

# Carteira Eficience
carteira_eficiente <- tangency.portfolio(retorno_medio, matriz_cov, tx_livre_risco, shorts = short_selling)

carteira_eficiente

ativos

# Calculo da carteira com a menor risco possível
carteira_min_risco <- globalMin.portfolio(retorno_medio, matriz_cov, shorts = short_selling)

carteira_min_risco

# compute portfolio frontier
fronteira_eficiente <- efficient.frontier(retorno_medio, matriz_cov, nport = 40, shorts = short_selling)

fronteira_eficiente

attributes(fronteira_eficiente)

# Visualizaçao da saida

plot(fronteira_eficiente, plot.assets=TRUE, col="blue", pch=16)

points(carteira_min_risco$sd, carteira_min_risco$er, col="green", pch=10, cex=2)
points(carteira_eficiente$sd, carteira_eficiente$er, col="red", pch=10, cex=2)

text(carteira_min_risco$sd, carteira_min_risco$er, labels="Risco Minimo", pos=2)
text(carteira_eficiente$sd, carteira_eficiente$er, labels="Carteira Eficiente", pos=2)

tangente <- (carteira_eficiente$er - tx_livre_risco)/carteira_eficiente$sd
abline(a = tx_livre_risco, b=tangente, col="green", lwd=2)
