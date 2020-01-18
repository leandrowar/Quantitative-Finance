#Twitter e Text Mining - Bitcoin
#https://www.outspokenmarket.com/blog
#Leandro Guerra

#Instalando as bibliotecas necessarias
# install.packages("rtweet")
# install.packages("wordcloud")
# install.packages("tm")

#Carregando as libraries
library(rtweet)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(cluster)   
library(fpc)

#####
#Carregando os Tweets
#Voce precisar ter uma conta no Twitter e autorizar
#Limite de 18.000 tweets a cada 15 minutos
bitcoin_tweets <- search_tweets(
  "#bitcoin", n = 18000, include_rts = FALSE,lang = "en")

#Rapida visualizaçao - exemplo tirado da propia documentaçao da rtweet
bitcoin_tweets %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequencia de #bitcoin Twitter posts",
    subtitle = "Tweets a cada 1 hora",
    caption = "\nSource: Dados coletados da Twitter's REST API via rtweet"
  )

#####
#O trabalho de Mineraçao de Textos - Text Mining
bitcoin_text <- bitcoin_tweets$text

#Criando e limpando o corpus
bitcoin_text_corpus <- VCorpus(VectorSource(bitcoin_text))
bitcoin_text_corpus <- tm_map(bitcoin_text_corpus,
                                     content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
bitcoin_text_corpus <- tm_map(bitcoin_text_corpus, content_transformer(tolower))
bitcoin_text_corpus <- tm_map(bitcoin_text_corpus, removePunctuation)
bitcoin_text_corpus <- tm_map(bitcoin_text_corpus,removeWords, stopwords("english"))

#Primeira visualizaçao
wordcloud(bitcoin_text_corpus,min.freq=2,max.words=100)
formatacao <- brewer.pal(8,"Dark2")
wordcloud(bitcoin_text_corpus,min.freq=2,max.words=100, random.order=T, colors=formatacao)

#Mas ainda aparece muito lixo

#####
#Limpeza do texto com a Document Term Matrix
bitcoin_dtm <- DocumentTermMatrix(bitcoin_text_corpus)   
bitcoin_dtm

bitcoin_frequencia <- colSums(as.matrix(bitcoin_dtm))   
length(bitcoin_frequencia) 
tail(bitcoin_frequencia,10)

#Removendo termos esparços
bitcoin_dtms <- removeSparseTerms(bitcoin_dtm, 0.98) 
bitcoin_dtms

bitcoin_frequencia <- colSums(as.matrix(bitcoin_dtms))   
length(bitcoin_frequencia) 

bitcoin_frequencia <- sort(colSums(as.matrix(bitcoin_dtms)), decreasing=TRUE) 
bitcoin_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
bitcoin_plot <- data.frame(word=names(bitcoin_frequencia), freq=bitcoin_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(bitcoin_plot, bitcoin_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico   

#Removendo palavras especificas e limpando novamente o corpus
bitcoin_text_corpus <- tm_map(bitcoin_text_corpus, removeWords, c("bitcoin","btc"))
bitcoin_dtms <- removeSparseTerms(DocumentTermMatrix(bitcoin_text_corpus) , 0.98) 
bitcoin_dtms

bitcoin_frequencia <- colSums(as.matrix(bitcoin_dtms))   
length(bitcoin_frequencia) 

bitcoin_frequencia <- sort(colSums(as.matrix(bitcoin_dtms)), decreasing=TRUE) 
bitcoin_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
bitcoin_plot <- data.frame(word=names(bitcoin_frequencia), freq=bitcoin_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(bitcoin_plot, bitcoin_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico   


#Nova nuvem de palavras
wordcloud(names(bitcoin_frequencia),bitcoin_frequencia,min.freq=2,max.words=150, random.order=T, colors=formatacao)

#####
#Aplicando um pouco de machine learning - Clustering
bitcoin_dtms2 <- removeSparseTerms(bitcoin_dtms, 0.95)
bitcoin_dtms2

#Clustering 1 - Dendograma
distancia <- dist(t(bitcoin_dtms2), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Tweets Bitcoin - Outspoken Market",
     xlab = "Distancia",
     ylab = "Altura")  

#Para ler melhor o Dendograma
groups <- cutree(dendograma, k=5)
rect.hclust(dendograma, k=5, border="red")   

#Clustering 2 - K-Means
kmeans_btc <- kmeans(distancia, 5)   
clusplot(as.matrix(distancia), kmeans_btc$cluster, color=T, shade=T, labels=3, lines=0,
         main = "K-Means Tweets Bitcoin - Outspoken Market",
         xlab = "PC1",
         ylab = "PC2") 
