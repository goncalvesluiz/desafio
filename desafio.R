###########################################################-
# Title: desafio.r
# Objective: Desafio Luiza Labs
# Author: Luiz Henrique Goncalves
# Date Modified: 10/10/2017
###########################################################-

###### 1. PREPARAÇÂO DO AMBIENTE ######

#importar bibliotecas
library(readr)
library(dplyr)
library(forecast)

#importar a base de dados
desafio <- read_csv("~/luizalabs/desafio.csv")

#analisar a estrutura dos dados
str(desafio)
#sumarizar os dados / criar dicionário de dados
summary(desafio)

#Tramentamento dos dados
# Para fins da análise considerar apenas os pedidos processados
pedidos <- filter(desafio,desafio$process_status=="processado")

#Agrupamento não supervisionado, utilizando clusters
#função Kmeans 
#Dependente do valor de K, portanto temos que estimar o valor de k
#kmeansruns() que chama a kmeans para varios valores de k e estima o melhor k.

clustering.ch <- kmeansruns(pedidos[,c(3,4,5,6,7,9)], krange = 1:5, criterion = "ch")
k <- clustering.ch$bestk
#k == 4

#selecionado apenas colunas numéricas, executar a função kmeans
kp = kmeans(pedidos[,c(3,4,5,6,7,9)],k)

#adicionar a coluna dos clusters a base
pedidos <- data.frame(pedidos,kp$cluster)


#################################################
#Trecho executado apenas para amostragem e analise
################################################
#Um outro modo de classificar os dados em um estrutura hierarquica e dendogramas
#realizar um recorte dos dados para exemplo
recorte <- pedidos [1:50,]
#calculo da distancia entre os valores (Distancia Euclideana)
d = dist(recorte[,c(3,4,5,6,7,9)])
#funcao hclust() gera um cluster hierarquico
h = hclust(d)
#plot para visualizacao
plot(h)
#recorte/ contorno de cada agrupamento
rect.hclust(h,4)
# Optou-se por utilizar a função kmeans()
##############################################

#filtrando a base por categorias e separando em dataframes distintos
cat1 <- filter(pedidos,kp.cluster == "1")
cat2 <- filter(pedidos,kp.cluster == "2")
cat3 <- filter(pedidos,kp.cluster == "3")
cat4 <- filter(pedidos,kp.cluster == "4")

#consolidar a quantidade de venda mensal para cada serie criada
#cat1 <- tapply(cat1$quantity, cat1$process_date, FUN=sum)
#cat1_consolidado <-tapply(cat1$quantity, format(cat1$process_date, format="%Y-%m-d"), FUN=sum)
cat1con <- aggregate(cat1$quantity, by=list(format(cat1$process_date, format="%Y-%m")), FUN=sum)
cat2con <- aggregate(cat2$quantity, by=list(format(cat2$process_date, format="%Y-%m")), FUN=sum)
cat3con <- aggregate(cat3$quantity, by=list(format(cat3$process_date, format="%Y-%m")), FUN=sum)
cat4con <- aggregate(cat4$quantity, by=list(format(cat4$process_date, format="%Y-%m")), FUN=sum)


############################################
#Observacao:
#Não existem dados completos para o mes 06/2017, portanto esses dados serão desconsiderados na série temporal
#Na serie cat4, existe apenas um valor para o mês 07/2017 e tambem será desconsiderado
###########################################
cat1con <- cat1con[1:12,]
cat2con <- cat2con[1:12,]
cat3con <- cat3con[1:12,]
cat4con <- cat4con[1:12,]
#Para poder realizar uma previsão temporal,
#será necessário converter o dataframe em uma serie temporal
#no R utilizamos a função ts()
#descobrir a data inicial
#min(cat1con$Group.1) = 2017-06

cat1ts <- ts(cat1con$x,frequency = 12, start = c(2016,6))
cat2ts <- ts(cat2con$x,frequency = 12, start = c(2016,6))
cat3ts <- ts(cat3con$x,frequency = 12, start = c(2016,6))
cat4ts <- ts(cat4con$x,frequency = 12, start = c(2016,6))

#aplicando um modelo temporal
#o modelo a ser aplicado será o TBATS
modelo1 <- tbats(cat1ts)
modelo2 <- tbats(cat2ts)
modelo3 <- tbats(cat3ts)
modelo4 <- tbats(cat4ts)

#previsão dos dados
pred1 <- predict(modelo1,3,prediction.interval=TRUE,level=0.95)
pred2 <- predict(modelo2,3,prediction.interval=TRUE)
pred3 <- predict(modelo3,3,prediction.interval=TRUE)
pred4 <- predict(modelo4,3,prediction.interval=TRUE)



