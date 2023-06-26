# Lendo o arquivo .csv no R 
BigMacData <- read.csv("epData (3).csv", sep = ",")
# verificando se foi lido corretamente
ls.str(BigMacData)

## CONFERINDO A IMPORTAÇÃO
# currencyCode :  chr [1:1868] "ARS" "AUD" ...
# date :  chr [1:1868] "2000-04-01" ...
# dollarEx :  num [1:1868] 1 1.68 1.79 1.47 1.7 ...
# GDPDollar :  num [1:1868] 8709 21747 3501 22341 41859 ...
# GDPLocal :  num [1:1868] 8709 33699 6351 33191 62879 ...
# isoA3 :  chr [1:1868] "ARG" "AUS" ...
# localPrice :  num [1:1868] 2.5 2.59 2.95 2.85 5.9 ...
# name :  chr [1:1868] "Argentina" ...


# Também podemos visualizar com a função View()
View(BigMacData)
# class(BigMacData) data.frame

Paises <- unique(sort(BigMacData[, "isoA3"]))
Paises

## Esses países foram separados por Continente
# Apenas os Nomes - vamos utilizar os valores posterioremente 
Paises.America.Sul <- c("ARG", "CHL", "COL", "PER")

# Alguns países da Europa
Países.Europa <- c("ITA", "DEU", "DNK", "RUS")

################## EP ##################

# E1 Faça uma função criaVetorAno que recebe um dataframe D como parâmetro e
## retorna um vetor com o ano de cada coleta do valor do Big Mac

## ?strsplit

# Criando a função criaVetorAno
criaVetorAno <- function(D) {
  # cria um vetor vazio
  vetor <- c()
  
  # realizando o split na coluna date
  date <- strsplit(BigMacData$date, split = "-", fixed = T) # retorna a coluna de datas
  
  # bloco que extrai o ano da coluna date
  matrix.date <- matrix(unlist(date), ncol=3, byrow = T) # transformando a variável date em uma matrix
  matrix.date[, 1] # pegando todos os anos da coluna [,1] 
  vetor <- matrix.date[, 1] # atribuindo esses valores ao vetor vazio
  
  return(vetor)

}
# retornandoo valor da função criaVetorAno
criaVetorAno(BigMacData)

### Adicionando o valor da função ao data.frame BigMacData
# E6. Use a função criaVetorAno e agregue o resultado ao dataframe BigMacData
# usando a função cbind
BigMacData <- cbind(BigMacData, criaVetorAno())

## Podemos verificar o resultado com a função View()
View(BigMacData)

# ...
# $ criaVetorAno(): chr  "2000" "2000" "2000" "2000" ...

### ALTERANDO O TIPO DE DADO ###
BigMacData$`criaVetorAno()` <- as.integer(BigMacData$`criaVetorAno()`)
str(BigMacData) # confirma as classes dos dados

# ...
# $ criaVetorAno(): int  2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...

###

?ifelse

## E2 Faça uma função BigMacUSPrice que recebe um dataframe D como parâmetro e 
## um número inteiro positivo A (que corresponde a um ano) e retorna do valor 
## do Big Mac nos Estados Unidos naquele ano.

## A : corresponde a um ano
## criando a função BigMacUSPrice
BigMacUSPrice <- function(D, A) {
  
  # Extraindo os valores da coluna isoA3
  vetor_USA <- unlist(BigMacData$isoA3)
  vetor_USA
  
  # Extraindo os anos da coluna criaVetorAno()
  vetor_Ano <- unlist(BigMacData$`criaVetorAno()`)
  vetor_Ano
  
  # Extraindo o valor local do Big Mac 
  valor.local <- unlist(BigMacData$localPrice)
  valor.local
  
  # Testando a condicional com vetores
  ifelse(vetor_USA == "USA" & vetor_Ano == A, valor.local, print("-"))
  
}

# retornando o valor da função BigMacUSPrice
BigMacUSPrice(BigMacData, 2002) # adicionar o valor de um ano


###


## E3 Faça uma função mediaColunaPais que recebe como parãmetro um dataframe D,
## um número inteiro positivo C (que corresponde numérica do dataframe) e uma string
## de três letras P (que corresponde ao código do país) e retorna a média de todos
## os valores da coluna C cujo país é igual a P. Você deve usar a fórmula da média
## sem usar as funções sum, ou mean, ou qualquer outra que o R tenha pronta.

## C corresponde a uma coluna numérica
## P corresponde a uma string de 3 letras (código do país)

mediaColunaPais <- function(D, C, P) {
  
  # selecionamos os dados do país desejado
  pais <- c(BigMacData[BigMacData$isoA3 == P, ])
  
  # Transformando a variável pais em um objeto data.frame
  pais <- print(as.data.frame(do.call(cbind, pais)))
  
  # Vamos transformar a variável localPrice para o tipo numérico
  pais$localPrice <- as.numeric(pais$localPrice)
  
  N <- length(pais$localPrice)
  
  # Somando o valor da variável localPrice
  soma <- 0
  for (i in pais$localPrice) {
    soma <- soma + i
  }
  

  media <- soma / N

  
  return(paste("O valor médio do local é: ", round(media, 4)))
}

# retorna o valor da função mediaColunaPais
mediaColunaPais(BigMacData, BigMacData$localPrice , "BRA")


##


## E4 Faça uma função varColunaPais que recebe como parâmetro um dataframe D,
## um número inteiro positivo C (que corresponde a uma coluna numérica do dataframe) e
## uma string de três letras P (que corresponde ao código do País) e retorna a variância
## de todos os valores da coluna C cujo país é igual a P. Você deve usar a fórmula da variância
## sem usar as funções sum, ou mean, ou var, ou qualquer outra que o R tenha pronta.

varColunaPais <- function(D, C, P) {
  
  # selecionamos os dados do país desejado
  pais <- c(BigMacData[BigMacData$isoA3 == P, ])
  
  # Transformando a variável pais em um objeto data.frame
  pais <- print(as.data.frame(do.call(cbind, pais)))
  
  # Vamos transformar a variável localPrice para o tipo numérico
  pais$localPrice <- as.numeric(pais$localPrice)
  
  # Somando o valor da variável localPrice
  soma <- 0
  for (i in pais$localPrice) {
    soma <- soma + i
  }
  
  N <- length(pais$localPrice)
  
  
  media <- soma / N
  
  # Encontrando as diferenças Xi - XMédia
  pais['Diferença'] <- pais$localPrice - media
  pais['(Xi - Xmedia)²'] <- pais$Diferença ^ 2
  
  soma.diferenca <- 0
  for (i in pais$`(Xi - Xmedia)²`) {
    soma.diferenca <- soma.diferenca + i 
  }
  
  variancia.amostral <- soma.diferenca / (N - 1)
  
  
  return(paste("A Variância Amostral do preço Local é: ", 
					round(variancia.amostral, 4)))
  
}

# retorna o valor da função varColunaPais
varColunaPais(BigMacData, BigMacData$localPrice , "CHL")


##


## E5 Faça uma função criaVetorBMI que recebe um dataframe D como parâmetro
## e retorna um vetor com índice Big Mac de cada coleta do valor do Big Mac.

criaVetorBMI <- function(D, Pais, A) {
  
  
  # Criando um data.frame com os dados apenas dos Estados Unidos
  vetor.local.USA <- c(BigMacData[BigMacData$isoA3 == "USA", ])
  # Transformando a variável vetor.local.USA (lista) em um objeto data.frame
  vetor.local.USA <- print(as.data.frame(do.call(cbind, vetor.local.USA)))
  
  # Criando um data.frame com os dados apenas do País desejado
  vetor.local.Pais <- c(BigMacData[BigMacData$isoA3 == Pais, ])
  # Transformando a variável vetor.local.BRA (lista) em um objeto data.frame
  vetor.local.Pais <- print(as.data.frame(do.call(cbind, vetor.local.Pais)))
  
  
  ## Mostra o tipo dos dados, ao filtrarmos todos ficam no formato chr. Portanto precisamos alterar
  ### o tipo do dado encontrado para numérico
  
  vetor.local.USA$localPrice <- as.numeric(vetor.local.USA$localPrice)
  vetor.local.Pais$localPrice <- as.numeric(vetor.local.Pais$localPrice)
  
  
  # selecionamos os dados do o preço do ano desejado - USA
  LocalPrice.USA <- c(vetor.local.USA[vetor.local.USA$`criaVetorAno()` == A, ])
  LocalPrice.USA$localPrice
  
  # selecionamos os dados do o preço do ano desejado - BRA
  LocalPrice.Pais <- c(vetor.local.Pais[vetor.local.Pais$`criaVetorAno()` == A, ])
  length(LocalPrice.Pais$localPrice)
  
  ## Tratando os valores duplos para um ano específico e retirando o valor médio
  ### Caso tenhamos mais de um valor para cada ano, utilizamos a média desse ano
  # LocalPrice.Pais$localPrice[1] : Primeiro valor
  # LocalPrice.Pais$localPrice[2] : Segundo valor  
  if ( length(LocalPrice.Pais$localPrice) > 1 ) {
    LocalPrice.Pais$localPrice <- ( LocalPrice.Pais$localPrice[1] + LocalPrice.Pais$localPrice[2] ) / 2
  } 
  
  # Calculo do Indice Big Mac
  indice.IBM <- LocalPrice.Pais$localPrice / LocalPrice.USA$localPrice
  
  ## valorização / desvalorização do real 
  indice.IBM.valorizacao <- ( indice.IBM - LocalPrice.USA$localPrice ) / LocalPrice.USA$localPrice  
  
  
  # Retorno da Função
  return(  paste("O Indice Big Mac e a valorização da moeda' é respectivamente :",  
                 list(round(indice.IBM, 3)   , round(indice.IBM.valorizacao, 4))    ))
  
}

# retornando o valor da função criaVetorBMI
criaVetorBMI(BigMacData, "BRA" , 2020)
### BRASIL, 2020 :
## IBM : 4.232
## Valorização : -0.1219 ou 12.19% de desvalorização


## Agregando os valores ao data.frame
BigMacData <- cbind(BigMacData, criaVetorBMI(BigMacData, "BRA", 2020))
View(BigMacData)


#################### ÍNDICE BIG MAC DOS PAÍSES PARA O ANO DE 2020 ####################

#           IBM         Valorização     ANO
Paises

# ARE      3.060       -0.3651         2020  
# ARG      43.67        8.0610         2020
# AUS      1.349       -0.7202         2020
# AUT      0.799       -0.8343         2020
# AZE      0.820       -0.8300         2020        
# BEL      0.897       -0.8138         2020
# BHR      0.290       -0.9397         2020
# BRA      4.232       -0.1219         2020
# CAN      1.416       -0.7062         2020
# CHE      1.349       -0.7202         2020
# CHL      558.091      114.79         2020
# CHN      4.481       -0.0700         2020
# COL      2468.88      511.21         2020
# CRI      487.55       100.12         2020
# CZE      18.05        2.7448         2020
# DEU      0.891       -0.8151         2020
# DNK      6.224        0.2913         2020
# EGY      8.714        0.8078         2020
# ESP      0.861       -0.8214         2020
# EST      0.664       -0.8623         2020
# FIN      1.001       -0.7910         2020
# FRA      0.871       -0.8192         2020
# GBR      0.703       -0.8541         2020  
# GRC      0.685       -0.858          2020
# GTM      5.187        0.0761         2020
# HKG      4.253       -0.1176         2020
# HND      18.05        2.7448         2020
# HRV      4.564       -0.0530         2020
# HUN      186.72       37.739         2020
# IDN      6950.21      1440.95        2020  
# IND      39.212       7.1352         2020
# IRL      0.882       -0.8171         2020
# ISR      3.527       -0.2683         2020
# ITA      0.892       -0.8149         2020
# JOR      0.477       -0.901          2020
# JPN      80.913       15.7869        2020
# KOR      933.61       192.695        2020
# KWT      0.233       -0.9516         2020
# LBN      1607.88      332.586        2020
# LKA      130.70       26.1173        2020
# LTU      0.591       -0.8773         2020 
# LVA      0.591       -0.8773         2020  
# MDA      9.544        0.980          2020
# MEX      10.373       1.1522         2020
# MYS      2.022       -0.5805         2020  
# NIC      24.896       4.1652         2020
# NLD      0.849       -0.824          2020
# NOR      10.892       1.2598         2020
# NZL      1.359       -0.7181         2020
# OMN      0.234       -0.9515         2020
# PAK     110.996       22.028         2020
# PER      2.469       -0.4878         2020
# PHL      29.461       5.1122         2020
# POL      2.282       -0.5265         2020
# PRT      0.695       -0.8558         2020
# QAT      2.697       -0.4404         2020
# ROU      2.012       -0.5825         2020
# RUS      28.01        4.8109         2020
# SAU      2.801       -0.4189         2020
# SGP      1.224       -0.746          2020
# SVK      0.726       -0.8493         2020 
# SVN      0.508       -0.8945         2020
# SWE      10.80        1.2404         2020
# THA      25.21        4.2298         2020
# TUR      2.799       -0.4193         2020
# TWN      14.94        2.0991         2020
# UKR      12.03        1.4965         2020
# URY      38.17        6.920          2020
# USA      1.000       -0.7925         2020
# VNM      13692.95     2839.86        2020
# ZAF      6.432        0.3343         2020


## Criando o Vetor com os valores do IBM de cada pais 
vetor.paisIBM <- c(3.060, 43.67, 1.349, 0.799, 0.820, 0.897, 0.290, 4.232, 1.416, 1.349,
                   558.091, 4.481, 2468.88, 487.55, 18.05, 0.891, 6.224, 8.714, 0.861,
                   0.664, 1.001, 0.871, 0.703, 0.685, 5.187, 4.253, 18.05, 4.564,186.72,
                   6950.21, 39.212, 0.882, 3.527, 0.892, 0.477, 80.913, 933.61, 0.233, 1607.88,
                   130.70, 0.591, 0.591, 9.544, 10.373, 2.022, 24.896, 0.849, 10.892, 1.359,
                   0.234, 110.996, 2.469, 29.461, 2.282, 0.695, 2.697, 2.012, 28.01, 
                   2.801, 1.224, 0.726, 0.508, 10.80, 25.21, 2.799, 14.94, 12.03, 38.17, 1.000,
                   13692.95, 6.432)

length(vetor.paisIBM) ## 71 registros, assim como a variável Paises


## Dados de um país Específico BRA, CAN, USA e CHL

ano <- c(2000, 2005, 2008, 2010, 2012, 2015, 2018, 2020, 2022)
vetorIBM.BRA <- c(1.317, 2.287, 2.336, 2.430, 2.633, 3.147, 3.615, 4.232, 4.499)
vetorIBM.CAN <- c(1.272, 1.271, 1.274, 1.191, 1.276, 1.346, 1.429, 1.416, 1.330)
vetorIBM.USA <- c(1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000)
vetorIBM.CHL <- c(582.5, 581.39, 482.87, 137.31, 531.19, 489.51, 567.10, 558.19, 638.51)


##


# E8. Faça um gráfico do índice do Big Mac médio dos países (eixo Y). No eixo X os Paises
## de acordo com a variável Paises. Faça mais dois gráficos do índice Big Mac médio usando 
## os vetores criados no ítem anterior no eixo X. Explique  que você vê nos gráficos no seu 
## relatório


## Scatterplots : Comparando o Índice Big Mac do Brasil com outros países 
plot(vetorIBM.BRA, vetorIBM.CAN, main = "Brasil vs Canadá")
lines(lowess(vetorIBM.BRA, vetorIBM.CAN), col = 2)

plot(vetorIBM.BRA, vetorIBM.USA, main = "Brasil vs USA")
lines(lowess(vetorIBM.BRA, vetorIBM.USA), col = 2)

plot(vetorIBM.BRA, vetorIBM.CHL, main = "Brasil vs Chile")
lines(lowess(vetorIBM.BRA, vetorIBM.CHL), col = 2)

## IBM POR ANO
## BRASIL
ano <- c(2000, 2005, 2008, 2010, 2012, 2015, 2018, 2020, 2022)
vetorIBM.BRA <- c(1.317, 2.287, 2.336, 2.430, 2.633, 3.147, 3.615, 4.232, 4.499)


plot(ano, vetorIBM.BRA, main = "IBM do Brasil ao longo dos anos")
lines(lowess(ano, vetorIBM.BRA), col = 2)

# O Índice Big Mac do Brasil ao longo de 20 anos sofreu muitas variações. Aumetando considera-
# velmente o preço do lanche no Brasil. Podemos observar que há uma crescente em relação a esse produto
# chegando a uma desvalorização do Real em 12.19%

## CHILE

vetorIBM.CHL <- c(582.5, 581.39, 482.87, 137.31, 531.19, 489.51, 567.10, 558.19, 638.51)

plot(ano, vetorIBM.CHL, main = "IBM do Chile ao longo dos anos")
lines(lowess(ano, vetorIBM.CHL), col = 2)

# Ao compararmos o Índice Big Mac com outro país da América do Sul (Chile) podemos observar 
# uma valorização de mais de 10000% (~ Valorização 114.79). Entretanto essa valorização da moeda
# chilena é imprecisa quando comparada com o mercado. Pois o Índice Big Mac não considera outras
# variáveis que refletem o real valor da moeda.


# COMPARAÇÃO ENTRE CONTINENTES
#                         ARG    CHL     COL     PER
Paises.America.Sul <- c(16.234, 567.1, 2467.53, 2.273)
#                   ITA    DEU     DNK    RUS
Países.Europa <- c(0.909, 0.868, 6.494, 28.139)

plot(Paises.America.Sul, Países.Europa, 
     main = "IBM entre América do Sul e Europa em 2018")
lines(lowess(Paises.America.Sul, Países.Europa), col = 2)

# Enquanto países da Euopa apresentam um Índice Big Mac mais estável, países da América do Sul
# apresentam uma maior variação entre os valores dos Índices. Isso pode ser explicado talvez pela 
# diferença da moeda de cada país assim como a valorização da mesma dentro do cenário Econômico
# e outros fatores que influenciam e impactam diretamente no cenário econômico local 
# -o que o IBM não demonstra

##


# E9 Faça um gráfico da variância do índice Big Mac dos países (eixo Y ). No eixo X os
## países de acordo com a variância Paises. Faça mais dois gráficos da variância do
## índice Big Mac usando os vetores criados no ítem anterior no eixo X. Explique o
## que você vê nos gráficos no seu relatório

## Valores das Variâncias
## vetorVIBM.BRA <- c(36.400)
## vetorVIBM.CAN <- c(1.858)
## vetorVIBM.USA <- c(0.935)
## vetorVIBM.CHL <- c(321010.36)

# Variância
#                                  BRA      CHL
variancia.Paises.AmericaSul <- c(36.400, 321010.36)
#                                  CAN    USA
variancia.Paises.AmericaNorte <-c(1.858, 0.935)

plot(variancia.Paises.AmericaSul, variancia.Paises.AmericaNorte, 
     main = "Variância América do Norte vs América do Sul")
lines(lowess(variancia.Paises.AmericaSul, variancia.Paises.AmericaNorte), col = 2)

# Ao compararmos os valores da variância dos Países da América do Norte com os Países da 
# América do Sul, observamos que ambos estão em extremos diferentes. 
# Enquanto os Países apresentam uma maior variação do Índice do Big Mac (~>300000), os Países
# da América do Norte parecem não sofrer tanto com a variação do Índice. 
