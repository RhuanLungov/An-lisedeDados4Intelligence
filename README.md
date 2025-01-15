# An-lisedeDados4Intelligence
Novo Repositório
---
title: "Análise de Dados em R"
author: "Rhuan Rafael Lungov"
date: "2025-01-15"
output: html_document
---
```{r}

```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

setwd("C:/Users/rhuan/OneDrive/Desktop/Análise de Dado - 4Intelligence")
getwd()
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)
diesel <- read_excel("vendas_distribuidoras_anp 1.xlsx", sheet="diesel")
gasolina <- read_excel("vendas_distribuidoras_anp 1.xlsx", sheet="gasolina")
etanol <- read_excel("vendas_distribuidoras_anp 1.xlsx", sheet="etanol")

# Função para transformar os dados
prepare_data <- function(data, fuel_type) {
  data %>%
    pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "value") %>%
    mutate(fuel_type = fuel_type)
}

# Aplicar a função para cada tipo de combustível
diesel_long <- prepare_data(diesel, "diesel")
gasolina_long <- prepare_data(gasolina, "gasolina")
etanol_long <- prepare_data(etanol, "etanol")

# Combinar todos os dados em um único data frame
all_data <- bind_rows(diesel_long, gasolina_long, etanol_long)
head(all_data)

#Sazonalidade dos Combustíveis

all_data %>%
  group_by(meses, fuel_type) %>%
  summarise(media = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = meses, y = media, color = fuel_type)) +
  geom_line() +
  labs(title = "Sazonalidade", x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

#Analisando o consumo médio dos anos, e colocando em um período de meses, nota-se que o diesel se sobressai do outros combustíveis;
#Mesmo com os anos, o diesel continua sendo amplamente consumido, e o etanol tem baixo consumo;
#Os períodos de sazonalidade a serem destacados são: Março,Outubro e Dezembro.

#Gráficos de Sazonalidade - Distrito Federal

all_data %>% 
  filter(regiao == "df")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Distrito Federal",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Goiás

all_data %>% 
  filter(regiao == "go")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Goiás",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Maranhão

all_data %>% 
  filter(regiao == "ma")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Maranhão",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Mato Grosso

all_data %>% 
  filter(regiao == "mt")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Mato Grosso",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Minas Gerais

all_data %>% 
  filter(regiao == "mg")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Minas Gerais",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Pará

all_data %>% 
  filter(regiao == "pa")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Pará",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# São Paulo

all_data %>% 
  filter(regiao == "sp")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no São Paulo",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Tocantins

all_data %>% 
  filter(regiao == "to")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Tocantins",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

# Brasil

all_data %>% 
  filter(regiao == "br")%>%
  group_by(meses, fuel_type) %>%
  ggplot(aes(x = meses, y = value, color = fuel_type)) + 
  geom_line() +
  facet_wrap(~ year) +
  labs(title = "Sazonalidade no Brasil",x = "Mês", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

#A comparação da sazonalidade média  com os estados tem adversidades dependendo do estado e do ano a ser analisado;
#De certo, elas não são iguais na maioria das vezes, e não tem os mesmos padrões;
#Alguns meses podem se sobressair, como outubro e março em alguns anos e alguns estados, mas há picos e decaídas que não se repetem;
#Um exemplo é o mês de maio de 2018, que em muitos gráficos aparece em queda;
#Ao mesmo tempo, alguns estados apresentam padrões bem estruturados, principalmente em relação ao diesel;
#Destaca-se o crescimento do consumo de combustível e a ascensão do etanol em alguns estados (MT,GO, MG e SP).


all_data %>%
  group_by(regiao, fuel_type) %>%
  summarise(total = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(x = regiao, y = total, fill = fuel_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição do Consumo de Combustíveis", x = "Região", y = "Consumo", Color = "Tipo de Combustível") +
  theme_minimal()

#No Brasil, grande parte do consumo vai para o Diesel;
#Na comparação dos estados, São Paulo é o estado que mais consome os três combustíveis, e tocatins é o que menos consome;
#Minas Gerais é o 2° com maior consumo, e Goiás o 3°;
#Distrito Federal é um dos menos em consumo, mas seu consumo de gasolina é o 4° maior;
#Uma coincidência, é o fato de Goiás, Minas Gerais e São Paulo estarem no top 3, ao mesmo tempo que são grandes potências agrárias do país.

all_data %>%
  filter(as.numeric(year) >= 2016, as.numeric(year) < 2021) %>%
  group_by(year, fuel_type) %>%
  summarise(total_value = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_value, fill = fuel_type)) +
  geom_bar(stat="identity") +
  labs(title = "Evolução 2016-2020",
       x = "Ano", y = "Consumo",
       color = "Tipo de Combustível") +
  theme_minimal()

#No gráfico de barras, vemos um aumento do consumo até o pico em 2019;
#Mesmo com a pandemia, o ano de 2020 também foi de grande consumo, estando empatado com o ano de 2018;
#O tamanho do mercado, aproximadamente, chegou aos 175.000.000,00 de consumo;
#há previsão de aumento com o fim da pandemia.
