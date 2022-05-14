#Investigação sobre Educação per capita
#Com deslocamento no tempo

#Pacotes
pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","knitr",
             "kableExtra","FactoMineR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Importando banco de dados em csv

#Gastos governamentais em relação ao PIB
Gastos_governamentais <- read.csv("total-gov-expenditure-gdp-wdi.csv", 
                                  header = TRUE, 
                                  sep = ",",
                                  dec = ".")

#PIB per capita
PIB_per_capita <- read.csv("gdp-per-capita-worldbank.csv", 
                           header = TRUE, 
                           sep = ",",
                           dec = ".")

#Índice de Gini
Gini <- read.csv("economic-inequality-gini-index.csv", 
                 header = TRUE, 
                 sep = ",",
                 dec = ",")

#Gastos em educação em relação aos gastos totais de governo
Educacao <- read.csv("share-of-education-in-government-expenditure.csv", 
                     header = TRUE, 
                     sep = ",",
                     dec = ".")

#Deslocando em 1 ano o índice de Gini, verificar se os efeitos das variáveis são 
#deslocados no tempo
Gini[,3] <- Gini[,3]-1


#Criando variável para juntar (join) bancos de dados
#União entre "Entity" e "Year", porque Code tem muitos que não tem
Gastos_governamentais$EntYear<-paste(Gastos_governamentais$Entity, Gastos_governamentais$Year)
PIB_per_capita$EntYear<-paste(PIB_per_capita$Entity, PIB_per_capita$Year)
Educacao$EntYear<-paste(Educacao$Entity, Educacao$Year)


#Calculando gastos em educação per capita
# = %gasto gov em educacao * gasto gov % PIB * PIB per capita
Educacao <- inner_join(Educacao, Gastos_governamentais, by="EntYear")
Educacao <- inner_join(Educacao, PIB_per_capita, by="EntYear")
Educacao$Valorpercapita <- Educacao[,4]*0.01*Educacao[,9]*0.01*Educacao[,13]
Educacao[,1:9] <- NULL
Educacao[,4] <- NULL

#Eliminando colunas EntYear por ora
Gastos_governamentais$EntYear<-NULL
PIB_per_capita$EntYear<-NULL
Educacao$EntYear<-NULL


#Calculando aumento e redução das variáveis de interesse.
#Testeifelse: ver se é o mesmo país
Gini$Gini.index_anoant <- 
  c(NA, Gini$Gini.index[1:nrow(Gini)-1])
Gini$Entity_anoant <- 
  c(NA, Gini$Entity[1:nrow(Gini)-1])
Gini$Testeifelse <- ifelse(
  Gini[,1]==Gini[,6], 1, NA)
Gini <- na.exclude(Gini)
Gini$Indice_Gini <- ifelse(
  (Gini[,4]-Gini[,5])^2<0.001, 
  NA, #Se quiser fazer tabela 3x3, colocar "Neutro" no lugar de NA aqui
  ifelse(Gini[,4]>Gini[,5], 
         "Aumento",
         ifelse(Gini[,4]<Gini[,5], 
                "Reducao", NA)
  )
)
Gini$Testeifelse <- NULL
Gini$Gini.index_anoant <- NULL
Gini$Entity_anoant <-  NULL
Gini <- na.exclude(Gini)


Educacao$Valorpercapita_anoant <- 
  c(NA, Educacao$Valorpercapita[1:nrow(Educacao)-1])
Educacao$Entity_anoant <- 
  c(NA, Educacao$Entity[1:nrow(Educacao)-1])
Educacao$Testeifelse <- ifelse(
  Educacao[,1]==Educacao[,6], 1, NA)
Educacao <- na.exclude(Educacao)
Educacao$Valorpercapita_aum_red <- ifelse(
  (Educacao[,4]-Educacao[,5])^2<0.001, 
  NA, #Se quiser fazer tabela 3x3, colocar "Neutro" no lugar de NA aqui
  ifelse(Educacao[,4]>Educacao[,5], 
         "Aumento",
         ifelse(Educacao[,4]<Educacao[,5], 
                "Reducao", NA)
  )
)
Educacao$Testeifelse <- NULL
Educacao$Valorpercapita_anoant <- NULL
Educacao$Entity_anoant <-  NULL
Educacao <- na.exclude(Educacao)

#Criando variável para juntar (join) bancos de dados
#União entre "Entity" e "Year", porque Code tem muitos que não tem
Gini$EntYear<-paste(Gini$Entity, Gini$Year)
Educacao$EntYear<-paste(Educacao$Entity, Educacao$Year)


#Removendo colunas para evitar duplicação nas tabelas Join
Gini$Entity<-NULL
Gini$Code<-NULL
Gini$Year<-NULL


#Juntar banco de dados (inner join para remover dados não constantes)
Gini_Educacao <- inner_join(Gini, Educacao, by="EntYear")

#Removendo colunas EntYear
Gini_Educacao$EntYear<-NULL

#Chi2 crítico para o trabalho
qchisq(p=0.05,df=1,lower.tail = FALSE)

#Matrizes contingencia de valores observados

# Gini_Educacao -----------------------------------------------------------

#Criando uma tabela de contingências
tab <- table(Gini_Educacao$Valorpercapita_aum_red, 
             Gini_Educacao$Indice_Gini)
tab
