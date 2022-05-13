#Carregando Pacotes
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

#Gastos em proteção social em relação aos gastos totais de governo
Prot_social <- read.csv("share-social-protection-in-government-exp-oecd-2013.csv", 
                 header = TRUE, 
                 sep = ",",
                 dec = ".")

#Gastos em educação em relação aos gastos totais de governo
Educacao <- read.csv("share-of-education-in-government-expenditure.csv", 
                        header = TRUE, 
                        sep = ",",
                        dec = ".")


#Calculando aumento e redução das variáveis de interesse.
#Testeifelse: verifica se é o mesmo país

Gastos_governamentais$PorcentPIB_GastoGov_anoant <- 
  c(NA, Gastos_governamentais[1:nrow(Gastos_governamentais)-1,4])
Gastos_governamentais$Entity_anoant <- 
  c(NA, Gastos_governamentais[1:nrow(Gastos_governamentais)-1,1])
Gastos_governamentais$Testeifelse <- ifelse(
  Gastos_governamentais[,1]==Gastos_governamentais[,6], 1, NA)
Gastos_governamentais <- na.exclude(Gastos_governamentais)
Gastos_governamentais$PorcentPIB_GastoGov <- ifelse(
  (Gastos_governamentais[,4]-Gastos_governamentais[,5])^2<0.001, 
  NA, #Se quiser fazer tabela 3x3, colocar "Neutro" no lugar de NA aqui
  ifelse(Gastos_governamentais[,4]>Gastos_governamentais[,5], 
         "Aumento",
         ifelse(Gastos_governamentais[,4]<Gastos_governamentais[,5], 
                "Reducao", NA)
  )
)
Gastos_governamentais$Testeifelse <- NULL
Gastos_governamentais$PorcentPIB_GastoGov_anoant <- NULL
Gastos_governamentais$Entity_anoant <-  NULL
Gastos_governamentais <- na.exclude(Gastos_governamentais)


Gini$Gini.index_anoant <- 
  c(NA, Gini$Gini.index[1:nrow(Gini)-1])
Gini$Entity_anoant <- 
  c(NA, Gini$Entity[1:nrow(Gini)-1])
Gini$Testeifelse <- ifelse(
  Gini[,1]==Gini[,6], 1, NA)
Gini <- na.exclude(Gini)
Gini$Indice_Gini <- ifelse(
  (Gini[,4]-Gini[,5])^2<0.001, 
  NA, 
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


PIB_per_capita$GDP.per.capita_anoant <- 
  c(NA, PIB_per_capita$GDP.per.capita..PPP..constant.2017.international...[1:nrow(PIB_per_capita)-1])
PIB_per_capita$Entity_anoant <- 
  c(NA, PIB_per_capita$Entity[1:nrow(PIB_per_capita)-1])
PIB_per_capita$Testeifelse <- ifelse(
  PIB_per_capita[,1]==PIB_per_capita[,6], 1, NA)
PIB_per_capita <- na.exclude(PIB_per_capita)
PIB_per_capita$PIB_per_capita <- ifelse(
  (PIB_per_capita[,4]-PIB_per_capita[,5])^2<0.001, 
  NA, 
  ifelse(PIB_per_capita[,4]>PIB_per_capita[,5], 
         "Aumento",
         ifelse(PIB_per_capita[,4]<PIB_per_capita[,5], 
                "Reducao", NA)
  )
)
PIB_per_capita$Testeifelse <- NULL
PIB_per_capita$GDP.per.capita_anoant <- NULL
PIB_per_capita$Entity_anoant <-  NULL
PIB_per_capita <- na.exclude(PIB_per_capita)

Prot_social$Share.of.social.protection_anoant <- 
  c(NA, Prot_social$Share.of.social.protection.in.government.expenditure..OECD..2017..[1:nrow(Prot_social)-1])
Prot_social$Entity_anoant <- 
  c(NA, Prot_social$Entity[1:nrow(Prot_social)-1])
Prot_social$Testeifelse <- ifelse(
  Prot_social[,1]==Prot_social[,6], 1, NA)
Prot_social <- na.exclude(Prot_social)
Prot_social$Porcent_GastosGov_ProtSocial <- ifelse(
  (Prot_social[,4]-Prot_social[,5])^2<0.001, 
  NA, 
  ifelse(Prot_social[,4]>Prot_social[,5], 
         "Aumento",
         ifelse(Prot_social[,4]<Prot_social[,5], 
                "Reducao", NA)
  )
)
Prot_social$Testeifelse <- NULL
Prot_social$Share.of.social.protection_anoant <- NULL
Prot_social$Entity_anoant <-  NULL
Prot_social <- na.exclude(Prot_social)


Educacao$Shareofeducation_anoant <- 
  c(NA, Educacao$Share.of.education.in.government.expenditure....of.government.expenditure...World.Bank..2019..[1:nrow(Educacao)-1])
Educacao$Entity_anoant <- 
  c(NA, Educacao$Entity[1:nrow(Educacao)-1])
Educacao$Testeifelse <- ifelse(
  Educacao[,1]==Educacao[,6], 1, NA)
Educacao <- na.exclude(Educacao)
Educacao$Porcent_GastosGov_Educacao <- ifelse(
  (Educacao[,4]-Educacao[,5])^2<0.001, 
  NA, 
  ifelse(Educacao[,4]>Educacao[,5], 
         "Aumento",
         ifelse(Educacao[,4]<Educacao[,5], 
                "Reducao", NA)
  )
)
Educacao$Testeifelse <- NULL
Educacao$Shareofeducation_anoant <- NULL
Educacao$Entity_anoant <-  NULL
Educacao <- na.exclude(Educacao)


#Criando variável para juntar (join) bancos de dados
#União entre "Entity" e "Year"
Gastos_governamentais$EntYear<-paste(Gastos_governamentais$Entity, Gastos_governamentais$Year)
PIB_per_capita$EntYear<-paste(PIB_per_capita$Entity, PIB_per_capita$Year)
Gini$EntYear<-paste(Gini$Entity, Gini$Year)
Prot_social$EntYear<-paste(Prot_social$Entity, Prot_social$Year)
Educacao$EntYear<-paste(Educacao$Entity, Educacao$Year)

#Removendo colunas para evitar duplicação nas tabelas Join
Gini$Entity<-NULL
Gini$Code<-NULL
Gini$Year<-NULL

#Juntar banco de dados (inner join para remover dados ausentes)
Gini_GovExp <- inner_join(Gini, Gastos_governamentais, by="EntYear")
Gini_PIBperC <- inner_join(Gini, PIB_per_capita, by="EntYear")
Gini_ProtSocial <- inner_join(Gini, Prot_social, by="EntYear")
PIBperC_ProtSocial <- inner_join(PIB_per_capita, Prot_social, by="EntYear")
PIBperC_Educacao <- inner_join(PIB_per_capita, Educacao, by="EntYear")
Gini_Educacao <- inner_join(Gini, Educacao, by="EntYear")
PIB_GovExp <- inner_join(PIB_per_capita, Gastos_governamentais, by="EntYear")

#Removendo colunas EntYear
Gini_GovExp$EntYear<-NULL
Gini_PIBperC$EntYear<-NULL
Gini_ProtSocial$EntYear<-NULL
PIBperC_ProtSocial$EntYear<-NULL
PIBperC_Educacao$EntYear<-NULL
Gini_Educacao$EntYear<-NULL

#Chi2 crítico para o trabalho
qchisq(p=0.05,df=1,lower.tail = FALSE)

#Matrizes contingencia de valores observados


# Gini_PIBperC ------------------------------------------------------------

#Criando uma tabela de contingências
tab <- table(Gini_PIBperC$PIB_per_capita, 
             Gini_PIBperC$Indice_Gini)
tab


# Gini_GovExp -------------------------------------------------------------

#Criando uma tabela de contingências
tab <- table(Gini_GovExp$PorcentPIB_GastoGov, 
             Gini_GovExp$Indice_Gini)
tab


# Gini_ProtSocial ---------------------------------------------------------


#Criando uma tabela de contingências
tab <- table(Gini_ProtSocial$Porcent_GastosGov_ProtSocial, 
             Gini_ProtSocial$Indice_Gini)
tab


# Gini_Educacao -----------------------------------------------------------


#Criando uma tabela de contingências
tab <- table(Gini_Educacao$Porcent_GastosGov_Educacao, 
             Gini_Educacao$Indice_Gini)
tab


