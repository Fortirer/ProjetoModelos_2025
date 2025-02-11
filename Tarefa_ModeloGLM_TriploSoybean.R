getwd()
setwd("C:/Users/LAFIECO/Documents/Doutorado_GDrive/PhD/Soybean_Paper1/DadosModeloGLM") 

# vai mudando o nome do arquivo limpar a cash e apagar as memórias dos objetos
# os arquivos estao somento com um tratamento, por exemplo so com AMb ou Elev, etc
# fazer para cada tratamento a funcao modelo <- glm(Grain ~ TotalBiomass, data = df, family='Gamma'(link='log'))
# depois na interecao rodar com outro script antes
# e ver a interacao 

#df <- read.table("dfAmb2_60dias.csv", sep=";")
df <- read.csv("dfTriple_60dias_SemGraoAbortado.csv", sep=";")# deu problema na leitura, tudo lendo como caracteres entao  vou abrir arquivo com read.table
df <- read.csv("dfElevDrought_60dias.csv", sep=";") 
df <- read.csv("SojaModelagemDrought.csv", sep=";")
df <- read.csv("SojaModelagemTriple.csv", sep=";")
df <- read.csv("SojaModelagemElevDrought.csv", sep=";")

df <- read.csv("SojaModelagemSample.csv", sep=";")  ### database com dados tudo junto por tratamento

df = df[df$Trataments == "Elev/Drought",]
df$Trataments <- as.factor(df$Trataments)
df
str(df)

modelo <- glm(Grain ~ TotalBiomass60Days:Trataments, data = df, family='Gamma'(link='identity')) # ver link inverse temp or log
#modelo <- glm(G_SS ~ TotalBiomass_60dias, data = df, family='Gamma'(link='inverse')) # ver link inverse temp or log

#modelo <- glm(Grain ~ TotalBiomass, data = df, family='gaussian'(link='identity')) # ver link inverse temp
#modelo1 <- glm(G_SS ~ TotalBiomass_60dias, data = df, family='Gamma'(link='log')) 
#modelo <- glm(Starch ~ TratamentsAmb.Drought*TratamentsElev, data = df, family='Gamma'(link='identity')) # ver link inverse temp or log

summary(modelo)
#summary(modelo1)

# calcular os valores preditos
valores_preditos <- predict(modelo, type = "response")
# Calcular os valores observados
valores_observados <- df$Grain #### trocar de acordo com avariavel resposta dependente do modelo

# Calcular o tamanho da amostra
tamanho_amostra <- length(valores_observados)

# VALORES OBSERVADOR
# Calcular a média
media <- mean(valores_observados)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_observados)
desvio_padrao

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media

# VALORES PREDITOS
# Calcular a média
media <- mean(valores_preditos)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_preditos)
desvio_padrao

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media

# Calcular o erro quadrático  RMSE
erro_quadratico <- (valores_observados - valores_preditos) ^ 2

# Calcular a média dos erros quadráticos
media_erro_quadratico <- mean(erro_quadratico)

# Calcular o RMSE
rmse <- sqrt(media_erro_quadratico)
rmse

######################

# MODELO COM INTERACAO

#####################

df <- read.csv("SojaModelagemSample.csv", sep=";")

str(df)
# Selecionar dois tratamentos específicos
df <- subset(df, Trataments %in% c("Elev", "Amb/Temp"))

df

df$Temp_Drought_Interaction <- as.integer(df$Trataments %in% c("Elev", "Amb/Temp"))
df
str(df)
df$Trataments <- as.factor(df$Trataments)
#df$Dro <- as.integer(df$Trataments %in% c('AmbDrought'))
df$Trataments

# Converte categorica coluna para numericaca
for (column in names(df)[sapply(df, is.factor)]) {
  df[[column]] <- as.integer(as.numeric(df[[column]]))
}

modelo <- glm(Grain ~ TotalBiomass125Days:Temp_Drought_Interaction-1, data = df, family='Gamma'(link='identity')) #identity
modelo <- glm(Grain ~ Temp_Drought_Interaction-1, data = df, family='Gamma'(link='identity')) #identity

summary(modelo)

valores_preditos <- predict(modelo, type = "response")
# Calcular os valores observados
valores_observados <- df$Grain

# Calcular o tamanho da amostra
tamanho_amostra <- length(valores_observados)

# VALORES OBSERVADOR
# Calcular a média
media <- mean(valores_observados)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_observados)
desvio_padrao

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media

# VALORES PREDITOS
# Calcular a média
media <- mean(valores_preditos)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_preditos)

desvio_padrao

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media

# Calcular o erro quadrático  RMSE
erro_quadratico <- (valores_observados - valores_preditos) ^ 2

# Calcular a média dos erros quadráticos
media_erro_quadratico <- mean(erro_quadratico)

# Calcular o RMSE
rmse <- sqrt(media_erro_quadratico)
rmse




