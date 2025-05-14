setwd("C:/Users/LAFIECO/Documents/Doutorado_GDrive/PhD/Soybean_Paper1/DadosModeloGLM") 

df <- read.csv("SojaModelagemSample.csv", sep=";") # dados com temp abortado

df = df[df$Trataments == "Amb",]
df$Trataments <- as.factor(df$Trataments)
str(df)

modelo <- glm(Grain ~ TotalBiomass60Days, data = df, family='Gamma'(link='identity')) # ver link inverse temp or log # inverse
summary(modelo)

# calcular os valores preditos
valores_preditos <- predict(modelo, type = "response")
# Calcular os valores observados
valores_observados <- df$G_SS #### trocar de acordo com avariavel resposta dependente do modelo
mean(valores_observados)
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

# Calcular o R² manualmente
SSE <- sum((valores_observados - valores_preditos)^2)  # Soma dos erros quadráticos (residuais)

SST <- sum((valores_observados - mean(valores_observados))^2)  # Soma total dos quadrados

R2 <- 1 - (SSE / SST)
R2

######################

# MODELO COM INTERACAO

#####################

df <- read.csv("SojaModelagemSample.csv", sep=";")

str(df)
# Selecionar dois tratamentos específicos
df <- subset(df, Trataments %in% c("ElevDrought", "ElevTemp"))



df$Temp_Drought_Interaction <- as.integer(df$Trataments %in% c("ElevDrought", "ElevTemp"))
df
str(df)
df$Trataments <- as.factor(df$Trataments)
#df$Dro <- as.integer(df$Trataments %in% c('AmbDrought'))
df$Trataments

# Converte categorica coluna para numericaca
for (column in names(df)[sapply(df, is.factor)]) {
  df[[column]] <- as.integer(as.numeric(df[[column]]))
}

modelo1 <- glm(G_Starch ~ TotalBiomass60Days:Temp_Drought_Interaction, data = df, family='gaussian'(link='identity')) #identity
#modelo <- glm(Grain ~ Temp_Drought_Interaction-1, data = df, family='Gamma'(link='identity')) #identity
modelo <- glm(Grain ~ Temp_Drought_Interaction, data = df) #identity
modelo <- glm(Grain ~ Temp_Drought_Interaction, data = df, family='gaussian'(link='identity'))











