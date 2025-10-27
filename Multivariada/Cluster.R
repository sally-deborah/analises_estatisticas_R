# ================================================================
# Script: analise_cluster.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Realiza análise de agrupamento hierárquico (método Ward),
#             gera dendrograma e exporta matrizes de distância e ligação.
# Linguagem: R
# Dependências: tidyverse, cluster, dendextend, openxlsx
# Data: 2025-10-27
# ================================================================


# Carregar as bibliotecas necessárias
install.packages("openxlsx")

library(tidyverse)
library(cluster)
library(dendextend)
library(openxlsx)  # Biblioteca para exportar para Excel

# Carregar os dados
dados <- read.csv("seu_path/.csv", 
                  header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

# Separar a coluna de identificadores ('trata')
tratamentos <- dados$trata

# Selecionar apenas as variáveis numéricas
dados_numericos <- dados %>% select(-trata)

# Padronizar os dados (z-score)
dados_padronizados <- scale(dados_numericos)

# Calcular a matriz de distância Euclidiana
distancia <- dist(dados_padronizados, method = "euclidean")

# Converter a matriz de distância para um data frame
dist_matrix <- as.matrix(distancia)
rownames(dist_matrix) <- colnames(dist_matrix) <- tratamentos

# Aplicar a técnica de agrupamento hierárquico usando o método de Ward
cluster_ward <- hclust(distancia, method = "ward.D2")

# Extrair as distâncias de ligação
dist_ligacao <- data.frame(Cluster1 = cluster_ward$merge[,1],
                           Cluster2 = cluster_ward$merge[,2],
                           Altura = cluster_ward$height)

# Criar um arquivo Excel com as planilhas
darq <- "seu_path/distancias_cluster.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "Distancia Euclidiana")
addWorksheet(wb, "Distancia Ligacao")
writeData(wb, sheet = "Distancia Euclidiana", dist_matrix, rowNames = TRUE)
writeData(wb, sheet = "Distancia Ligacao", dist_ligacao)
saveWorkbook(wb, darq, overwrite = TRUE)

# Converter o objeto hclust para dendrograma
dend <- as.dendrogram(cluster_ward)

# Adicionar os rótulos dos tratamentos no eixo horizontal
dend <- dendextend::set(dend, "labels", tratamentos[cluster_ward$order])

# Colorir os ramos do dendrograma
dend <- color_branches(dend, k = 3, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# Ajustar a margem inferior para afastar os rótulos
par(mgp = c(2, 1, 0))  # Ajusta a posição dos eixos sem alterar as margens gerais

# Determinar o limite correto para o eixo Y
ylim <- c(0, max(cluster_ward$height) * 1.1) 

# Plotar o dendrograma
plot(dend, main = "Dendrograma - Método Ward", ylim = ylim, 
     xlab = "", ylab = "Distância de ligação", axes = FALSE, cex.lab = 1.2)

# Adicionar o eixo Y
axis(2)  

# Ajustar a posição do título do eixo X para afastá-lo dos rótulos
mtext("Tratamentos", side = 1, line = 5)  # Aumenta a distância da legenda do eixo X

# Adicionar retângulos nos clusters
rect.hclust(cluster_ward, k = 3, border = c("black", "black", "black")) 

# **Salvar o gráfico em um arquivo JPG**
jpeg("seu_path/dengrograma.jpg", width = 12, height = 8, units = "in", res = 300)

# Repetir a plotagem para salvar a imagem
par(mar = c(10, 4, 4, 2))  # Ajuste das margens novamente
plot(dend, main = "Dendrograma - Método Ward", ylim = ylim, 
     xlab = "", ylab = "Distância de ligação", axes = FALSE)
axis(2)
mtext("Tratamentos", side = 1, line = 3)
abline(h = -1, col = "black", lwd = 2)
rect.hclust(cluster_ward, k = 3, border = c("black", "black", "black")) 

# **Fechar o dispositivo gráfico**
dev.off()


cat("Figura ajustada salva como 'dendrograma_ajustado.jpg' no diretório atual.\n")
