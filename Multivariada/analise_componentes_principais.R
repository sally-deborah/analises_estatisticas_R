# ================================================================
# Script: analise_componentes_principais.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Realiza Análise de Componentes Principais (ACP) com rotação
#             Varimax normalizada, gera autovalores, contribuições e gráficos.
# Linguagem: R
# Dependências: FactoMineR, factoextra, psych, writexl, ggplot2
# Data: 2025-10-27
# ================================================================

# Instalando e carregando os pacotes necessários
if (!require("FactoMineR")) install.packages("FactoMineR")
if (!require("factoextra")) install.packages("factoextra")
if (!require("psych")) install.packages("psych")
if (!require("writexl")) install.packages("writexl")

library(FactoMineR)
library(factoextra)
library(psych)
library(writexl)

# Importando a planilha CSV com os dados
dados <- read.csv("seu_path/dados_PCA.CSV", 
                  header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE, check.names = FALSE)

# Convertendo colunas para numérico
dados[] <- lapply(dados, function(x) as.numeric(as.character(x)))

# Padronizando os dados
dados_padronizados <- scale(dados)

# Definindo variáveis de análise (colunas 4 a 38) e suplementares (colunas 1 a 3)
vars_analise <- as.data.frame(dados_padronizados[, 4:38])
vars_suplementares <- as.data.frame(dados_padronizados[, 1:3])

# Unindo todas as variáveis em um dataframe completo
dados_completos <- cbind(vars_analise, vars_suplementares)

# Realizando a ACP
acp <- PCA(dados_completos, scale.unit = FALSE, ncp = 5, 
           quanti.sup = (ncol(vars_analise) + 1):ncol(dados_completos), graph = FALSE)

# Aplicando a rotação varimax normalizada
acp_rot <- principal(vars_analise, nfactors = 5, rotate = "varimax", scores = TRUE, normalize = TRUE)

# Criando data frame dos autovalores
autovalores <- data.frame(Componente = 1:length(acp$eig[, 1]), 
                          Autovalores = acp$eig[, 1], 
                          Variancia_Explicada = acp$eig[, 2],
                          Cumulativo = cumsum(acp$eig[, 2]))

# Contribuições das variáveis em cada fator
contribuicoes <- as.data.frame(acp_rot$loadings[]^2 * 100)  # Convertendo cargas fatoriais em contribuição percentual
contribuicoes$Variavel <- rownames(acp_rot$loadings)  # Adicionando nomes das variáveis

# Correlações entre todas as variáveis do dataframe
correlacoes <- cor(dados_padronizados, use = "pairwise.complete.obs")

# Exportando os resultados para Excel
write_xlsx(list(
  "Autovalores" = autovalores,
  "Contribuicoes_Variaveis" = contribuicoes,
  "Correlacoes" = as.data.frame(correlacoes)  # Inclui todas as variáveis
), path = "seu_path/PCA_Results.xlsx")

# Plotando o gráfico de dispersão das variáveis de análise após a rotação varimax
plot(acp_rot$loadings, type = "n")
text(acp_rot$loadings, labels = rownames(acp_rot$loadings), col = "black")

# Obtendo as porcentagens de variância explicada pelos dois primeiros componentes principais
var_exp <- acp$eig[, 2]  # Segunda coluna contém a variância explicada
cp1_label <- paste0("CP 1 (", sprintf("%.1f", var_exp[1]), "%)")
cp2_label <- paste0("CP 2 (", sprintf("%.1f", var_exp[2]), "%)")

# Criando o gráfico PCA com os eixos ajustados
p <- fviz_pca_var(acp, col.var = "contrib", 
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                  repel = TRUE, 
                  max.overlaps = 25) +
  labs(x = cp1_label, y = cp2_label)  # Alterando os rótulos dos eixos

# Adicionando o círculo de correlação
p + geom_path(data = circle, aes(x, y), linetype = "dashed", color = "gray50") + 
  coord_fixed()

#salvando o grafico na pasta determinada
ggsave("seu_path/PCA_Variaveis.tiff", 
       plot = p + 
         geom_path(data = circle, aes(x, y), linetype = "dashed", color = "gray50") + 
         coord_fixed(), 
       width = 8, height = 6, dpi = 300, units = "in", device = "tiff")



