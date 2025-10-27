# ================================================================
# Script: analise_fatorial.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Realiza análise fatorial utilizando componentes principais
#             como método de extração, com rotação Varimax normalizada.
# Linguagem: R
# Dependências: psych, readr, factoextra, writexl, ggplot2, ggrepel, dplyr
# Data: 2025-10-27
# ================================================================

# Instalar e carregar pacotes necessários
install.packages(c("psych", "readr", "factoextra", "writexl", "ggplot2", "caret", "ggrepel", "dplyr"))
library(psych)
library(readr)
library(factoextra)
library(writexl)
library(ggplot2)
library(caret)
library(ggrepel)
library(dplyr)

# Caminho do arquivo CSV
arquivo <- "seupath/.CSV"

# Importar CSV corrigindo a codificação e delimitador
dados <- read_delim(arquivo, delim = ";", locale = locale(encoding = "ISO-8859-1"), col_types = cols(.default = "c"))

# Converter colunas numéricas corretamente
dados <- dados %>%
  mutate(across(where(is.character), ~ as.numeric(gsub(",", ".", .))))  # Troca ',' por '.' e converte

# Remover valores ausentes
dados <- na.omit(dados)

# Padronizar os dados
padronizados <- scale(dados)

# Calcular a matriz de correlação
cor_matrix <- cor(padronizados, use = "pairwise.complete.obs")

# Determinar os eigenvalues e variância explicada
eigenvalues <- eigen(cor_matrix)$values
total_variance <- (eigenvalues / sum(eigenvalues)) * 100
cumulative_eigen <- cumsum(eigenvalues)
cumulative_variance <- cumsum(total_variance)

# Criar dataframe com todas as informações solicitadas
eigen_df <- data.frame(
  Fator = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  `% Total (variance)` = total_variance,
  `Cumulative (Eigenvalue)` = cumulative_eigen,
  `Cumulative (%)` = cumulative_variance
)

# Determinar o número de fatores pelo critério de Kaiser (Eigenvalue > 1)
num_fatores <- sum(eigenvalues > 1)

# Scree plot manual
ggplot(eigen_df, aes(x = Fator, y = Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Scree Plot", x = "Fator", y = "Eigenvalue") +
  theme_minimal()

# Análise Fatorial com PCA e rotação Varimax Normalized
fa_result <- fa(r = cor_matrix, nfactors = num_fatores, rotate = "varimax", fm = "pc")

# Obter carga fatorial (Factor Loadings)
loadings_df <- as.data.frame(fa_result$loadings[])
loadings_df$Variavel <- rownames(fa_result$loadings)

# Criar dataframe com a contribuição de todas as variáveis em cada fator
contribuicao_variaveis <- as.data.frame(fa_result$loadings)
colnames(contribuicao_variaveis) <- paste0("Fator_", seq_len(ncol(contribuicao_variaveis)))
contribuicao_variaveis$Variavel <- rownames(fa_result$loadings)

# Obter contribuição percentual de cada fator
contrib_fatores <- fa_result$Vaccounted["Proportion Var", ] * 100

# Definir os fatores para o gráfico
fator_x <- 1
fator_y <- 3

# Criar dataframe para o gráfico
loadings_df_plot <- as.data.frame(fa_result$loadings[, c(fator_x, fator_y)])
colnames(loadings_df_plot) <- c("FatorX", "FatorY")
loadings_df_plot$Variavel <- rownames(fa_result$loadings)

# Gerar gráfico de dispersão dos Factor Loadings com percentual nos eixos
grafico <- ggplot(loadings_df_plot, aes(x = FatorX, y = FatorY, label = Variavel)) +
  geom_point(color = "blue", size = 3) +  
  geom_text_repel(size = 4, box.padding = 0.5, max.overlaps = Inf) +  
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_vline(xintercept = 0, linetype = "dashed") +  
  ggtitle(sprintf("Factor Loadings (PCA + Varimax Normalized): Fator %d (%.2f%%) vs Fator %d (%.2f%%)", 
                  fator_x, contrib_fatores[fator_x], 
                  fator_y, contrib_fatores[fator_y])) +
  xlab(sprintf("Fator %d (%.2f%%)", fator_x, contrib_fatores[fator_x])) +
  ylab(sprintf("Fator %d (%.2f%%)", fator_y, contrib_fatores[fator_y])) +
  theme_minimal()

# Exibir o gráfico
print(grafico)

# Criar diretório de saída, se não existir
dir_saida <- "C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\PCA_FATORIAL"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Exportar gráfico
ggsave(file.path(dir_saida, "fatorial_grafico_F1xF3.tiff"), plot = grafico, width = 10, height = 8, dpi = 300)

# Salvar os Eigenvalues e Factor Loadings em planilha Excel
write_xlsx(list(Eigenvalues = eigen_df, FactorLoadings = loadings_df, Contributions = contribuicao_variaveis), 
           file.path(dir_saida, "fatorial_Results.xlsx"))

