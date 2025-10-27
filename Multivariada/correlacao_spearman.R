# ================================================================
# Script: correlacao_spearman.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Calcula a matriz de correlação de Spearman entre variáveis
#             numéricas, gera gráfico de correlação e exporta resultados.
# Linguagem: R
# Dependências: ggcorrplot, readr, dplyr, openxlsx
# Data: 2025-10-27
# ================================================================

# Carregar pacotes
library(ggcorrplot)
library(readr)
library(dplyr)
library(openxlsx)

# Caminho do CSV
arquivo <- "data/processed/dados_correlacao.csv"

# Importar dados
dados <- read.csv(
  arquivo, header = TRUE, sep = ";", dec = ".", 
  stringsAsFactors = FALSE, check.names = FALSE
)

# Selecionar colunas numéricas
dados_num <- dados %>% select(where(is.numeric))

# Calcular matriz de correlação (Spearman)
cor_spearman <- cor(dados_num, method = "spearman", use = "pairwise.complete.obs")

# Função para calcular matriz de p-valores
cor.mtest <- function(mat, method = "spearman") {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(mat[, i], mat[, j], method = method)
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  return(p.mat)
}

# Calcular matriz de p-valores
p.mat <- cor.mtest(dados_num, method = "spearman")

# Diretório de saída
dir_saida <- "results/correlacao"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Caminhos dos arquivos de saída
arquivo_png <- file.path(dir_saida, "matriz_correlacao_spearman.png")
arquivo_xlsx <- file.path(dir_saida, "correlacao_spearman.xlsx")

# Gráfico de correlação
png(filename = arquivo_png, width = 2400, height = 2400, res = 300)
ggcorrplot(
  cor_spearman,
  method = "circle",
  type = "lower",
  hc.order = TRUE,
  insig = "blank",
  lab = TRUE,
  lab_size = 2,
  tl.cex = 6,
  tl.srt = 45,
  colors = c("#00AFBB", "white", "#E46726")
)
dev.off()

# Exportar matriz de correlação para Excel
write.xlsx(cor_spearman, file = arquivo_xlsx, rowNames = TRUE)

cat("Gráfico salvo em:", arquivo_png, "\n")
cat("Planilha salva em:", arquivo_xlsx, "\n")
