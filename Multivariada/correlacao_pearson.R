# ================================================================
# Script: heatmap_correlacao_pearson.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Gera um mapa de calor (heatmap) da matriz de correlação de pearson
#             entre variáveis numéricas, com reordenação hierárquica
#             e valores de correlação exibidos nas células.
# Linguagem: R
# Dependências: reshape2, ggplot2
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes
# ------------------------------------------------------------
library(reshape2)
library(ggplot2)

# ------------------------------------------------------------
# 2. Funções auxiliares
# ------------------------------------------------------------
reorder_cormat <- function(m) {
  d <- as.dist((1 - m) / 2)
  o <- hclust(d)$order
  m[o, o]
}

get_lower_tri <- function(m) {
  m[upper.tri(m)] <- NA
  m
}

get_upper_tri <- function(m) {
  m[lower.tri(m)] <- NA
  m
}

# ------------------------------------------------------------
# 3. Importar dados
# ------------------------------------------------------------
arquivo_csv <- "data/processed/estressadas.csv"

dados <- read.csv(
  arquivo_csv, sep = ";", dec = ".", header = TRUE,
  stringsAsFactors = FALSE, check.names = FALSE
)

# Renomear coluna "clorofila" (case-insensitive)
names(dados)[tolower(names(dados)) == "clorofila"] <- "Teor de clorofila"

# ------------------------------------------------------------
# 4. Calcular matriz de correlação (Pearson)
# ------------------------------------------------------------
num <- dados[sapply(dados, is.numeric)]
m <- cor(num, method = "pearson", use = "pairwise.complete.obs")
m <- reorder_cormat(m)
m_tri <- get_upper_tri(m)

# ------------------------------------------------------------
# 5. Converter para formato longo
# ------------------------------------------------------------
df <- melt(m_tri, na.rm = TRUE)

# ------------------------------------------------------------
# 6. Plotar heatmap
# ------------------------------------------------------------
p <- ggplot(df, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 4) +
  scale_fill_gradient2(
    low = "#fee5d9", mid = "#ffb47b", high = "#ff6e59",
    midpoint = 0, limit = c(-1, 1), name = "Correlação\nPearson"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    plot.margin = margin(5, 5, 20, 5)
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 7,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  coord_fixed()

print(p)

# ------------------------------------------------------------
# 7. Exportar resultado
# ------------------------------------------------------------
dir_saida <- "results/correlacao"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

ggsave(
  file.path(dir_saida, "heatmap_correlacao_estressadas.jpeg"),
  p, width = 6, height = 6, dpi = 300
)

cat("✅ Heatmap de correlação salvo em:", dir_saida, "\n")
