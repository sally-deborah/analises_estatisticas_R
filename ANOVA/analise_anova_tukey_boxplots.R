# ================================================================
# Script: analise_anova_tukey_boxplots.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva &
#         Eng. de Software Lucas Gelhen Rigon
#
# Descrição: Executa análise de variância (ANOVA) com delineamento em
#             blocos casualizados (DBC) e teste de Tukey, avaliando bandas
#             espectrais e índices de vegetação. Gera boxplots padronizados
#             com legendas estatísticas e exporta resultados.
# Linguagem: R
# Dependências: pacman, dplyr, ggplot2, ExpDes.pt, ggpubr, car, rstatix, lmtest,
#               rio, cowplot, metan, corrplot, fpp, psych
# Data: 2025-10-27
# ================================================================

# ------------------------------------------------------------
# 1. Carregar pacotes
# ------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(
  dplyr, ggplot2, car, rstatix, lmtest, ggpubr, rio, cowplot,
  metan, corrplot, fpp, psych, ExpDes.pt
)

# ------------------------------------------------------------
# 2. Importar dados
# ------------------------------------------------------------
arquivo <- "data/processed/DBC_saligna_micasense_health.csv"
dados <- import(arquivo, header = TRUE, sep = ";", dec = ".")
glimpse(dados)

# ------------------------------------------------------------
# 3. Análise DBC + Tukey (variáveis espectrais)
# ------------------------------------------------------------
bandas <- c("blue", "green", "red", "rededge", "NIR", "NDVI", "NDRE", "GNDVI", "PSRI", "CCCI")

for (banda in bandas) {
  cat("Executando ANOVA + Tukey para:", banda, "\n")
  resultado <- dbc(
    trat = dados$trata,
    bloco = dados$bl,
    resp = dados[[banda]],
    quali = TRUE,
    mcomp = "tukey",
    nl = FALSE,
    hvar = "oneillmathews",
    sigT = 0.05,
    sigF = 0.05
  )
  print(resultado)
}

# ------------------------------------------------------------
# 4. Função para criação e exportação de boxplots
# ------------------------------------------------------------
criar_boxplot <- function(dados, banda, cores, nome_arquivo, label_y, labels_x, descricao) {
  p <- ggplot(dados, aes(x = as.factor(trata), y = !!sym(banda), fill = as.factor(trata))) +
    geom_boxplot(width = 0.2) +
    scale_fill_manual(values = cores) +
    scale_x_discrete(labels = labels_x) +
    labs(x = "Tratamentos", y = label_y, caption = descricao) +
    theme_classic() +
    theme(plot.caption = element_text(hjust = 0.5))
  
  print(p)
  ggsave(file.path("results/anova_boxplots", nome_arquivo), plot = p, width = 6, height = 4, dpi = 300)
}

# Criar diretório de saída
dir_saida <- "results/anova_boxplots"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# ------------------------------------------------------------
# 5. Gerar boxplots com legendas estatísticas
# ------------------------------------------------------------
labels_over <- c("T0", "T1", "T2")

# Exemplo de uso da função (replicável para todas as bandas)
criar_boxplot(
  dados = dados,
  banda = "blue",
  cores = c("0" = "#c3f8b4", "1" = "#5f9550", "2" = "#5f9550"),
  nome_arquivo = "boxplot_blue.png",
  label_y = "Reflectância - Banda Azul",
  labels_x = labels_over,
  descricao = paste(
    "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente\n",
    "Normalidade Shapiro-Wilk (p=0,210); Homogeneidade Oneill-Mathews (p=0,866)"
  )
)

# Repetir para as demais bandas conforme interesse,
# apenas alterando `banda`, `cores`, `label_y` e `descricao`.

cat("Análises e boxplots salvos em:", dir_saida, "\n")
