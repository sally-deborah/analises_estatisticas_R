# ================================================================
# Script: regressao_linear_quatro_variaveis.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Ajusta modelos de regressão linear simples com quatro
#             variáveis dependentes em função de uma variável independente
#             e exporta os resultados e gráfico consolidado.
# Linguagem: R
# Dependências: ggplot2, dplyr, rlang, tidyr, writexl
# Data: 2025-10-27
# ================================================================

# Carregar pacotes
library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(writexl)

# Função: cálculo do R² ajustado
calc_r2_ajustado <- function(modelo) {
  r2 <- summary(modelo)$r.squared
  n <- length(modelo$fitted.values)
  p <- length(modelo$coefficients) - 1
  r2_ajustado <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))
  return(r2_ajustado)
}

# Caminho dos dados
arquivo <- "data/processed/Efeit_NPK_IVs_2025.csv"

# Importar dados
dados <- read.csv(
  arquivo, header = TRUE, sep = ";", dec = ".",
  stringsAsFactors = FALSE, check.names = FALSE
)

# Variável independente e dependentes
var_indep <- "Dose_N"
vars_dep <- c("CCCI", "PSRI", "NDRE", "GNDVI")

# Nomes formatados (opcional)
nomes_formatados <- c()

# Lista de resultados
resultados <- list()

# Cores para os gráficos
cores <- c(
  "CCCI" = "#00AFBB",
  "PSRI" = "#E7B800",
  "NDRE" = "#FC4E07",
  "GNDVI" = "#B3EE3A"
)

# Gráfico base
p <- ggplot(dados, aes_string(x = var_indep)) +
  labs(
    x = "Doses de Nitrogênio",
    y = "Índices de vegetação",
    color = "Índices"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black")

# Loop de regressões
for (i in seq_along(vars_dep)) {
  dep <- vars_dep[i]
  nome_exibicao <- ifelse(dep %in% names(nomes_formatados), nomes_formatados[dep], dep)
  
  # Fórmula e modelo
  formula_reg <- as.formula(paste0("`", dep, "` ~ `", var_indep, "`"))
  modelo <- lm(formula_reg, data = dados)
  
  # Estatísticas
  b0 <- coef(modelo)[1]
  b1 <- coef(modelo)[2]
  r2_ajustado <- calc_r2_ajustado(modelo)
  anova_tabela <- anova(modelo)
  
  resultados[[dep]] <- data.frame(
    Variavel_Depend = nome_exibicao,
    Intercepto = b0,
    Coef_Inclinacao = b1,
    R2_Ajustado = r2_ajustado,
    Erro_Padrao = summary(modelo)$sigma,
    SQ_Regressao = anova_tabela[1, "Sum Sq"],
    SQ_Residuos = anova_tabela[2, "Sum Sq"],
    SQ_Total = sum(anova_tabela[, "Sum Sq"])
  )
  
  # Posição de anotação
  x_pos <- max(dados[[var_indep]], na.rm = TRUE) * 0.8
  y_max <- max(dados[[dep]], na.rm = TRUE)
  y_offset <- (y_max * 0.1) * i
  
  # Adicionar ao gráfico
  p <- p +
    geom_point(aes_string(y = dep, color = shQuote(nome_exibicao))) +
    geom_smooth(aes_string(y = dep, color = shQuote(nome_exibicao)), method = "lm", se = FALSE) +
    annotate(
      "text", x = x_pos, y = y_max + y_offset,
      label = paste0(
        nome_exibicao, ":\nY = ", round(b1, 4), "x + ", round(b0, 4),
        "\nR² ajustado = ", round(r2_ajustado, 4)
      ),
      hjust = 0, color = cores[[nome_exibicao]], size = 4
    )
}

# Exibir gráfico
print(p)

# Diretório de saída
dir_saida <- "results/regressoes"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Exportar gráfico e planilha
ggsave(file.path(dir_saida, "dose_N_IVs.tiff"), plot = p, width = 12, height = 8, dpi = 300)
write_xlsx(do.call(rbind, resultados), file.path(dir_saida, "Regdose_N_IVs.xlsx"))

cat("Resultados e gráfico exportados para:", dir_saida, "\n")
