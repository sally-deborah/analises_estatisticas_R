# ================================================================
# Script: regressao_linear_duas_variaveis.R
# Autor: Eng. Florestal MSc. Sally Deborah P. da Silva
#
# Descrição: Ajusta modelos de regressão linear simples com duas variáveis
#             dependentes em função de uma variável independente e exporta
#             estatísticas e gráficos dos resultados.
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
vars_dep <- c("Teor_de_N", "Proteina_foliar")

# Nomes formatados para gráficos e resultados
nomes_formatados <- c(
  "Teor_de_N" = "Teor de N (%)",
  "Proteina_foliar" = "Teor de Proteína foliar (%)"
)

# Lista para armazenar resultados
resultados <- list()

# Cores para os gráficos
cores <- c("Teor de N (%)" = "#00AFBB", "Teor de Proteína foliar (%)" = "#E7B800")

# Gráfico base
p <- ggplot(dados, aes_string(x = var_indep)) +
  labs(
    x = "Doses de Nitrogênio",
    y = "Variáveis biofísicas",
    color = "Variáveis"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black")

# Loop de regressões
for (dep in vars_dep) {
  formula_reg <- as.formula(paste0("`", dep, "` ~ `", var_indep, "`"))
  modelo <- lm(formula_reg, data = dados)
  
  # Coeficientes e estatísticas
  b0 <- coef(modelo)[1]
  b1 <- coef(modelo)[2]
  r2_ajustado <- calc_r2_ajustado(modelo)
  anova_tabela <- anova(modelo)
  
  resultados[[dep]] <- data.frame(
    Variavel_Depend = nomes_formatados[dep],
    Intercepto = b0,
    Coef_Inclinacao = b1,
    R2_Ajustado = r2_ajustado,
    Erro_Padrao = summary(modelo)$sigma,
    SQ_Regressao = anova_tabela[1, "Sum Sq"],
    SQ_Residuos = anova_tabela[2, "Sum Sq"],
    SQ_Total = sum(anova_tabela[, "Sum Sq"])
  )
  
  # Coordenadas de texto da equação
  x_pos <- max(dados[[var_indep]]) * 0.8
  y_pos <- max(dados[[dep]]) * 1.1
  
  # Adicionar pontos e linha ao gráfico
  p <- p +
    geom_point(aes_string(y = dep, color = shQuote(nomes_formatados[dep]))) +
    geom_smooth(aes_string(y = dep, color = shQuote(nomes_formatados[dep])),
                method = "lm", se = FALSE) +
    annotate(
      "text", x = x_pos, y = y_pos,
      label = paste0(
        nomes_formatados[dep], ":\nY = ", round(b1, 4), "x + ", round(b0, 4),
        "\nR² ajustado = ", round(r2_ajustado, 4)
      ),
      hjust = 0, color = cores[[nomes_formatados[dep]]], size = 4
    )
}

# Exibir gráfico
print(p)

# Diretório de saída
dir_saida <- "results/regressoes"
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

# Exportar gráfico
ggsave(
  file.path(dir_saida, "Reg_dose_N.png"),
  plot = p, width = 10, height = 6, dpi = 300
)

# Exportar resultados consolidados
resultados_df <- do.call(rbind, resultados)
write_xlsx(resultados_df, file.path(dir_saida, "Reg_dose_N.xlsx"))

cat("Resultados e gráficos exportados para:", dir_saida, "\n")
