#regressao linear
#Com duas variaveis dependentes
#modifica o nome dos dados de entrada
############ tese de doutorado #Sally Deborah Pereira da Silva ###
#PPGEF-UFSM - 05-03-25



# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(writexl)

# Função para calcular o coeficiente de determinação ajustado
calc_r2_ajustado <- function(modelo) {
  r2 <- summary(modelo)$r.squared
  n <- length(modelo$fitted.values)
  p <- length(modelo$coefficients) - 1
  r2_ajustado <- 1 - ((1 - r2) * (n - 1) / (n - p - 1))
  return(r2_ajustado)
}

# Importar os dados
dados <- read.csv("C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\regressoes\\Efeit_NPK_IVs_2025.csv", 
                  header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE, check.names = FALSE)

# Variável independente
var_indep <- "Dose_N"

# Variáveis dependentes
vars_dep <- c("Teor_de_N", "Proteina_foliar")  # Nomes das colunas no DataFrame

# Criar um mapeamento entre nomes das colunas e nomes desejados no gráfico
nomes_formatados <- c("Teor_de_N" = "Teor de N (%)", "Proteina_foliar" = "Teor de Proteína foliar (%)")

# Criar uma lista para armazenar os resultados estatísticos
resultados <- list()

# Criar um gráfico inicial com linhas dos eixos X e Y
p <- ggplot(dados, aes_string(x = var_indep)) +
  labs(
    title = "",
    x = "Doses de Nitrogênio",
    y = "Variáveis biofísicas",
    color = "Variáveis"  # Legenda ajustada
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +  # Linha no eixo X
  geom_vline(xintercept = 0, color = "black")    # Linha no eixo Y

# Criar uma lista de cores compatível com os nomes desejados
cores <- c("Teor de N (%)" = "#00AFBB", "Teor de Proteína foliar (%)" = "#E7B800") 

# Loop para ajustar modelos e adicionar ao gráfico
for (dep in vars_dep) {
  # Criar fórmula de regressão
  formula_reg <- as.formula(paste0("`", dep, "` ~ `", var_indep, "`"))
  modelo <- lm(formula_reg, data = dados)
  
  # Obter coeficientes e R² ajustado
  b0 <- coef(modelo)[1]
  b1 <- coef(modelo)[2]
  r2_ajustado <- calc_r2_ajustado(modelo)
  
  # Adicionar estatísticas à lista de resultados
  anova_tabela <- anova(modelo)
  resultados[[dep]] <- data.frame(
    Variavel_Depend = nomes_formatados[dep],  # Nome formatado no resultado
    Intercepto = b0,
    Coef_Inclinacao = b1,
    R2_Ajustado = r2_ajustado,
    Erro_Padrao = summary(modelo)$sigma,
    SQ_Regressao = anova_tabela[1, "Sum Sq"],
    SQ_Residuos = anova_tabela[2, "Sum Sq"],
    SQ_Total = sum(anova_tabela[, "Sum Sq"])
  )
  
  # Definir a posição da equação fora da linha de regressão
  x_pos <- max(dados[[var_indep]]) * 0.8
  y_pos <- max(dados[[dep]]) * 1.1  # Ajuste para afastar do gráfico
  
  # Adicionar pontos e linhas de regressão ao gráfico
  p <- p + 
    geom_point(aes_string(y = dep, color = shQuote(nomes_formatados[dep]))) + 
    geom_smooth(aes_string(y = dep, color = shQuote(nomes_formatados[dep])), method = "lm", se = FALSE) +
    annotate("text", x = x_pos, y = y_pos,
             label = paste0(nomes_formatados[dep], ":\nY = ", round(b1, 4), "x + ", round(b0, 4), 
                            "\nR² ajustado = ", round(r2_ajustado, 4)),
             hjust = 0, color = cores[[nomes_formatados[dep]]], size = 4)
}

# Exibir gráfico
print(p)

# Exportar o gráfico como imagem
ggsave("C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\regressoes\\Reg_dose_N.png", plot = p, width = 10, height = 6, dpi = 300)

# Combinar os resultados em um único dataframe
resultados_df <- do.call(rbind, resultados)

# Exportar resultados para Excel
write_xlsx(resultados_df, "C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\regressoes\\Reg_dose_N.xlsx")

print("Arquivo 'Reg_dose_N.xlsx' e gráfico 'Reg_dose_N.png' foram salvos com sucesso!")
