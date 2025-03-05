#Regressão linear
#Com 4 variaveis dependentes
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

# Definir variável independente e variáveis dependentes
var_indep <- "Dose_N"
vars_dep <- c("CCCI", "PSRI", "NDRE", "GNDVI")  # Pode mudar dinamicamente

# Criar um dicionário opcional para formatar nomes
nomes_formatados <- c()

# Criar uma lista para armazenar os resultados estatísticos
resultados <- list()

# Criar um gráfico inicial com linhas dos eixos X e Y
p <- ggplot(dados, aes_string(x = var_indep)) +
  labs(
    title = "",
    x = "Doses de Nitrogênio",
    y = "Índices de vegetação",
    color = "Variáveis"  # Ajustando legenda do gráfico
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black") +  # Linha no eixo X
  geom_vline(xintercept = 0, color = "black")    # Linha no eixo Y

# Criar um dicionário de cores compatível com nomes formatados ou originais
cores <- c(
  "CCCI" = "#00AFBB", 
  "PSRI" = "#E7B800",
  "NDRE" = "#FC4E07", 
  "GNDVI" = "#B3EE3A"
)

# Loop para ajustar modelos e adicionar ao gráfico
for (i in seq_along(vars_dep)) {
  dep <- vars_dep[i]
  
  # Obter nome formatado, se existir, senão usar o original
  nome_exibicao <- ifelse(dep %in% names(nomes_formatados), nomes_formatados[dep], dep)
  
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
    Variavel_Depend = nome_exibicao,
    Intercepto = b0,
    Coef_Inclinacao = b1,
    R2_Ajustado = r2_ajustado,
    Erro_Padrao = summary(modelo)$sigma,
    SQ_Regressao = anova_tabela[1, "Sum Sq"],
    SQ_Residuos = anova_tabela[2, "Sum Sq"],
    SQ_Total = sum(anova_tabela[, "Sum Sq"])
  )
  
  # Ajustar dinamicamente a posição vertical das equações
  x_pos <- max(dados[[var_indep]]) * 0.8
  y_max <- max(dados[[dep]], na.rm = TRUE)
  y_offset <- (y_max * 0.1) * i  # Multiplicador para deslocar equações
  
  # Adicionar pontos e linhas de regressão ao gráfico
  p <- p + 
    geom_point(aes_string(y = dep, color = shQuote(nome_exibicao))) + 
    geom_smooth(aes_string(y = dep, color = shQuote(nome_exibicao)), method = "lm", se = FALSE) +
    annotate("text", x = x_pos, y = y_max + y_offset,  # Ajuste dinâmico de posição
             label = paste0(nome_exibicao, ":\nY = ", round(b1, 4), "x + ", round(b0, 4), 
                            "\nR² ajustado = ", round(r2_ajustado, 4)),
             hjust = 0, color = cores[[nome_exibicao]], size = 4)
}

# Exibir gráfico
print(p)


# Exportar o gráfico como imagem
ggsave("C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\regressoes\\dose_N_IVs.tiff", plot = p, width = 12, height = 8, dpi = 300)

# Combinar os resultados em um único dataframe
resultados_df <- do.call(rbind, resultados)

# Exportar resultados para Excel
write_xlsx(resultados_df, "C:\\Users\\sally\\OneDrive\\Área de Trabalho\\teste_nutri\\regressoes\\Regdose_N_IVs.xlsx")

print("Arquivo 'Reg_dose_N.xlsx' e gráfico 'Reg_dose_N.png' foram salvos com sucesso!")
