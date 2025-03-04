#Codigo adaptado por: Sally Deborah P. da Silva, Engª Florestal MsC &
#Lucas Gelhen Rigon, Engº de Software
#
#14/02/2024

# Passo 1: Carregar os pacotes que serao usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr, rio, cowplot, metan, corrplot, fpp, psych)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)

###ABRIR ARQUIVO

dados <- import("C:\\Users\\sally\\OneDrive\\Área de Trabalho\\medias_final\\DBC_saligna_micasense_health.CSV", header=TRUE,sep=";",dec=".")## Carregando arquivo de dados csv (Pacote RIO)
#dados <- rio::import("dados_saligna.CSV", na = "NA")
View(dados)     # Visualiza??o dos dados em janela separada
glimpse(dados)  # Visualiza??o de um resumo dos dados

if(!require(ExpDes.pt)) install.packages("ExpDes.pt")

#ativar pacote ExpDes.pt

library(ExpDes.pt)

############ FATORIAL ###############

#Rodar o fatorial com o teste de Tukey 

# Rodar o fatorial com o teste de Tukey para a variável "blue"
resultados_blue <- dbc(
  dados$trata,
  dados$bl,
  dados$blue,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#GREEN

dbc(
  dados$trata,
  dados$bl,
  dados$green,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#RED

dbc(
  dados$trata,
  dados$bl,
  dados$red,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#REDEDGE

dbc(
  dados$trata,
  dados$bl,
  dados$rededge,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#NIR

dbc(
  dados$trata,
  dados$bl,
  dados$NIR,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#NDVI

dbc(
  dados$trata,
  dados$bl,
  dados$NDVI,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#NDRE

dbc(
  dados$trata,
  dados$bl,
  dados$NDRE,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#GNDVI

dbc(
  dados$trata,
  dados$bl,
  dados$GNDVI,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#PSRI

dbc(
  dados$trata,
  dados$bl,
  dados$PSRI,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#CCCI

dbc(
  dados$trata,
  dados$bl,
  dados$CCCI,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


# Carregar pacotes necessários para criar os boxplot dos resultados
library(ggplot2)

# Definir função para criar e salvar o boxplot
criar_boxplot <- function(dados, banda, cores_tratamentos_hex, nome_arquivo,file_path,label_y, labels_over_ride, description) {
  # Criar boxplot para a variável especificada (banda) com base nos tratamentos e cores diferenciadas
  boxplot_plot <- ggplot(data = dados, aes(x = as.factor(trata), y = !!sym(banda), fill = as.factor(trata))) +
    geom_boxplot(width = 0.2) + # Ajuste a largura da caixa aqui
    scale_fill_manual(values = cores_tratamentos_hex) + # Atribuir cores aos tratamentos em hexadecimal
    labs(title = "",
         x = "Tratamentos",
         y = label_y) +
    theme_classic()+
    scale_x_discrete(labels = labels_over_ride)+
    labs(caption = description)+
    theme(plot.caption = element_text(hjust = 0.5))
  
  # Mostrar o boxplot
  print(boxplot_plot)
  
  # Salvar o gráfico como um arquivo PNG na pasta especificada
  ggsave(filename = nome_arquivo, plot = boxplot_plot, path = file_path)
}

labels_over<-c("T0", "T1", "T2")

# Definir paleta de cores em hexadecimal para as bandas e tratamentos 
cores_tratamentos_hex_blue <- c("0" = "#c3f8b4", "1" = "#5f9550", "2" = "#5f9550") # Adicione mais cores conforme necessário
criar_boxplot(
  dados = dados, 
  banda = "blue", 
  cores_tratamentos_hex = cores_tratamentos_hex_blue, 
  nome_arquivo = "boxplot_blue.png", 
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health","Reflectância banda azul",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
  Normalidade Shapiro-Wilk (p<0,05):(p=0,210);
  Homogeneidade de Variâncias oneillmathews:(p=0,866)"
)

cores_tratamentos_hex_green <- c("0" = "#c3f8b4", "1" = "#c3f8b4", "2" = "#5f9550")
criar_boxplot(
  dados = dados, 
  banda = "green", 
  cores_tratamentos_hex = cores_tratamentos_hex_green, 
  nome_arquivo = "boxplot_green.png", 
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health","Reflectância banda verde",
  labels_over,
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05):(p= 0,420);
Homogeneidade de Variâncias oneillmathews:(p=0,919)"
  )

cores_tratamentos_hex_red <- c("0" = "#486d03", "1" = "#c3f8b4", "2" = "#5f9550")
criar_boxplot(
  dados = dados, 
  banda = "red", 
  cores_tratamentos_hex = cores_tratamentos_hex_red, 
  nome_arquivo = "boxplot_red.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância banda vermelha",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05):(p= 0,356);
Homogeneidade de Variâncias oneillmathews:(p=0,82)"

  )

cores_tratamentos_hex_rededge <- c("0" = "#c3f8b4", "1" = "#c3f8b4", "2" = "#5f9550")
criar_boxplot(
  dados = dados, 
  banda = "rededge", cores_tratamentos_hex = cores_tratamentos_hex_rededge, 
  nome_arquivo = "boxplot_rededge.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância borda do vermelho",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05):(p= 0,007);
Homogeneidade de Variâncias oneillmathews:(p=0,970)"
  )

cores_tratamentos_hex_NIR <- c("0" = "#26402d", "1" = "#26402d", "2" = "#26402d")
criar_boxplot(
  dados = dados, 
  banda = "NIR", 
  cores_tratamentos_hex = cores_tratamentos_hex_NIR, 
  nome_arquivo = "boxplot_NIR.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância Infravermelho próximo",
  labels_over, 
  "teste F: as médias não podem ser consideradas diferentes 
Normalidade Shapiro-Wilk (p<0,05):(p= 1.0E-08);
Homogeneidade de Variâncias oneillmathews:(p=0.008)"
  )

# Definir paleta de cores em hexadecimal para os IVs e tratamentos 

cores_tratamentos_hex_NDVI <- c("0" = "#26402d", "1" = "#26402d", "2" = "#26402d")
criar_boxplot(
  dados = dados, 
  banda = "NDVI", 
  cores_tratamentos_hex = cores_tratamentos_hex_NDVI, 
  nome_arquivo = "boxplot_NDVI.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância NDVI",
  labels_over, 
  "teste F: as médias não podem ser consideradas diferentes 
Normalidade Shapiro-Wilk (p<0,05):(p= 2.8E-05);
Homogeneidade de Variâncias oneillmathews:(p=0.356)"
  )

cores_tratamentos_hex_NDRE <- c("0" = "#5f9550", "1" = "#5f9550", "2" = "#c3f8b4")
criar_boxplot(
  dados = dados, 
  banda = "NDRE", 
  cores_tratamentos_hex = cores_tratamentos_hex_NDRE, 
  nome_arquivo = "boxplot_NDRE.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância NDRE",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05):(p= 0.157);
Homogeneidade de Variâncias oneillmathews:(p=0.954)"
  )

cores_tratamentos_hex_GNDVI <- c("0" = "#5f9550", "1" = "#5f9550", "2" = "#c3f8b4")
criar_boxplot(
  dados = dados, 
  banda = "GNDVI", 
  cores_tratamentos_hex = cores_tratamentos_hex_GNDVI, 
  nome_arquivo = "boxplot_GNDVI.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância GNDVI",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05):(p= 0.051);
Homogeneidade de Variâncias oneillmathews:(p=0.999)"
  )

cores_tratamentos_hex_PSRI <- c("0" = "#5f9550", "1" = "#5f9550", "2" = "#c3f8b4")
criar_boxplot(
  dados = dados, 
  banda = "PSRI", 
  cores_tratamentos_hex = cores_tratamentos_hex_PSRI, 
  nome_arquivo = "boxplot_PSRI.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância PSRI",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05): (p= 0.093);
Homogeneidade de Variâncias oneillmathews: (p=1)"
  )

cores_tratamentos_hex_CCCI <- c("0" = "#5f9550", "1" = "#5f9550", "2" = "#c3f8b4")
criar_boxplot(
  dados = dados, 
  banda = "CCCI", 
  cores_tratamentos_hex = cores_tratamentos_hex_CCCI, 
  nome_arquivo = "boxplot_CCCI.png",
  "C:/Users/sally/OneDrive/Área de Trabalho/medias_final/boxplot_sg_health", "Reflectância CCCI",
  labels_over, 
  "Teste de Tukey (p<0,05): Médias seguidas pelas mesmas letras não diferem estatisticamente 
Normalidade Shapiro-Wilk (p<0,05): (p= 0.247);
Homogeneidade de Variâncias oneillmathews: (p=0.936)"
  )

