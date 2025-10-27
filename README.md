# 🧠 Análises Estatísticas em R

Repositório de scripts utilizados nas análises estatísticas da tese de doutorado de **Eng. Florestal MSc. Sally Deborah Pereira da Silva (PPGEF–UFSM)**.  
Os códigos estão organizados em quatro grandes grupos temáticos, contemplando abordagens **multivariadas**, **estatísticas descritivas**, **testes de hipótese (ANOVA e não-paramétricos)** e **modelos de regressão**.
---

## 📁 Estrutura de Pastas
analises_estatisticas_R/
├── scripts/

│├── multivariada/

│├── estatistica_descritiva/

│├── ANOVA/

│├──  regressao

---
## 🔹 1. Multivariada (`scripts/multivariada/`)

Scripts voltados à análise exploratória e de interdependência entre variáveis.

### `analise_fatorial.R`
- **Objetivo:** Reduzir variáveis originais em fatores principais via PCA com rotação *Varimax Normalized*.  
- **Saídas:** Scree Plot, cargas fatoriais, variância explicada e planilha `.xlsx` com autovalores e contribuições.
### `analise_componentes_principais.R`
- **Objetivo:** Executar ACP com variáveis suplementares e análise de contribuição.  
- **Saídas:** Autovalores, contribuições, correlações e gráfico PCA exportado em `.tiff`.
### `Cluster.R`
- **Objetivo:** Agrupamento hierárquico (método Ward) e geração de dendrogramas com retângulos de clusters.  
- **Saídas:** `dendrograma.jpg` e planilha Excel com matriz de distâncias e ligações.
### `correlacao_pearson.R`
- **Objetivo:** Gerar mapa de calor de correlações de Pearson entre variáveis numéricas.  
- **Saídas:** `heatmap_correlacao_estressadas.jpeg`.
### `correlacao_spearman.R`
- **Objetivo:** Calcular matriz de correlação de Spearman e p-valores.  
- **Saídas:** Mapa de correlação (`.png`) e planilha `correlacao_spearman.xlsx`.
---

## 🔹 2. Estatística Descritiva (`scripts/estatistica_descritiva/`)

Códigos destinados à sumarização dos dados e testes básicos.

### `estatistica_descritiva.R`
- **Objetivo:** Calcular média, mediana, desvio, mínimo, máximo e coeficiente de variação (CV%).  
- **Extras:** Teste de normalidade (Shapiro-Wilk) e exportação de resultados em `.csv`.

### `estatistica_descritiva_anova_kruskal.R`
- **Objetivo:** Verificar normalidade (Shapiro-Wilk), aplicar ANOVA unidirecional com Tukey ou Kruskal-Wallis com Dunn.  
- **Saídas:** Resultados de normalidade e comparação múltipla exportados em `.csv`.

### `teste_t_f_grupos.R`
- **Objetivo:** Comparar grupos (saudáveis vs estressadas) por variáveis espectrais via testes F e t.  
- **Saídas:** Planilha `testes_t_f_grupos.csv` e gráfico de violino `violinos_grupos_estressadas.jpeg`.

### `Script: t-test ou Mann-Whitney.R`
- **Objetivo:** Comparar nutrientes entre plantas saudáveis e doentes, testando normalidade e homogeneidade, com testes `t` ou Mann-Whitney automáticos.  
- **Saídas:** Gráficos de violino com significância (`violinos_macronutrientes.jpeg`) e planilha de resultados.

---

## 🔹 3. ANOVA (`scripts/ANOVA/`)

Scripts com modelos unifatoriais e testes de comparação múltipla.
### `analise_anova_tukey_boxplots.R`
- **Objetivo:** Executa análise de variância (ANOVA) com delineamento em blocos casualizados (DBC) e teste de Tukey para múltiplas comparações, avaliando bandas espectrais e índices de vegetação.
- **Saídas:**  
  - Resultados da ANOVA impressos no console.  
  - Gráficos individuais salvos em 
---

## 🔹 4. Regressão (`scripts/regressao/`)

Modelos lineares simples para diagnóstico nutricional e espectral.

### `regressao_linear_2variaveis.R`
- **Objetivo:** Ajustar regressões simples com duas variáveis dependentes (ex.: Teor de N e Proteína).  
- **Saídas:** Gráfico `Reg_dose_N.png` e planilha `Reg_dose_N.xlsx`.

### `regressao_linear_4variaveis.R`
- **Objetivo:** Ajustar regressões com quatro índices de vegetação dependentes (CCCI, PSRI, NDRE, GNDVI).  
- **Saídas:** Gráfico `dose_N_IVs.tiff` e planilha `Regdose_N_IVs.xlsx`.

---

## 🧩 Dependências Principais

Todos os scripts utilizam **R ≥ 4.3** com os seguintes pacotes:

| Categoria | Principais pacotes |
|------------|--------------------|
| Manipulação de dados | `tidyverse`, `dplyr`, `readr`, `tidyr` |
| Estatística | `car`, `FSA`, `psych`, `FactoMineR`, `ExpDes.pt` |
| Visualização | `ggplot2`, `ggrepel`, `ggcorrplot`, `reshape2` |
| Exportação | `writexl`, `openxlsx` |

🧾 Licença

Distribuído sob licença MIT. Livre para uso acadêmico e extensão de análises.

📬 Contato

📧 sallydeborah@outlook.com
📧 sally.silva@ufsm.acad.br

🌳 Programa de Pós-Graduação em Engenharia Florestal – PPGEF-UFSM

📍 Santa Maria, RS – Brasil