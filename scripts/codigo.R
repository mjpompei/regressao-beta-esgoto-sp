# PROJETO: Modelos Lineares Generalizados (Regressão Beta)
# TEMA: Proporção de Domicílios Ligados a Rede Geral de Esgoto em SP (Censo 2022)
# FONTES: IBGE / SIDRA (Tabelas 6803, 9858, 4714, 6805,6892)

# 1. Pacotes -------------------------------------------------------------------
# install.packages(c("sidrar", "tidyverse", "betareg", "car", "corrplot", "lmtest", 
#                    "ResourceSelection", "nortest"))
library(sidrar)
library(tidyverse)
library(betareg)
library(car)
library(corrplot)
library(lmtest)
library(ResourceSelection)
library(nortest)
library(dplyr)

# 2. Importação de Dados -------------------------------------------------------

# A seleção das tabelas necessárias da API foram feitas pelo site:
# https://sidra.ibge.gov.br/acervo

# A lista de identificadores das variáveis foram obtidos nos sites:
# https://sidra.ibge.gov.br/tabela/6803
# https://sidra.ibge.gov.br/tabela/9858
# https://sidra.ibge.gov.br/tabela/4714
# https://sidra.ibge.gov.br/tabela/6805
# https://sidra.ibge.gov.br/tabela/6892

# Vetor da Região Metropolitana de SP
codigos_rmsp <- c(
  "3503901","3505708","3506607","3509007","3509205",
  "3510609","3513009","3513801","3515004","3515103",
  "3515707","3516309","3516408","3518305","3518800",
  "3522208","3522505","3523107","3525003","3526209",
  "3528502","3529401","3530607","3534401","3539103",
  "3539806","3543303","3544103","3545001","3546801",
  "3547304","3547809","3548708","3548807","3549953",
  "3550308","3552502","3552809","3556453"
)

# Y: Esgotamento Sanitário (Tabela 6805)
dados_esgoto <- get_sidra(
  x = 6805, variable = 381, period = "2022", geo = "City",
  geo.filter = list("State" = 35), classific = "c11558", 
  category = list(c(46292, 46290))
)

# X1: Taxa de Alfabetização (Tabela 9858 - API direta por causa do limite de dados)
link_api_9858 <- "/t/9858/n6/in%20n3%2035/v/10835/c125/2932/c301/72053/c1817/72125/c11558/46292/c67/10972/c2661/32776"
dados_desenvolvimento <- get_sidra(api = link_api_9858)

# X2: Densidade Demográfica (Tabela 4714)
dados_demograficos <- get_sidra(
  x = 4714, variable = 614, period = "2022", geo = "City", 
  geo.filter = list("State" = 35)
)

# X3: Abastecimento de Água (Tabela 6803)
dados_agua <- get_sidra(
  x = 6803, variable = 381, period = "2022", geo = "City",
  geo.filter = list("State" = 35), classific = "c1821", 
  category = list(c(72129, 72144))
)

# X4: Coleta de Lixo (Tabela 6892)
dados_lixo <- get_sidra(
  x = 6892, variable = 381, period = "2022", geo = "City",
  geo.filter = list("State" = 35), classific = "c67", 
  category = list(c(10972, 2520))
)

# 3. Manipulação e Limpeza -----------------------------------------------------

tabela_Y <- dados_esgoto %>%
  select(cod_municipio = `Município (Código)`, categoria = `Tipo de esgotamento sanitário`, valor = Valor) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  rename(total = `Total`, esgoto_rede = `Rede geral, rede pluvial ou fossa ligada à rede`) %>%
  mutate(y_proporcao_esgoto = as.numeric(esgoto_rede) / as.numeric(total)) %>%
  select(cod_municipio, y_proporcao_esgoto)

tabela_X1 <- dados_desenvolvimento %>%
  select(cod_municipio = `Município (Código)`, valor = Valor) %>%
  mutate(x1_alfabetizacao = as.numeric(valor) / 100) %>%
  select(cod_municipio, x1_alfabetizacao)

tabela_X2 <- dados_demograficos %>%
  select(cod_municipio = `Município (Código)`, valor = Valor) %>%
  mutate(x2_densidade = as.numeric(valor)) %>%
  select(cod_municipio, x2_densidade)

tabela_X3 <- dados_agua %>%
  select(cod_municipio = `Município (Código)`,categoria = `Existência de ligação à rede geral de distribuição de água e principal forma de abastecimento de água`,valor = Valor
  ) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  rename(
    total = `Total`, 
    agua_rede = `Possui ligação à rede geral e a utiliza como forma principal`
  ) %>%
  mutate(x3_agua = as.numeric(agua_rede) / as.numeric(total)) %>%
  select(cod_municipio, x3_agua)

tabela_X4 <- dados_lixo %>%
  select(cod_municipio = `Município (Código)`, categoria = `Destino do lixo`, valor = Valor) %>%
  pivot_wider(names_from = categoria, values_from = valor) %>%
  rename(total = `Total`, lixo_coletado = `Coletado`) %>%
  mutate(x4_lixo = as.numeric(lixo_coletado) / as.numeric(total)) %>%
  select(cod_municipio, x4_lixo)

# 4. Join e Criação da Variável X5 ---------------------------------------------

dados_completos <- tabela_Y %>%
  inner_join(tabela_X1, by = "cod_municipio") %>%
  inner_join(tabela_X2, by = "cod_municipio") %>%
  inner_join(tabela_X3, by = "cod_municipio") %>%
  inner_join(tabela_X4, by = "cod_municipio") %>%
  mutate(x5_metropolitana = ifelse(cod_municipio %in% codigos_rmsp, 1, 0)) %>%
  drop_na()

rm(codigos_rmsp, dados_agua, link_api_9858, dados_desenvolvimento,
   dados_demograficos, dados_esgoto, dados_lixo, tabela_Y, tabela_X1, 
   tabela_X2, tabela_X3, tabela_X4)

# 5. Análise Exploratória ------------------------------------------------------

# Estatística descritiva
summary(dados_completos)

# Histograma da variável resposta
ggplot(dados_completos, aes(x = y_proporcao_esgoto)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  theme_minimal() +
  labs(
    title = "Distribuição da Proporção de Esgotamento Sanitário",
    x = "Proporção de Domicílios com Esgoto", 
    y = "Frequência de Municípios"
  )


# Dispersão para as variáveis X1 a X4
ggplot(dados_completos, aes(x = x1_alfabetizacao, y = y_proporcao_esgoto)) +
  geom_point(alpha = 0.5, color = "steelblue") + theme_minimal() + 
  labs(x = "Taxa de Alfabetização", y = "Esgoto (Y)")

ggplot(dados_completos, aes(x = log(x2_densidade), y = y_proporcao_esgoto)) +
  geom_point(alpha = 0.5, color = "steelblue") + theme_minimal() + 
  labs(x = "Densidade (log)", y = "Esgoto (Y)")

ggplot(dados_completos, aes(x = x3_agua, y = y_proporcao_esgoto)) +
  geom_point(alpha = 0.5, color = "steelblue") + theme_minimal() + 
  labs(x = "Proporção de Água", y = "Esgoto (Y)")

ggplot(dados_completos, aes(x = x4_lixo, y = y_proporcao_esgoto)) +
  geom_point(alpha = 0.5, color = "steelblue") + theme_minimal() + 
  labs(x = "Proporção de Lixo", y = "Esgoto (Y)")


# Boxplot para a variável Dummy (X5)
ggplot(dados_completos, aes(x = as.factor(x5_metropolitana), y = y_proporcao_esgoto)) +
  geom_boxplot(fill = c("gray80", "tomato")) +
  theme_minimal() + 
  labs(x = "Região (0 = Interior, 1 = RMSP)", y = "Esgoto (Y)")

# Matriz de Correlação (corrplot não usa ggplot, então exportamos com pdf())
cor(dados_completos %>% select(starts_with("y"), starts_with("x")))
colnames(matriz_cor) <- c("Esgoto (Y)", "Alfabetização", "Densidade", "Água", "Lixo", "RMSP")
rownames(matriz_cor) <- colnames(matriz_cor)
corrplot(matriz_cor, method = "number", type = "upper", 
         tl.col = "black", tl.srt = 45, diag = FALSE,
         col = colorRampPalette(c("firebrick3", "white", "navyblue"))(100))

# 6. Seleção de modelo ---------------------------------------------------------

dados_modelagem <- dados_completos %>%
  mutate(log_densidade = log(x2_densidade)) %>%
  mutate(across(
    c(x1_alfabetizacao, log_densidade, x3_agua, x4_lixo, x5_metropolitana),
    ~as.numeric(scale(.))
  ))


m0 <- betareg(y_proporcao_esgoto ~ 1, 
              data = dados_modelagem, type = "BR")
m1 <- betareg(y_proporcao_esgoto ~ x1_alfabetizacao, 
              data = dados_modelagem, type = "BR")
m2 <- betareg(y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade, 
              data = dados_modelagem, type = "BR")
m3 <- betareg(y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + x3_agua, 
              data = dados_modelagem, type = "BR")
m4 <- betareg(y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + x3_agua + x4_lixo,
              data = dados_modelagem, type = "BR")
m5 <- betareg(y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + x3_agua + x4_lixo + 
                x5_metropolitana,
              data = dados_modelagem, type = "BR")

comparacao <- data.frame(
  Modelo = c("M0: Modelo Nulo", "M1: Alfabetização", "M2: + Densidade", 
             "M3: + Água", "M4: + Lixo", "M5: Completo"),
  Variáveis = c(0, 1, 2, 3, 4, 5),
  AIC = c(AIC(m0), AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5)),
  BIC = c(BIC(m0), BIC(m1), BIC(m2), BIC(m3), BIC(m4), BIC(m5)),
  LL = c(round(logLik(m0)[1], 1), round(logLik(m1)[1], 1), 
         round(logLik(m2)[1], 1), round(logLik(m3)[1], 1), 
         round(logLik(m4)[1], 1), round(logLik(m5)[1], 1)),
  Pseudo_R2 = c(
    summary(m0)$pseudo.r.squared, 
    summary(m1)$pseudo.r.squared,
    summary(m2)$pseudo.r.squared,
    summary(m3)$pseudo.r.squared,
    summary(m4)$pseudo.r.squared,
    summary(m5)$pseudo.r.squared
  )
); print(comparacao)

# 7. Modelagem -----------------------------------------------------------------

# Ajuste do modelo (sem variáveis distorcidas pelo scale)
modelo_beta <- betareg(
  y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + x3_agua + 
    x4_lixo + x5_metropolitana,
  data = dados_modelagem
)

# Resultados
summary(modelo_beta)      # sumário
coef(modelo_beta)         # coeficientes
confint(modelo_beta)      # IC coeficientes
exp(coef(modelo_beta))    # Razão de chances
exp(confint(modelo_beta)) # IC Razão de chances
fitted(modelo_beta)       # Valores ajustados

# AIC e BIC
AIC(modelo_beta)
BIC(modelo_beta)

# Probabilidades Estimadas e Predições
predict(modelo_beta, type = "response") 
novo <- data.frame(
  x1_alfabetizacao = c(0.9374, 0.9503, 0.9634),
  log_densidade = c(log(20.21),log(40.07), log(117.50)),
  x3_agua = c(0.8315,0.9063,0.9502),
  x4_lixo = c(0.9444, 0.9714, 0.9885),
  x5_metropolitana = c(0,0,0))
predict(modelo_beta, newdata = novo, type = "response")

# 8. Diagnóstico ---------------------------------------------------------------

# Resíduos
res_dev <- residuals(modelo_beta, type = "deviance")
res_pearson <- residuals(modelo_beta, type = "pearson")
mean(res_dev); min(res_dev); max(res_dev); sd(res_dev)
mean(res_pearson); min(res_pearson); max(res_pearson); sd(res_pearson)

# Gráfico dos resíduos
par(mfrow = c(2,2))
plot(res_dev, pch = 19, main = "Resíduos Deviance", ylab = "Resíduos", xlab = "Índices")
abline(h = 0, col = 2, lwd = 2)
qqnorm(res_dev, main = "QQ-Plot (Deviance)"); qqline(res_dev, col = "red")
plot(res_pearson, pch = 19, main = "Resíduos Pearson", ylab = "Resíduos", xlab = "Índices")
abline(h = 0, col = 2, lwd = 2)
qqnorm(res_pearson, main = "QQ-Plot (Pearson)"); qqline(res_pearson, col = "red") 


# Teste Shapiro Wilk (Normalidade)
shapiro.test(res_dev)
shapiro.test(res_pearson)
lillie.test(res_dev)
lillie.test(res_pearson)
ks.test(res_dev, "pnorm", mean(res_dev), sd(res_dev))
ks.test(res_pearson, "pnorm", mean(res_pearson), sd(res_pearson))

# Multicolinearidade
modelo_lm <- lm(y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + 
                  x3_agua + x4_lixo + x5_metropolitana,
                data = dados_modelagem)
vif(modelo_lm)

# Teste da deviance
modelo_nulo <- betareg(y_proporcao_esgoto ~ 1, data = dados_modelagem)
ll_modelo <- as.numeric(logLik(modelo_beta))
ll_nulo <- as.numeric(logLik(modelo_nulo))
dif_deviance <- 2 * (ll_modelo - ll_nulo)
df_dif <- length(coef(modelo_beta)) - length(coef(modelo_nulo))
p_valor_deviance <- pchisq(dif_deviance, df = df_dif, lower.tail = FALSE)
dif_deviance; p_valor_deviance

# Pseudo R² (múltiplas medidas)
n <- nrow(dados_modelagem)
mcfadden_r2 <- summary(modelo_beta)$pseudo.r.squared; mcfadden_r2
cox_snell_r2 <- 1 - exp((2/n) * (ll_nulo - ll_modelo)); cox_snell_r2

# 9. Observações Influentes ----------------------------------------------------

par(mfrow=c(1,3), cex.main=1.6, cex.lab=1.4, cex.axis=1.2)

# Influência Global
cook <- cooks.distance(modelo_beta)
plot(cook, type = "h", pch = 19, col = "darkgray",
     main = "Distância de Cook (Global)",
     ylab = "Distância", xlab = "Índice dos Municípios")
abline(h = 0.8, col = "red", lty = 2, lwd = 2)
pontos_cook <- which(cook > 0.8)

# Influência Local
n <- nobs(modelo_beta)                           
h_ii <- hatvalues(modelo_beta)                   # Alavancagem (h_ii da matriz H)
r_pi <- residuals(modelo_beta, type = "pearson") # Resíduos de Pearson (r_pi)
# Cálculo da direção de influência
C_i <- h_ii * (r_pi^2)
C_prop <- C_i / sum(C_i)
# Limite 2*C_barra 
C_barra <- mean(C_prop)
corte_local <- 2 * C_barra
plot(C_prop, type = "h", pch = 19, col = "darkgray",
     main = "Influência Local",
     ylab = expression(C[i] / sum(C[j])), xlab = "Índice dos Municípios")
abline(h = corte_local, col = "blue", lty = 2, lwd = 2)
# Municípios que ultrapassam o limite local
pontos_local <- which(C_prop > corte_local)
pontos_local; length(pontos_local); length(pontos_local)/n

# Alavancagem
hat <- hatvalues(modelo_beta)
p <- length(coef(modelo_beta))
corte_alavanca <- 2 * p / n

plot(h_ii, type = "h", pch = 19, col = "darkgray",
     main = "Pontos de Alavanca",
     ylab = "Alavancagem", xlab = "Índice dos Municípios")
abline(h = corte_alavanca, col = "darkgreen", lty = 2, lwd = 2)
pontos_alavanca <- which(h_ii > corte_alavanca)
pontos_alavanca; length(pontos_alavanca); length(pontos_alavanca)/n

# Análise de sensibilidade
pontos_ambos <- unique(c(pontos_local, pontos_alavanca))
dados_sem_influentes <- dados_modelagem[-pontos_ambos, ]
modelo_beta_sens <- betareg(
  y_proporcao_esgoto ~ x1_alfabetizacao + log_densidade + x3_agua + 
    x4_lixo + x5_metropolitana,
  data = dados_sem_influentes, type = "BR")

res_orig <- summary(modelo_beta)$coefficients
res_sens <- summary(modelo_beta_sens)$coefficients

estimativas_orig <- c(res_orig$mean[, "Estimate"], res_orig$precision[, "Estimate"])
p_valores_orig   <- c(res_orig$mean[, "Pr(>|z|)"], res_orig$precision[, "Pr(>|z|)"])

estimativas_sens <- c(res_sens$mean[, "Estimate"], res_sens$precision[, "Estimate"])
p_valores_sens   <- c(res_sens$mean[, "Pr(>|z|)"], res_sens$precision[, "Pr(>|z|)"])

tabela_sensibilidade <- data.frame(
  Parametro = names(estimativas_orig),
  Est_Original = round(estimativas_orig, 4),
  P_val_Orig = round(p_valores_orig, 4),
  Est_Sem_Inf = round(estimativas_sens, 4),
  P_val_Sem_Inf = round(p_valores_sens, 4)
)

# Variação percentual dos coeficientes 
tabela_sensibilidade$Variacao_Perc <- round(((tabela_sensibilidade$Est_Sem_Inf - tabela_sensibilidade$Est_Original) / abs(tabela_sensibilidade$Est_Original)) * 100, 2)
print(tabela_sensibilidade)

# Tabela com os pontos influentes
dados_diag <- dados_completos %>%
  mutate(Indice = row_number())
dados_diag$Influencia_Local <- ifelse(dados_diag$Indice %in% pontos_local, "S", "N")
dados_diag$Alavanca <- ifelse(dados_diag$Indice %in% pontos_alavanca, "S", "N")
tabela_extremos <- dados_diag %>%
  filter(Influencia_Local == "S" | Alavanca == "S") %>%
  select(Indice, cod_municipio, y_proporcao_esgoto, x2_densidade, x4_lixo, 
         x5_metropolitana, Influencia_Local, Alavanca)
print(nrow(tabela_extremos))
print(tabela_extremos)

summary(tabela_extremos)
table(tabela_extremos$x5_metropolitana)