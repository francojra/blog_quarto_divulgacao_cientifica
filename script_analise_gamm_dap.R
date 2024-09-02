# Dados bióticos ---------------------------------------------------------------------------------------------------------------------------

# Ambiente arbóreo -------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse) # Manipular dados
library(mgcv) # Gerar análises GAMM
library(sjPlot) # Gerar tabela para publicação dos resultados
library(performance) # Calcular R² e colinearidade

update.packages(c("tidyverse", "mgcv", "sjPlot", "performance"))

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados_dap <- readxl::read_xlsx("dados_dap_vs_biotico.xlsx")

dados_dap <- dados_dap |>
  dplyr::select(-tempo, -trimestre)

view(dados_dap)
glimpse(dados_dap)

dados2 <- readxl::read_xlsx("dados_abioticos_arvores.xlsx")
view(dados2)
glimpse(dados2)

dados2 <- dados2 |>
  dplyr::select(tempo, luminosidade, ph, temperatura) |>
  filter(tempo %in% c(5, 6, 7, 8, 9, 10, 11))
view(dados2)

## Unir tabelas de dados bióticos e abióticos

dados_eco <- bind_cols(dados_dap, dados2)
view(dados_eco)
glimpse(dados_eco)

dados_eco$parcelas <- as.factor(dados_eco$parcelas)

# Análise GAMM -----------------------------------------------------------------------------------------------------------------------------

## Define os parâmetros de controle

controle <- lmeControl(opt = "optim", 
                       msMaxIter = 1000, 
                       tolPwrss = 1e-6)

mod_gamm_dap <- gamm(n_plantulas ~ 
                     s(DAP),
                    control = controle,
                    random = list(parcelas = ~1, tempo = ~1), 
                    data = dados_eco)

mod_gamm_dap1 <- gamm(altura ~ 
                     s(DAP),
                    control = controle,
                    random = list(parcelas = ~1, tempo = ~1), 
                    data = dados_eco)

mod_gamm_dap2 <- gamm(n_ramos ~ 
                     s(DAP),
                    control = controle,
                    random = list(parcelas = ~1, tempo = ~1), 
                    data = dados_eco)

mod_gamm_dap3 <- gamm(das ~ 
                     s(DAP),
                    control = controle,
                    random = list(parcelas = ~1, tempo = ~1), 
                    data = dados_eco)

mod_gamm_dap4 <- gamm(luminosidade ~ 
                     s(DAP),
                    control = controle,
                    random = list(parcelas = ~1, tempo = ~1), 
                    data = dados_eco)

# Verificando o resumo ---------------------------------------------------------------------------------------------------------------------

summary(mod_gamm_dap$gam)  
summary(mod_gamm_dap1$gam) 
summary(mod_gamm_dap2$gam) 
summary(mod_gamm_dap3$gam) 
summary(mod_gamm_dap4$gam)

# Diagnósticos do modelo -------------------------------------------------------------------------------------------------------------------

plot(mod_gamm_dap$gam, residuals = T, pch = 16)

## Extrair Resíduos:
resid_gam <- resid(mod_gamm_dap$gam)
fitted_values <- fitted(mod_gamm_dap$gam)

## Resíduos vs Valores Ajustados:
## Esse gráfico ajuda a identificar heterocedasticidade 
## e padrões nos resíduos.
plot(fitted_values, resid_gam)
abline(h = 0, col = "red")

## Gráfico para o artigo

car::qqPlot(resid(mod_gamm_dap$gam))

## Histograma dos Resíduos:
## Esse gráfico fornece uma visão geral da distribuição dos resíduos.
hist(resid_gam, breaks = 20)

## Verificar Autocorrelação dos Resíduos
## Se seus dados possuem uma estrutura temporal ou 
## espacial, é importante verificar a autocorrelação dos resíduos.
acf(resid_gam)
acf(resid_gam1)

## Verificar pressupostos com o pacote mgcv
gam.check(mod_gamm_dap$gam)
residuals_gam_dap <- residuals(mod_gamm_dap$gam)
plot(residuals_gam_dap)

## Verificar colinearidade de variáveis
check_collinearity(mod_gamm_dap)

# Cálculo do R² ----------------------------------------------------------------------------------------------------------------------------

## Calcula os valores do R² não ajustado e ajustado

r2(mod_gamm_dap)
r2(mod_gamm_dap$gam)
rsq(mod_gamm_dap$gam)

# Tabela dos resultados --------------------------------------------------------------------------------------------------------------------

## Tabela

tab_model(mod_gamm_dap$gam, 
          show.se = T, show.r2 = T,
          show.stat = T)

round(model_performance(mod_gamm_dap), 3)

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

plot_dap_plant <- ggplot(dados_eco, aes(x = DAP, y = n_plantulas)) +
 geom_point(color = "black", size = 4.5) +
  geom_smooth(method = 'lm', formula = 'y ~ x', 
              color = "black", size = 1.5, se = F) +
  labs(y = "Número de plântulas", 
        x = "Diâmetro de árvores (cm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 20,color = "black"),
        axis.text = element_text(color = "black", size = 18),
        legend.position = "none")
plot_dap_plant

## Gráficos para material suplementar

plot_dap_alt <- ggplot(dados_eco, aes(x = DAP, y = altura)) +
 geom_point(color = "black", size = 4.5) +
  geom_smooth(method = 'lm', formula = 'y ~ x', 
              color = "black", size = 1.5, se = F) +
  # labs(x = "Número de plântulas", 
  #      y = "Diâmetro médio do caule (cm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 20,color = "black"),
        axis.text = element_text(color = "black", size = 18),
        legend.position = "none")
plot_dap_alt

plot_dap_ram <- ggplot(dados_eco, aes(x = DAP, y = n_ramos)) +
 geom_point(color = "black", size = 4.5) +
  geom_smooth(method = 'lm', formula = 'y ~ x', 
              color = "black", size = 1.5, se = F) +
  # labs(x = "Número de plântulas", 
  #      y = "Diâmetro médio do caule (cm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 20,color = "black"),
        axis.text = element_text(color = "black", size = 18),
        legend.position = "none")
plot_dap_ram

plot_dap_das <- ggplot(dados_eco, aes(x = DAP, y = das)) +
 geom_point(color = "black", size = 4.5) +
  geom_smooth(method = 'lm', formula = 'y ~ x', 
              color = "black", size = 1.5, se = F) +
  # labs(x = "Número de plântulas", 
  #      y = "Diâmetro médio do caule (cm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 20,color = "black"),
        axis.text = element_text(color = "black", size = 18),
        legend.position = "none")
plot_dap_das
