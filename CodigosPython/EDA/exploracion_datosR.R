# EDA VARIABLES MACROECONÓMICAS

#Instalamos paquetes necesarios
install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
install.packages("corrplot")
install.packages("moments")    
install.packages("tseries")

#Cargamos bibliotecas necesarias
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(corrplot)
library(moments)
library(tseries)
çlibrary(zoo)

# Seleccionamos solo las variables macro
dataset_macro <- dataset_final %>%
  select(Date, sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd)

dataset_macro_conActivoRet <- dataset_final %>%
  select(Date, sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd, activo_return)

# ANÁLISIS DESCRIPTIVO -Aqui buscaremos entender las variables del dataset (tipos, na, escala ...)

# Estadísticos descriptivos
summary(dataset_macro)

#Verificar nulos
colSums(is.na(dataset_macro))

# Creamos una tabla para ver de forma más detallada, ciertas metricas de las variables macro
dataset_macro %>%
  select(-Date) %>%
  summarise(across(everything(), 
                   list(media = ~mean(., na.rm = TRUE),
                        mediana = ~median(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE),
                        skewness = ~skewness(., na.rm = TRUE),
                        kurtosis = ~kurtosis(., na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = "estadistico", values_to = "valor") %>%
  print(n = Inf)


# EVOLUCIÓN TEMPORAL

# Fed vs ECB
p1 <- dataset_macro %>%
  select(Date, tipos_fed, tipos_ecb) %>%
  pivot_longer(-Date) %>%
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("tipos_fed" = "darkblue", "tipos_ecb" = "red"),
                     labels = c("Fed", "ECB")) +
  theme_minimal() +
  labs(title = "Evolución de Tipos de Interés",
       subtitle = "Política monetaria comparada",
       x = "Fecha", y = "Tipo de interés %",
       color = "Banco Central")
ggplotly(p1)

# Inflación USA vs Europa
p2 <- dataset_macro %>%
  select(Date, inflacion_usa, inflation_eu) %>%
  pivot_longer(-Date) %>%
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("inflacion_usa" = "darkblue", "inflation_eu" = "red"),
                     labels = c("USA", "Europa")) +
  geom_hline(yintercept = 0.02, linetype = "dashed", color = "gray50") + # Objetivo 2%
  theme_minimal() +
  labs(title = "Evolución de la Inflación",
       subtitle = "Línea gris = objetivo del 2%",
       x = "Fecha", y = "Inflación %")
ggplotly(p2)

# S&P 500 vs VIX
p3 <- dataset_macro %>%
  mutate(sp500_acumulado = cumprod(1 + sp500_return)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = sp500_acumulado, color = "S&P 500 Acumulado")) +
  geom_line(aes(y = vix/10, color = "VIX/10")) + # Escalar para visualizar
  scale_color_manual(values = c("S&P 500 Acumulado" = "darkblue", 
                                "VIX/10" = "red")) +
  theme_minimal() +
  labs(title = "S&P 500 vs VIX",
       subtitle = "VIX dividido entre 10 para visualización conjunta",
       x = "Fecha", y = "Valor",
       color = "Variable")
ggplotly(p3)

# Petróleo y Oro
p4 <- dataset_macro %>%
  mutate(petroleo_acum = cumprod(1 + petroleo_return),
         oro_acum = cumprod(1 + oro_return)) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = petroleo_acum, color = "Petróleo")) +
  geom_line(aes(y = oro_acum, color = "Oro")) +
  scale_color_manual(values = c("Petróleo" = "brown", "Oro" = "gold")) +
  theme_minimal() +
  labs(title = "Evolución de Petroleo vs Oro",
       subtitle = "Petróleo vs Oro (base 100 = inicio)",
       x = "Fecha", y = "Valor acumulado")
ggplotly(p4)

# Curva 10Y-2Y
ggplot(dataset_macro, aes(x = Date, y = curva10Y2Y)) +
  geom_line(color = "purple", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = pmin(curva10Y2Y, 0), ymax = 0, 
                  fill = curva10Y2Y < 0), alpha = 0.3) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "transparent")) +
  theme_minimal() +
  labs(title = "Curva de Tipos 10Y2Y",
       subtitle = "Área roja = curva invertida (predictor de recesiones)",
       x = "Fecha", y = "Diferencial (%)")

# Precios del activo
p6 <- dataset_final %>%
  select(Date, precio) %>%
  pivot_longer(-Date) %>%
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("precio" = "darkblue"),
                     labels = c("P")) +
  theme_minimal() +
  labs(title = "Precios del activo",
       x = "Fecha", y = "Precio")
ggplotly(p6)

# Retornos del activo
p7 <- dataset_final %>%
  select(Date, activo_return) %>%
  ggplot(aes(x = Date, y = activo_return)) +
  geom_line(color = "red", size = 0.8) +
  theme_minimal() +
  labs(title = "Evolución del Rendimiento de Apple",
       subtitle = "Retornos mensuales (2004-2025)",
       x = "Fecha", 
       y = "Rendimiento (%)")
ggplotly(p7)

# DISTRIBUCIÓN DE RETORNOS (solo para variables que son retornos)


# Seleccionar variables macro NUMÉRICAS (excluyendo las que no son retornos)
# Me quedo con las que tienen sentido para distribución (continuas, sin muchas restricciones)
dataset_macro %>%
  select(sp500_return, vix, petroleo_return, oro_return, 
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~name, scales = "free", ncol = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribución de Variables Macroeconómicas",
       subtitle = "Se muestra la densidad de cada variable (línea roja = valor 0)",
       x = "Valor", y = "Densidad") +
  theme(legend.position = "none")

# QQ-plots para normalidad
par(mfrow = c(1,3))
qqnorm(dataset_macro$sp500_return, main = "S&P 500"); qqline(dataset_macro$sp500_return, col = "red")
qqnorm(dataset_macro$petroleo_return, main = "Petróleo"); qqline(dataset_macro$petroleo_return, col = "red")
qqnorm(dataset_macro$oro_return, main = "Oro"); qqline(dataset_macro$oro_return, col = "red")
par(mfrow = c(1,1))


# MATRIZ DE CORRELACIONES

# Código simple y efectivo
cor_macro <- dataset_macro %>%
  select(-Date) %>%
  cor(use = "pairwise.complete.obs")

# Redondear y mostrar
round(cor_macro, 2)

# Gráfico limpio
corrplot(round(cor_macro, 2), 
         method = "number", 
         type = "upper", 
         tl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.8,
         title = "Matriz de Correlaciones",
         mar = c(0,0,2,0))

# Desempleo vs Curva10Y2Y
ggplot(dataset_macro, aes(x = Date)) +
  geom_line(aes(y = desempleo, color = "Desempleo"), size = 0.8) +
  geom_line(aes(y = curva10Y2Y * 3, color = "Curva 10Y2Y (x3)"), size = 0.8) +
  scale_color_manual(values = c("Desempleo" = "red", 
                                "Curva 10Y2Y (x3)" = "purple")) +
  scale_y_continuous(
    name = "Desempleo",
    sec.axis = sec_axis(~./3, name = "Curva 10Y2Y")
  ) +
  theme_minimal() +
  labs(title = "Desempleo vs Curva de Tipos",
       x = "Fecha",
       color = "Variable")

# MACRO VS APPLE

# Relacionar macro con el target de Apple
# Usamos dataset_final (el original con todas las variables)

cor_macro_target <- dataset_final %>%
  select(target, sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd) %>%
  cor(use = "pairwise.complete.obs")

# Extraer solo correlaciones con target
cor_target_macro <- cor_macro_target["target", -1] %>%  # -1 excluye target consigo misma
  as.data.frame() %>%
  rename(correlacion = ".") %>%
  rownames_to_column("variable") %>%
  arrange(desc(abs(correlacion)))

# Visualizar
cor_target_macro %>%
  ggplot(aes(x = reorder(variable, abs(correlacion)), 
             y = correlacion, fill = correlacion > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
  theme_minimal() +
  labs(title = "Apple vs Variables Macroeconómicas",
       x = "Variable Macro", y = "Correlación con rendimiento de Apple")

# AUTOCORRELACIÓN

# Seleccionar algunas series macro clave
series_macro <- c("sp500_return", "vix", "petroleo_return", "oro_return",
                  "tipos_fed", "tipos_ecb", "inflacion_usa", "inflation_eu",
                  "desempleo", "curva10Y2Y", "eurusd", "activo_return")

par(mfrow = c(2,2))
for(var in series_macro) {
  acf(dataset_macro_conActivoRet[[var]], lag.max = 24, 
      main = paste("ACF -", var), na.action = na.pass)
}
par(mfrow = c(1,1))

#  DETECCIÓN DE OUTLIERS

# Boxplots por variable
dataset_macro %>%
  select(-Date) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots",
       x = "Variable", y = "Valor")

# Identificar outliers con método IQR
detectar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  list(inferior = Q1 - 1.5 * IQR, superior = Q3 + 1.5 * IQR)
}

dataset_macro %>%
  select(-Date) %>%
  summarise(across(everything(), 
                   list(Q1 = ~quantile(., 0.25, na.rm = TRUE),
                        Q3 = ~quantile(., 0.75, na.rm = TRUE),
                        outliers_inf = ~sum(. < quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE), na.rm = TRUE),
                        outliers_sup = ~sum(. > quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE), na.rm = TRUE)))) %>%
  pivot_longer(everything()) %>%
  print(n = Inf)
