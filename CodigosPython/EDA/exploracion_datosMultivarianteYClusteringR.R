# EDA MULTIVARIANTE Y CLUSTERING DE SERIES TEMPORALES
install.packages("GGally")
install.packages("entropy")
install.packages("infotheo")

library(GGally)
library(tidyverse)
library(factoextra)
library(cluster)
library(dendextend)
library(pheatmap)
library(entropy)
library(infotheo)


# Cargar datos
dataset_final$Date <- as.Date(dataset_final$Date)

# Seleccionar variables macro 
dataset_macro <- dataset_final %>%
  select(sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd)

dataset_macro_precios <- dataset_final %>%
  select(sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd, activo_return, precio, target)
dataset_finan_precios <- dataset_final %>%
  select(media_movil_3m, media_movil_6m, media_movil_12m, volatilidad_3m, volatilidad_6m, momentum_3m, momentum_6m, momentum_12m, precio, activo_return, lag_1, lag_2, lag_3, lag_6, lag_12)

# ANÁLISIS DE COMPONENTES PRINCIPALES
# Escalar los datos
macro_scaled <- scale(dataset_macro)

# Realizar PCA
pca_result <- prcomp(macro_scaled, center = TRUE, scale. = TRUE)

# Resumen de varianza explicada
summary(pca_result)

# Gráfico de varianza explicada (scree plot)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "PCA")


# Contribución de cada variable a los componentes principales
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribución de Variables al PC1")
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10) +
  labs(title = "Contribución de Variables al PC2")
fviz_contrib(pca_result, choice = "var", axes = 3, top = 10) +
  labs(title = "Contribución de Variables al PC3")
fviz_contrib(pca_result, choice = "var", axes = 4, top = 10) +
  labs(title = "Contribución de Variables al PC4")
fviz_contrib(pca_result, choice = "var", axes = 5, top = 10) +
  labs(title = "Contribución de Variables al PC5")
fviz_contrib(pca_result, choice = "var", axes = 6, top = 10) +
  labs(title = "Contribución de Variables al PC6")
fviz_contrib(pca_result, choice = "var", axes = 7, top = 10) +
  labs(title = "Contribución de Variables al PC7")

# HEATMAP DE CORRELACIONES

#Matriz de correlaciones sin clustering
cor_macro <- cor(dataset_macro_precios, use = "pairwise.complete.obs")
cor_fin <- cor(dataset_finan_precios, use = "pairwise.complete.obs")


# Heatmap sin clustering (orden original de las variables)
pheatmap(cor_macro,
         main = "Matriz de Correlaciones",
         display_numbers = TRUE,
         number_format = "%.2f",
         fontsize_number = 8,
         cluster_rows = FALSE,   # Desactiva clustering en filas
         cluster_cols = FALSE)   # Desactiva clustering en columnas

pheatmap(cor_fin,
         main = "Matriz de Correlaciones",
         display_numbers = TRUE,
         number_format = "%.2f",
         fontsize_number = 8,
         cluster_rows = FALSE,   # Desactiva clustering en filas
         cluster_cols = FALSE)   # Desactiva clustering en columnas

# CLUSTERING

# Definir regímenes basados en eventos históricos
dataset_final$regimen <- case_when(
  # Auge y burbuja inmobiliaria (2003-2007)
  dataset_final$Date >= "2003-01-01" & dataset_final$Date <= "2007-09-30" ~ "Precrisis 2008",
  
  # Crisis financiera aguda (2008-2009)
  dataset_final$Date >= "2007-10-01" & dataset_final$Date <= "2009-12-31" ~ "Crisis financiera",
  
  # Recuperación lenta y crisis de deuda europea (2010-2013)
  dataset_final$Date >= "2010-01-01" & dataset_final$Date <= "2013-12-31" ~ "Recuperación de crisis",
  
  # Recuperación y crecimiento sostenido (2014-2019)
  dataset_final$Date >= "2014-01-01" & dataset_final$Date <= "2019-12-31" ~ "Crecimiento económico",
  
  # Parón por COVID-19 (2020-2021)
  dataset_final$Date >= "2020-01-01" & dataset_final$Date <= "2021-12-31" ~ "Crisis por COVID-19",
  
  # Recuperación, inflación y estabilización (2022-2025)
  dataset_final$Date >= "2022-01-01" & dataset_final$Date <= "2025-12-31" ~ "Recuperación y estabilización",
  
  # Período anterior a 2003 (si existe)
  dataset_final$Date < "2003-01-01" ~ "Período inicial",
  
  TRUE ~ "Otros"
)

# Verificar la distribución
table(dataset_final$regimen)


# Calcular la posición de la franja (parte inferior del gráfico)
# Usamos el mínimo del precio para situar la franja abajo
y_min_precio <- min(dataset_final$precio, na.rm = TRUE)
y_max_precio <- max(dataset_final$precio, na.rm = TRUE)

# Definir la altura de la franja (10% del rango del precio)
franja_altura <- (y_max_precio - y_min_precio) * 0.08
franja_y_base <- y_min_precio - franja_altura * 0.5

ggplot(dataset_final, aes(x = Date)) +
  # Franja de regímenes en la parte inferior
  geom_rect(aes(xmin = Date, xmax = lead(Date), 
                ymin = franja_y_base, 
                ymax = franja_y_base + franja_altura,
                fill = regimen), alpha = 0.9) +
  # Línea del precio de Apple
  geom_line(aes(y = precio), color = "blue", size = 0.8) +
  # Personalizar colores de los regímenes
  scale_fill_manual(values = c(
    "Precrisis 2008" = "darkgreen",
    "Crisis financiera" = "red",
    "Recuperación de crisis" = "pink",
    "Crecimiento económico" = "green",
    "Crisis por COVID-19" = "orange",
    "Recuperación y estabilización" = "purple",
    "Otros" = "gray"
  )) +
  # Etiquetas y tema
  labs(title = "Evolución del Precio de Apple ciclos económicos",
       x = "Fecha", 
       y = "Precio de Apple",
       fill = "Ciclo económico") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.subtitle = element_text(size = 9)
  )

# Caracterizar cada régimen
regimen_summary <- dataset_final %>%
  group_by(regimen) %>%
  summarise(
    n_meses = n(),
    fecha_inicio = min(Date),
    fecha_fin = max(Date),
    rendimiento_medio_apple = mean(target, na.rm = TRUE),
    sp500_medio = mean(sp500_return, na.rm = TRUE),
    vix_medio = mean(vix, na.rm = TRUE),
    inflacion_usa_media = mean(inflacion_usa, na.rm = TRUE),
    inflacion_eu_media = mean(inflation_eu, na.rm = TRUE),
    tipos_fed_medio = mean(tipos_fed, na.rm = TRUE),
    tipos_ecb_medio = mean(tipos_ecb, na.rm = TRUE),
    desempleo_medio = mean(desempleo, na.rm = TRUE),
    petroleo_medio = mean(petroleo_return, na.rm = TRUE),
    oro_medio = mean(oro_return, na.rm = TRUE),
    curva10Y2Y_medio = mean(curva10Y2Y, na.rm = TRUE),
    eurusd_medio = mean(eurusd, na.rm = TRUE)
  ) %>%
  arrange(fecha_inicio)

# Mostrar tabla completa
print(regimen_summary)

# Gráfico de caracterización de regímenes
regimen_summary %>%
  select(regimen, sp500_medio, vix_medio, inflacion_usa_media, inflacion_eu_media, tipos_fed_medio, tipos_ecb_medio) %>%
  pivot_longer(-regimen) %>%
  ggplot(aes(x = regimen, y = value, fill = regimen)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Influencia de variables en cada ciclo",
       x = "Régimen", y = "Valor medio")


# Gráfico de caracterización de regímenes
regimen_summary %>%
  select(regimen, desempleo_medio, oro_medio, petroleo_medio, curva10Y2Y_medio, eurusd_medio) %>%
  pivot_longer(-regimen) %>%
  ggplot(aes(x = regimen, y = value, fill = regimen)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Influencia de variables en cada ciclo",
       x = "Régimen", y = "Valor medio")


# Cálculo de la entropía

# Función para calcular entropía de una variable continua
calcular_entropia <- function(x, bins = 10) {
  # Discretizar la variable en bins
  x_discreto <- cut(x, breaks = bins, include.lowest = TRUE)
  # Calcular frecuencias
  frecuencias <- table(x_discreto)
  # Calcular entropía
  entropia <- entropy.empirical(frecuencias, unit = "log2")
  return(entropia)
}

# Calcular entropía de las variables predictoras
variables_predictoras <- dataset_final %>%
  select(sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd,
         media_movil_3m, media_movil_6m, volatilidad_3m, 
         momentum_3m, lag_1, lag_2, lag_3)

entropias <- data.frame(
  variable = names(variables_predictoras),
  entropia = sapply(variables_predictoras, calcular_entropia, bins = 10)
) %>%
  arrange(desc(entropia))

# Mostrar resultados
print(entropias)

# Visualizar
ggplot(entropias, aes(x = reorder(variable, entropia), y = entropia)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Entropía de las Variables Predictoras",
       subtitle = "Mayor entropía = más información (menos predecible)",
       x = "Variable", y = "Entropía (bits)")

# Mutual information con target

# Preparar datos (eliminar NAs)
datos_ml <- dataset_final %>%
  select(target, sp500_return, vix, petroleo_return, oro_return,
         tipos_fed, tipos_ecb, inflacion_usa, inflation_eu, 
         desempleo, curva10Y2Y, eurusd, media_movil_12m,
         media_movil_3m, media_movil_6m, volatilidad_3m,volatilidad_6m, 
         momentum_3m, momentum_6m, momentum_12m, lag_1, lag_2, lag_3, lag_6, lag_12,  activo_return, precio) %>%
  na.omit()

# Discretizar todas las variables para mutual information
datos_discretos <- discretize(datos_ml)

# Calcular mutual information con el target
mi_resultados <- data.frame(
  variable = names(datos_ml)[-1],  # excluir target
  mutual_info = sapply(names(datos_ml)[-1], function(var) {
    mi <- mutinformation(datos_discretos$target, datos_discretos[[var]])
    return(mi)
  })
) %>%
  arrange(desc(mutual_info))

# Mostrar resultados
print(mi_resultados)

# Visualizar
ggplot(mi_resultados, aes(x = reorder(variable, mutual_info), y = mutual_info)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Información Mutua con el Rendimiento de Apple",
       subtitle = "Mayor valor = mayor capacidad predictiva (captura relaciones lineales y no lineales)",
       x = "Variable", y = "Información Mutua (nats)")