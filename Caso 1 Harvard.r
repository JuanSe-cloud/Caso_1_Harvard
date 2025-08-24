##############################################################
# Caso Harvard - Web Analytics at Quality Alloys, Inc.
# Business Analytics - Pontificia Universidad Javeriana
# Integrantes: Paula Rodriguez, Juan Sebastian Cardenas y Valery Ramirez
# Fecha:  2025-08-24
##############################################################
install.packages("moments")
library(tidyverse)
library(readr)
library(e1071)

##############################################################
# Punto 1. Visualizaciones iniciales
##############################################################

weekly_visits <- read_csv("Weekly Visits.csv")
financials <- read_csv("Financials.csv")

# quitamos comas y símbolos
weekly_visits$`Unique Visits` <- as.numeric(gsub(",", "", weekly_visits$`Unique Visits`))
financials$Revenue <- as.numeric(gsub("[$,]", "", financials$Revenue))
financials$Profit <- as.numeric(gsub("[$,]", "", financials$Profit))
financials$`Lbs. Sold` <- as.numeric(gsub(",", "", financials$`Lbs. Sold`))

# Unimos los datos por la columna de semana
data <- left_join(weekly_visits, financials, by = "Week (2008-2009)")

# Convertimos la columna Week (2008-2009) a formato de fecha
data$`Week (2008-2009)` <- as.Date(data$`Week (2008-2009)`, format = "%Y-%m-%d")

# Gráfico 1: Visitas únicas
ggplot(data, aes(x = `Week (2008-2009)`, y = `Unique Visits`)) +
  geom_col(fill = "blue") +
  labs(title = "Visitas Únicas", x = "Semana (Fecha)", y = "Visitas") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfico 2: Ingresos
ggplot(data, aes(x = `Week (2008-2009)`, y = Revenue)) +
  geom_col(fill = "red") +
  labs(title = "Ingresos", x = "Semana (Fecha)", y = "Ingresos ($)") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfico 3: Ganancias
ggplot(data, aes(x = `Week (2008-2009)`, y = Profit)) +
  geom_col(fill = "green") +
  labs(title = "Ganancias", x = "Semana (Fecha)", y = "Ganancias ($)") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Si la conversión a fecha da NA, usa como carácter
if (any(is.na(as.Date(data$Week)))) {
  data$Week <- as.character(data$Week)
} else {
  data$Week <- as.Date(data$Week)
}

# Gráfico 4: Libras vendidas
ggplot(data, aes(x = `Week (2008-2009)`, y = `Lbs. Sold`)) +
  geom_col(fill = "purple") +
  labs(title = "Libras Vendidas", x = "Semana (Fecha)", y = "Libras") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##############################################################  
# Punto 2. calculo de estadisticas: visita y resumen financiero
##############################################################

# quitamos comas y símbolos, lo volvemos a hacer por que a veces falla
weekly_visits$Visits <- as.numeric(gsub(",", "", weekly_visits$Visits))
weekly_visits$`Unique Visits` <- as.numeric(gsub(",", "", weekly_visits$`Unique Visits`))
financials$Revenue <- as.numeric(gsub("[$,]", "", financials$Revenue))
financials$Profit <- as.numeric(gsub("[$,]", "", financials$Profit))
financials$`Lbs. Sold` <- as.numeric(gsub(",", "", financials$`Lbs. Sold`))

# Unimos los datos por la columna de semana
data <- left_join(weekly_visits, financials, by = "Week (2008-2009)")
# Unir los datos por la columna de semana
data$`Week (2008-2009)` <- as.Date(data$`Week (2008-2009)`, format = "%Y-%m-%d")

# Definimos los periodos por las fechas
data$periodo <- case_when(
  data$`Week (2008-2009)` >= as.Date("2008-05-25") & data$`Week (2008-2009)` <= as.Date("2008-08-24") ~ "Inicial",
  data$`Week (2008-2009)` >= as.Date("2008-08-31") & data$`Week (2008-2009)` <= as.Date("2009-01-18") ~ "Prepromoción",
  data$`Week (2008-2009)` >= as.Date("2009-01-25") & data$`Week (2008-2009)` <= as.Date("2009-05-17") ~ "Promoción",
  data$`Week (2008-2009)` >= as.Date("2009-05-24") & data$`Week (2008-2009)` <= as.Date("2009-08-23") ~ "Pospromoción",
  TRUE ~ NA_character_
)

# Seleccionamos las variables de interés
vars <- c("Visits", "Unique Visits", "Revenue", "Profit", "Lbs. Sold")

# Calculo de estadísticas resumidas por período y variable
resumen <- data %>%
  select(periodo, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "variable", values_to = "valor") %>%
  group_by(periodo, variable) %>%
  summarise(
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(periodo, variable)

# Crear una tabla por cada periodo
resumen_inicial <- resumen %>% filter(periodo == "Inicial")
resumen_prepromocion <- resumen %>% filter(periodo == "Prepromoción")
resumen_promocion <- resumen %>% filter(periodo == "Promoción")
resumen_pospromocion <- resumen %>% filter(periodo == "Pospromoción")

# Mostrar cada tabla
print(resumen_inicial)
print(resumen_prepromocion)
print(resumen_promocion)
print(resumen_pospromocion) 

##############################################################
# Punto 3. Visualizaciones adicionales
##############################################################
# tabla resumen solo con medias por periodo y variable
tabla_medias <- resumen %>%
  select(periodo, variable, media) %>%
  pivot_wider(names_from = variable, values_from = media) %>%
  arrange(factor(periodo, levels = c("Inicial", "Prepromoción", "Promoción", "Pospromoción")))

print(tabla_medias)

# Gráfico de columnas: visitas promedio por periodo
ggplot(tabla_medias, aes(x = periodo, y = Visits)) +
  geom_col(fill = "blue") +
  labs(title = "Visitas Promedio por Periodo", x = "Periodo", y = "Visitas Promedio")

# Gráfico de columnas: visitas únicas promedio por periodo
ggplot(tabla_medias, aes(x = periodo, y = `Unique Visits`)) +
  geom_col(fill = "green") +
  labs(title = "Visitas Únicas Promedio por Periodo", x = "Periodo", y = "Visitas Únicas Promedio")

# Gráfico de columnas: ingresos promedio por periodo
ggplot(tabla_medias, aes(x = periodo, y = Revenue)) +
  geom_col(fill = "orange") +
  labs(title = "Ingresos Promedio por Periodo", x = "Periodo", y = "Ingresos Promedio")

# Gráfico de columnas: ganancias promedio por periodo
ggplot(tabla_medias, aes(x = periodo, y = Profit)) +
  geom_col(fill = "purple") +
  labs(title = "Ganancias Promedio por Periodo", x = "Periodo", y = "Ganancias Promedio")

# Gráfico de columnas: libras vendidas promedio por periodo
ggplot(tabla_medias, aes(x = periodo, y = `Lbs. Sold`)) +
  geom_col(fill = "brown") +
  labs(title = "Libras Vendidas Promedio por Periodo", x = "Periodo", y = "Libras Promedio")# 4 Hallazgos hasta el momento

##############################################################
# Punto 4. Hallazgos hasta el momento
##############################################################
#Los datos muestran una disminución en las ventas y la utilidad a medida 
#que avanzan los periodos de promoción. El promedio de la utilidad sigue disminuyó desde el periodo inicial (200233.43) hasta 
#la promoción (131929.88). Y presentando una disminución considerable en el periodo pos-promoción llegando a los
#(111045.86), lo que sugiere que los efectos de la promoción no lograron un impacto positivo en 
#las ganacias. Ni las ventas, ya que el promedio de las ventas en el periodo inicial fue de (608250.21),
#y para el periodo de promoción fue de (456390.88), y en el periodo pos-promoción fue de (371728.07). 

#La unicsa variables que muestran un aumento significativo en el periodo de promoción 
#fueron las visitas y las visitas únicas, que empezaron en (1056) y(976) y para el perido 
#promoción llegaron a (1815)(1739) respectivamente. Pero para el peridodo de post-promoción disminuyeron drasticamente,
#llegando a (857) y (801) respectivamente. Ahora en cuanto a las libras vendidas, estas disminuyeron levemente del periodo 
#inicial (18736.71) al pre-promoción (18440.71), y presentaron una disminución considerable en el periodo de promoción (17112.82)
#hasta llegar a un nivel inferior del inicial para el periodo de post-promoción (145577.86). 
#En resumen, los datos sugieren que las promociones no son efectivas para aumentar las ventas y los ingresos a corto plazo, y 
#su impacto en la rentabilidad y el tráfico web puede ser más complejo y requiere un análisis más detallado.

##############################################################
# Punto 5. Diagrama de dispersión: Revenue vs Lbs. Sold
##############################################################

ggplot(data, aes(x = `Lbs. Sold`, y = Revenue)) +
  geom_point(color = "darkorange") +
  labs(title = "Relación entre Revenue y Lbs. Sold",
       x = "Libras Vendidas",
       y = "Ingresos ($)") +
  theme_minimal()

# Coeficiente de correlación
cor_revenue_lbs <- cor(data$Revenue, data$`Lbs. Sold`, use = "complete.obs")
print(paste("Coeficiente de correlación entre Revenue y Lbs. Sold:", round(cor_revenue_lbs, 3)))

##############################################################
# Punto 6. Diagrama de dispersión: Revenue vs Visits
##############################################################
ggplot(data, aes(x = Visits, y = Revenue)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Revenue y Visits",
       x = "Visitas",
       y = "Ingresos ($)") +
  theme_minimal()

# Coeficiente de correlación
cor_revenue_visits <- cor(data$Revenue, data$Visits, use = "complete.obs")
print(paste("Coeficiente de correlación entre Revenue y Visits:", round(cor_revenue_visits, 3)))

# Diagrama de dispersión: Revenue vs Unique Visits
ggplot(data, aes(x = `Unique Visits`, y = Revenue)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Revenue y Visits",
       x = "Visitas Unicas",
       y = "Ingresos ($)") +
  theme_minimal()

# Coeficiente de correlación
cor_revenue_unique_visits <- cor(data$Revenue, data$`Unique Visits`, use = "complete.obs")
print(paste("Coeficiente de correlación entre Revenue y Unique Visits:", round(cor_revenue_unique_visits, 3)))

# Diagrama de dispersión: Lbs. Sold vs Visits
ggplot(data, aes(x = Visits, y = `Lbs. Sold`)) +
  geom_point(color = "blue") +
  labs(title = "Relación entre Revenue y Lbs. Sold",
       x = "Visitas",
       y = "Libras Vendidas") +
  theme_minimal()

# Coeficiente de correlación
cor_lbs_visits <- cor(data$Visits, data$`Lbs. Sold`, use = "complete.obs")
print(paste("Coeficiente de correlación entre Lbs. Sold y Visits:", round(cor_lbs_visits, 3)))

##############################################################
# Punto 7. Resumen de los resultados
##############################################################

#Los resultados de las correlaciones asi como en la observación del diagrama
#de dispersión muestran que hay una relación clara entre los ingresos y las libras de material vendidas,
# del (0.869). Sin embargo, al momento de ver la relación entre las variables 
# de Visitas e Ingresos, en el diagrama de dispersión no se ve ningun tipo de tendencia,
#y esta posee una correlación negativa del (-0.059) lo , en general, un aumento en las visitas
# no está asociado de manera significativa con un aumento en los ingresos, 
#y podría incluso haber una ligera tendencia a la disminución de los ingresos, similarmente ocurre
#con las variables de visitas unicas que presentan una correlación del (0.069). 
#Finalmente, la relación entre las visitas y las libras vendidas también es débilmente negativa (-0.047),
#la relación entre ambos variables es prácticamente nula, lo que sugiere que otros 
#factores más allá del número de visitas de la página pueden influir en los ingresos y las ventas de libras.

##############################################################
# Punto 8. Estadisticos Descriptivos
##############################################################

LBS <- read_csv("LBSsold.csv")
glimpse(LBS)
head(LBS)
# Convertir la columna a numérica y renombrar
LBS$Lbs.Sold <- as.numeric(gsub(",", "", gsub("\"", "", LBS$`Lbs. Sold`)))

# a) Calcula los estadísticos
tabla_vertical_lbs <- LBS %>%
  summarise(
    Mean = mean(Lbs.Sold, na.rm = TRUE),
    `Standard Error` = sd(Lbs.Sold, na.rm = TRUE) / sqrt(sum(!is.na(Lbs.Sold))),
    Median = median(Lbs.Sold, na.rm = TRUE),
    Standard = sd(Lbs.Sold, na.rm = TRUE),
    `Sample Variance` = var(Lbs.Sold, na.rm = TRUE),
    Kurtosis = kurtosis(Lbs.Sold, na.rm = TRUE) + 3,
    Skewness = skewness(Lbs.Sold, na.rm = TRUE),
    Range = max(Lbs.Sold, na.rm = TRUE) - min(Lbs.Sold, na.rm = TRUE),
    Minimum = min(Lbs.Sold, na.rm = TRUE),
    Maximum = max(Lbs.Sold, na.rm = TRUE),
    Sum = sum(Lbs.Sold, na.rm = TRUE),
    Count = sum(!is.na(Lbs.Sold))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadistico", values_to = "Valor") %>%
  mutate(Valor = format(round(Valor, 3), nsmall = 3)) %>% # Siempre tres decimales
  arrange(match(Estadistico, c(
    "Mean", "Standard Error", "Median", "Standard", "Sample Variance",
    "Kurtosis", "Skewness", "Range", "Minimum", "Maximum", "Sum", "Count"
  )))

print(tabla_vertical_lbs)


# b) Histograma
ggplot(LBS, aes(x = `Lbs. Sold`)) +
  geom_histogram(
    bins = nclass.Sturges(LBS$`Lbs. Sold`),   # calcula # de bins
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    title = "Histograma de Lbs. Sold",
    x = "Lbs. Sold",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 30)

# c) Describir el histograma
# El histograma de Lbs. Sold concentra la mayoría de valores entre 10,000 y 20,000, 
# pero no es completamente simétrico: muestra una ligera cola hacia la derecha. 
# Esto coincide con un skewness de 0.626 (asimetría positiva leve) 
# y una kurtosis de 3.509 (ligeramente más apuntado que una distribución normal). 
# Por tanto, no tiene una forma de campana perfecta y el histograma por sí solo 
# no basta para concluir normalidad

# d) Conteo para la Regla Empírica (tabla para ±1σ, ±2σ, ±3σ)

LBS$Lbs.Sold <- as.numeric(gsub(",", "", gsub("\"", "", LBS$`Lbs. Sold`)))

LBS <- LBS %>%
  mutate(z = (Lbs.Sold - mean(Lbs.Sold, na.rm = TRUE)) / sd(Lbs.Sold, na.rm = TRUE))

N <- sum(!is.na(LBS$z))
theo_pct <- c(68, 95, 99)
theo_obs <- round(N * theo_pct / 100)

actual_obs <- c(
  sum(abs(LBS$z) <= 1, na.rm = TRUE),
  sum(abs(LBS$z) <= 2, na.rm = TRUE),
  sum(abs(LBS$z) <= 3, na.rm = TRUE)
)

tabla_empirica_lbs <- data.frame(
  Interval = c("mean ± 1 std. dev.", "mean ± 2 std. dev.", "mean ± 3 std. dev."),
  `Theoretical % of Data` = paste0(theo_pct, "%"),
  `Theoretical No. Obs.` = theo_obs,
  `Actual No. Obs.` = actual_obs
)

print(tabla_empirica_lbs)

# e) Conteo para la Regla Empírica (tabla refinada)
theo_pct_ref <- c(34, 34, 13.5, 13.5, 2, 2)
theo_obs_ref <- round(N * theo_pct_ref / 100)

actual_obs_ref <- c(
  sum(LBS$z > 0 & LBS$z <= 1, na.rm = TRUE),     # mean + 1 std. dev.
  sum(LBS$z >= -1 & LBS$z <= 0, na.rm = TRUE),   # mean - 1 std. dev.
  sum(LBS$z > 1 & LBS$z <= 2, na.rm = TRUE),     # 1 std. dev. to 2 std. dev.
  sum(LBS$z >= -2 & LBS$z < -1, na.rm = TRUE),   # -1 std. dev. to -2 std. dev.
  sum(LBS$z > 2 & LBS$z <= 3, na.rm = TRUE),     # 2 std. dev. to 3 std. dev.
  sum(LBS$z >= -3 & LBS$z < -2, na.rm = TRUE)    # -2 std. dev. to -3 std. dev.
)

tabla_refinada_lbs <- data.frame(
  Interval = c("mean + 1 std. dev.",
               "mean - 1 std. dev.",
               "1 std. dev. to 2 std. dev.",
               "-1 std. dev. to -2 std. dev.",
               "2 std. dev. to 3 std. dev.",
               "-2 std. dev. to -3 std. dev."),
  `Theoretical No. Obs.` = theo_obs_ref,
  `Actual No. Obs.` = actual_obs_ref
)

print(tabla_refinada_lbs)

# f) Distribucion normal?
# En la tabla refinada se observa un ligero desequilibrio:
# en +1σ hubo 84 obs. vs 99 teóricas (menos de lo esperado),
# mientras que en -1σ hubo 117 vs 99 teóricas (más de lo esperado).
# Esto indica que hay más datos en el lado izquierdo del promedio 
# y menos en el derecho, lo que refleja una asimetría positiva leve.
# Por otro lado, los valores reales de las observaciones son casi iguales a los teóricos
# en ±2σ y ±3σ. Esto sugiere que la distribución de Lbs. Sold 
# se aproxima bastante a una normal y puede usarse para pronósticos,
# aunque no sea perfectamente simétrica.

# g) Skewness y Kurtosis
# Skewness = 0.626 → confirma una asimetría positiva leve (cola ligeramente hacia la derecha).
# Kurtosis = 3.509 → indica que la distribución tiene un pico apenas más pronunciado que la normal (kurtosis = 3), 
# con colas ligeramente más pesadas.
# Estos valores coinciden con lo observado en el histograma y en la tabla refinada,
# la serie no es perfectamente normal, pero sí se aproxima lo suficiente para el análisis.

##############################################################
# Punto 9. Analisis Comparables Estadisticos Descriptivos
##############################################################
Daily <- read_csv("DailyVisit.csv")
glimpse(Daily)
head(Daily)

# 1) Descriptivos 

mode_visits <- as.numeric(names(sort(table(Daily$Visits), decreasing = TRUE)[1]))

desc_stats <- c(
  Mean = mean(Daily$Visits, na.rm = TRUE),
  `Standard Error` = sd(Daily$Visits, na.rm = TRUE) / sqrt(sum(!is.na(Daily$Visits))),
  Median = median(Daily$Visits, na.rm = TRUE),
  Mode = mode_visits,
  Standard = sd(Daily$Visits, na.rm = TRUE),
  `Sample Variance` = var(Daily$Visits, na.rm = TRUE),
  Kurtosis = kurtosis(Daily$Visits, na.rm = TRUE),
  Skewness = skewness(Daily$Visits, na.rm = TRUE),
  Range = max(Daily$Visits, na.rm = TRUE) - min(Daily$Visits, na.rm = TRUE),
  Minimum = min(Daily$Visits, na.rm = TRUE),
  Maximum = max(Daily$Visits, na.rm = TRUE),
  Sum = sum(Daily$Visits, na.rm = TRUE),
  Count = sum(!is.na(Daily$Visits))
)

tabla_vertical <- data.frame(
  Estadistico = names(desc_stats),
  Valor = as.numeric(desc_stats)
)

print(tabla_vertical)

# 2) Histograma
ggplot(Daily, aes(x = Visits)) +
  geom_histogram(color = "black", fill = "lightblue", binwidth = 25) +
  scale_x_continuous(breaks = seq(min(Daily$Visits, na.rm = TRUE), max(Daily$Visits, na.rm = TRUE), by = 25)) +
  labs(title = "Histograma de Visits", x = "Visits", y = "Frecuencia")

# 3) Conteo para la Regla Empírica (tabla para ±1σ, ±2σ, ±3σ)

Daily <- Daily %>%
  mutate(z = (Visits - mean(Visits, na.rm = TRUE)) / sd(Visits, na.rm = TRUE))

N <- sum(!is.na(Daily$z))

theo_pct <- c(68, 95, 99)
theo_obs <- round(N * theo_pct / 100)

actual_obs <- c(
  sum(abs(Daily$z) <= 1, na.rm = TRUE),  # mean ± 1 std. dev.
  sum(abs(Daily$z) <= 2, na.rm = TRUE),  # mean ± 2 std. dev.
  sum(abs(Daily$z) <= 3, na.rm = TRUE)   # mean ± 3 std. dev.
)

tabla_empirica <- data.frame(
  Interval = c("mean ± 1 std. dev.", "mean ± 2 std. dev.", "mean ± 3 std. dev."),
  `Theoretical % of Data` = paste0(theo_pct, "%"),
  `Theoretical No. Obs.` = theo_obs,
  `Actual No. Obs.` = actual_obs
)

print(tabla_empirica)

# 4) Conteo para la Regla Empírica (tabla refinada)

Daily <- Daily %>%
  mutate(z = (Visits - mean(Visits, na.rm = TRUE)) / sd(Visits, na.rm = TRUE))

N <- sum(!is.na(Daily$z))

# Valores teóricos según la normal estándar
theo_pct <- c(34, 34, 13.5, 13.5, 2, 2)
theo_obs <- round(N * theo_pct / 100)

actual_obs <- c(
  sum(Daily$z > 0 & Daily$z <= 1, na.rm = TRUE),      # mean + 1 std. dev.
  sum(Daily$z >= -1 & Daily$z <= 0, na.rm = TRUE),    # mean - 1 std. dev.
  sum(Daily$z > 1 & Daily$z <= 2, na.rm = TRUE),      # 1 std. dev. to 2 std. dev.
  sum(Daily$z >= -2 & Daily$z < -1, na.rm = TRUE),    # -1 std. dev. to -2 std. dev.
  sum(Daily$z > 2 & Daily$z <= 3, na.rm = TRUE),      # 2 std. dev. to 3 std. dev.
  sum(Daily$z >= -3 & Daily$z < -2, na.rm = TRUE)     # -2 std. dev. to -3 std. dev.
)

tabla_refinada <- data.frame(
  Interval = c("mean + 1 std. dev.",
               "mean - 1 std. dev.",
               "1 std. dev. to 2 std. dev.",
               "-1 std. dev. to -2 std. dev.",
               "2 std. dev. to 3 std. dev.",
               "-2 std. dev. to -3 std. dev."),
  `Theoretical No. Obs.` = theo_obs,
  `Actual No. Obs.` = actual_obs
)

print(tabla_refinada)

# Comparacion entre Lbs. Sold y Daily Visits
# Comparando ambas variables, los Lbs. Sold muestran un comportamiento más cercano
# a la normalidad: tienen skewness = 0.63 y kurtosis = 3.51, lo cual indica 
# solo una ligera asimetría positiva y colas un poco más pesadas que la normal. 
# En cambio, los Daily Visits presentan skewness = 2.16 y kurtosis = 5.75, 
# valores que reflejan una fuerte asimetría positiva y colas mucho más pesadas.

# Esto también se ve en la Regla Empírica, los Daily Visits tienen más datos a la izquierda 
# de la media y menos a la derecha de lo esperado. 
# Conclusión: los Lbs. Sold se ajustan mejor a la distribución normal, mientras 
# que los Daily Visits se desvían más de la normalidad y muestran mayor sesgo 
# y curtosis. Esto quiere decir que es mejor usar los Lbs. Sold para análisis
# que asumen normalidad, lo que es bueno porque nuestra muestra se acerca a la poblacional.

##############################################################
# Punto 10
##############################################################

# Librerías necesarias
library(readxl)
library(ggplot2)

traffic <- read_excel("quality_alloys_data.xlsx", sheet = "Traffic_Sources")
ref_sites <- read_excel("quality_alloys_data.xlsx", sheet = "Referring_Sites")
search_engines <- read_excel("quality_alloys_data.xlsx", sheet = "Search_Engines")
geo <- read_excel("quality_alloys_data.xlsx", sheet = "Geographic_Sources")
browsers <- read_excel("quality_alloys_data.xlsx", sheet = "Browsers")
os <- read_excel("quality_alloys_data.xlsx", sheet = "Operating_Systems")

# 1) All Traffic Sources
ggplot(traffic, aes(x = Source, y = Visits, fill = Source)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("All Traffic Sources")

# La web depende principalmente de sitios referentes externos, 
# más que de buscadores o tráfico directo.

# 2) Referring Sites
ggplot(ref_sites, aes(x = reorder(Site, -Visits), y = Visits, fill = Site)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Ten Referring Sites")

# Prácticamente todo el tráfico referido viene de solo dos fuentes principales: Doubleclick y Googlesyndication.
# El resto de los sitios aportan un volumen muy pequeño en comparación.

# 3) Search Engines
ggplot(search_engines, aes(x = reorder(Engine, -Visits), y = Visits, fill = Engine)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Ten Search Engine Sources")

# Google domina ampliamente como fuente de tráfico de buscadores,
# casi todos los usuarios que llegan por buscadores lo hacen desde allí.
# Los demás motores de búsqueda tienen una presencia mínima.

# 4) Geographic Sources
ggplot(geo, aes(x = reorder(Region, -Visits), y = Visits, fill = Region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Ten Geographic Sources")

# La mayoría de visitas proviene de América, sobre todo del sur y del norte.
# Europa y Asia también aportan tráfico, pero claramente en menor medida.

# 5) Browsers
ggplot(browsers, aes(x = reorder(Browser, -Visits), y = Visits, fill = Browser)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Ten Browsers Used")

# Internet Explorer es, por mucho, el navegador más usado, con Firefox como segunda opción.
# El resto de navegadores apenas se usan.

# 6) Operating Systems
ggplot(os, aes(x = reorder(OS, -Visits), y = Visits, fill = OS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Ten Operating Systems Used")

# Casi todos los usuarios navegan desde Windows, lo que muestra un claro dominio de este sistema operativo.
# Otros sistemas como Mac, Linux o móviles aparecen, pero con una participación mínima.
