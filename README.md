# Caso_1_Harvard

# Punto 1. Visualizaciones iniciales
library(tidyverse)

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



  
# Punto 2. calculo de estadisticas: visita y resumen financiero

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
  data$`Week (2008-2009)` >= as.Date("2008-05-25") & data$`Week (2008-2009)` <= as.Date("2008-07-12") ~ "Inicial",
  data$`Week (2008-2009)` >= as.Date("2008-07-13") & data$`Week (2008-2009)` <= as.Date("2008-12-13") ~ "Prepromoción",
  data$`Week (2008-2009)` >= as.Date("2008-12-14") & data$`Week (2008-2009)` <= as.Date("2009-01-24") ~ "Promoción",
  data$`Week (2008-2009)` >= as.Date("2009-01-25") & data$`Week (2008-2009)` <= as.Date("2009-08-29") ~ "Pospromoción",
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

# Punto 3. Visualizaciones adicionales
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

# Punto 4. Hallazgos hasta el momento

#Los datos muestran un aumento en las ventas, ingresos y visitas a medida 
#que avanzan los periodos de promoción. En particular, las libras vendidas alcanzan su 
#máximo en el periodo de promoción (19026.17), lo que indica que la promoción tuvo 
#un impacto positivo en las ventas. De manera similar, los ingresos suben significativamente
#durante la promoción, con un valor de 495063.83. Sin embargo, tras la promoción, las ventas 
#y los ingresos disminuyen, lo que podría reflejar que los efectos positivos de la promoción no 
#se mantienen a largo plazo.

#El beneficio sigue una tendencia similar, aumentando desde el periodo inicial (194923.71) hasta 
#la promoción (146737.33). Sin embargo, el periodo pos-promoción muestra una disminución considerable 
#(122498.39), lo que sugiere que los efectos de la promoción pueden haber tenido un impacto positivo en 
#las ventas, pero no necesariamente en los márgenes de ganancia. En cuanto a las visitas,
#hubo una caida en el periodo pre-promoción comparado con el inicial, sin embargo,
#en el periodo de post-promoción el promedio de las visitas aumentaron significativamente alcamzando los (1381.81),
#lo que podría indicar un interés sostenido en el sitio web después de la promoción.
#En resumen, los datos sugieren que las promociones pueden ser efectivas para aumentar las ventas y los ingresos a corto plazo,
#pero su impacto en la rentabilidad y el tráfico web puede ser más complejo y requiere un análisis más detallado.

# Punto 5. Diagrama de dispersión: Revenue vs Lbs. Sold
ggplot(data, aes(x = `Lbs. Sold`, y = Revenue)) +
  geom_point(color = "darkorange") +
  labs(title = "Relación entre Revenue y Lbs. Sold",
       x = "Libras Vendidas",
       y = "Ingresos ($)") +
  theme_minimal()

# Coeficiente de correlación
cor_revenue_lbs <- cor(data$Revenue, data$`Lbs. Sold`, use = "complete.obs")
print(paste("Coeficiente de correlación entre Revenue y Lbs. Sold:", round(cor_revenue_lbs, 3)))

# Punto 6. Diagrama de dispersión: Revenue vs Visits
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

# Punto 7. Resumen de los resultados

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