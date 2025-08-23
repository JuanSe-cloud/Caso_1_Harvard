# Caso_1_Harvard

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

# Gráfico 4: Libras vendidas
ggplot(data, aes(x = `Week (2008-2009)`, y = `Lbs. Sold`)) +
  geom_col(fill = "purple") +
  labs(title = "Libras Vendidas", x = "Semana (Fecha)", y = "Libras") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))