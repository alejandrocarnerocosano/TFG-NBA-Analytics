df <- read_excel("tfg_base.xlsx")

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Gráfico: Evolución del número TOTAL de analistas por año
df_total <- df %>%
  group_by(Season) %>%
  summarise(Total_Analysts = sum(Analysts, na.rm = TRUE))

ggplot(df_total, aes(x = Season, y = Total_Analysts)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  theme_minimal() +
  labs(title = "Evolució del total de analistes a l'NBA",
       x = "Temporada", y = "Nombre Total de Analistes")

# 2. Gráfico: Evolución del número MEDIO de analistas por equipo por año
df_media <- df %>%
  group_by(Season) %>%
  summarise(Media_Analysts = mean(Analysts, na.rm = TRUE))

ggplot(df_media, aes(x = Season, y = Media_Analysts)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "black", size = 2) +
  theme_minimal() +
  labs(title = "Evolución de la media de analistas por equipo",
       x = "Temporada", y = "Media de Analistas")

# 3. Gráfico: Evolución de analistas POR EQUIPO (Facet Grid con los 30 gráficos)
ggplot(df, aes(x = Season, y = Analysts)) +
  geom_line(color = "red") +
  theme_minimal() +
  facet_wrap(~ Team, ncol = 5) + # Crea una cuadrícula con 5 columnas
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  labs(title = "Evolució del nombre d'analistas per equip",
       x = "Temporada", y = "Analistes")
