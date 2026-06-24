# ==============================================================================
#                      PREPROCESSING: Descriptiva Automàtica NBA
# ==============================================================================
# Carreguem les llibreries =====================================================
# install.packages(c("visdat", "inspectdf", "skimr", "tidyverse", "psych"))
library(visdat)
library(inspectdf)
library(skimr)
library(tidyverse)
library(psych)

# Carreguem la base de dades ===================================================
# Substitueix "nba_panel_data.csv" pel nom real del teu Excel/CSV
dades <- df

# Preparació de carpetes =======================================================
path <- "grafics_univariants/"
if (!dir.exists(path)) {
  dir.create(path)
}

# Conversió de variables categòriques de l'NBA =================================
# Assegurem que l'equip i el canvi d'entrenador siguin factors, no números o text pla.
dades$
dades$Team <- as.factor(dades$Team)
if("New_Coach" %in% colnames(dades)) {
  dades$New_Coach <- as.factor(dades$New_Coach)
}

### Escollim el tipus de les variables 
tipos <- sapply(dades, class)
varNum <- names(tipos)[tipos %in% c("integer", "numeric")]
varCat <- names(tipos)[tipos %in% c("factor", "character")]

# ==============================================================================
## 1. VARIABLES NUMÈRIQUES
# ==============================================================================
#Boxplot Analistes por año
png(filename = "grafics_univariants/boxplot_analysts_any.png", width = 800, height = 600)
ggplot(dades, aes(x = as.factor(Season), y = Analysts)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Evolució del departament de dades per temporada",
       x = "Temporada",
       y = "Nombre d'analistes") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
ggplot(dades, aes(x = as.factor(Season), y = Analysts)) + geom_boxplot(fill="skyblue") + theme_minimal()

sink(file = paste0(path, "descriptiva_univariant_numerica.txt"))
for (vN in varNum) {
  cat(paste0("================= ", vN, " ================\n"))
  
  # Guardem la informació numèrica (Mitjana, Mediana, Desviació, Skewness)
  print(psych::describe(dades[, vN], na.rm = TRUE))
  cat(paste0(rep("-", 50), "\n"))
  
  ## Boxplot
  png(filename = paste0(path, "boxplot_", vN, ".png"), width = 800, height = 600)
  grafico_box <- ggplot(dades, aes(y = .data[[vN]])) +
    geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
    labs(title = paste0("Distribució de ", vN),
         y = "Valor",
         x = "") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  print(grafico_box)
  dev.off()
  
  ## Histograma
  # Utilitzem 'bins = 30' en lloc de 'binwidth = 5' perquè s'adapti automàticament 
  # a escales molt diferents (ex: Victòries vs Salaris en milions)
  png(filename = paste0(path, "histograma_", vN, ".png"), width = 800, height = 600)
  grafico_hist <- ggplot(dades, aes(x = .data[[vN]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "darkblue", alpha = 0.7) +
    labs(title = paste0("Histograma de ", vN),
         x = vN,
         y = "Freqüència") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  print(grafico_hist)
  dev.off()
}
sink()

# ==============================================================================
## 2. VARIABLES CATEGÒRIQUES
# ==============================================================================
sink(file = paste0(path, "descriptiva_univariant_categorica.txt"))
for (vC in varCat) {
  cat(paste0("================= ", vC, " ================\n"))
  
  # Taules de freqüències absolutes i relatives
  frecAbs <- table(dades[, vC])
  frecRel <- prop.table(frecAbs)
  
  tabla_frecuencia <- data.frame(Frec_Absoluta = as.numeric(frecAbs))
  rownames(tabla_frecuencia) <- names(frecAbs)
  tabla_frecuencia$Frec_Relativa <- round(as.numeric(frecRel), 3)
  
  print(tabla_frecuencia)
  cat(paste0(rep("-", 50), "\n"))
  
  # Barplot 
  png(filename = paste0(path, "barplot_", vC, ".png"), width = 800, height = 600)
  datos_plot <- data.frame(Categoria = names(frecAbs), Freq = as.numeric(frecAbs))
  
  grafico_bar <- ggplot(datos_plot, aes(x = reorder(Categoria, -Freq), y = Freq, fill = Categoria)) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = paste0("Gràfic de Barras: ", vC),
         x = "Categoria",
         y = "Freqüència") +
    theme_minimal(base_size = 14) + 
    scale_fill_viridis_d() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") # Amaguem la llegenda si l'eix X ja ho diu
  print(grafico_bar)
  dev.off()
}
sink()


# Calculem la mitjana de Tech_ratio per equip i ho endrecem
tech_por_equipo <- dades %>%
  group_by(Team) %>%
  summarise(Tech_ratio_mean = mean(Tech_ratio, na.rm = TRUE))

# Generem el gràfic de barres ordenat
png(filename = "grafics_univariants/barplot_tech_ratio_equip.png", width = 800, height = 600)
ggplot(tech_por_equipo, aes(x = reorder(Team, -Tech_ratio_mean), y = Tech_ratio_mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue", alpha = 0.8) +
  labs(title = "Densitat tecnològica (Tech_ratio) mitjana per franquícia",
       x = "Franquícia de l'NBA",
       y = "Tech_ratio Mitjà (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
