# ==============================================================================
#                 TEST DE NORMALITAT I ANÀLISI BIVARIANT (Capítol 4.3)
# ==============================================================================

# Carreguem llibreries necessàries
# install.packages(c("nortest", "ggplot2", "corrplot", "dplyr"))
library(nortest)
library(ggplot2)
library(corrplot)
library(dplyr)

# Carreguem la base de dades (assegura't de posar el nom correcte del teu arxiu)
dades <- read.csv("BBDD_neta.csv")
dades$New_Coach <- as.factor(dades$New_Coach)

# Creem la carpeta per guardar els gràfics bivariants si no existeix
path <- "grafics_bivariants/"
if (!dir.exists(path)) {
  dir.create(path)
}

# ==============================================================================
# 0. TEST DE NORMALITAT (Kolmogorov-Smirnov amb correcció de Lilliefors)
# ==============================================================================
cat("\n=== TEST DE NORMALITAT PER A LES VICTÒRIES (W) ===\n")
# Aquest test és més robust que el Shapiro-Wilk per a mostres N > 50 (tenim 420)
test_norm <- lillie.test(dades$W)
print(test_norm)
cat("Si el p-value > 0.05, confirmem la normalitat matemàtica.\n\n")

# ==============================================================================
# 1. EL CORRELOGRAMA (L'estrella de l'anàlisi bivariant)
# ==============================================================================
# Seleccionem només les variables numèriques
num_vars <- dades %>% select(where(is.numeric))

# Calculem la matriu de correlacions
cor_matrix <- cor(num_vars, use = "complete.obs")

# Guardem el gràfic. Utilitzem method="color" i afegim els números perquè quedi professional
png(filename = paste0(path, "correlacio.png"), width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", # Només la meitat superior per no duplicar informació
         addCoef.col = "black", # Mostra els números de correlació a dins
         tl.col = "black", # Color del text de les variables
         tl.srt = 45, # Rota els noms a 45 graus
         number.cex = 0.7, # Mida dels números
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200))
dev.off()

# ==============================================================================
# 2. BOXPLOT: VICTÒRIES vs CANVI D'ENTRENADOR (Numèrica vs Categòrica)
# ==============================================================================
# Això ens dirà si fitxar un entrenador nou serveix d'alguna cosa abans de fer el model
png(filename = paste0(path, "boxplot_W_New_Coach.png"), width = 800, height = 600)
ggplot(dades, aes(x = New_Coach, y = W, fill = New_Coach)) +
  geom_boxplot(color = "darkblue", alpha = 0.7) +
  scale_fill_manual(values = c("skyblue", "salmon")) + # Colors per diferenciar el 0 i l'1
  labs(title = "Impacte del canvi d'entrenador en les victòries (W)",
       x = "Nou Entrenador (0 = Manté entrenador, 1 = Entrenador nou)",
       y = "Victòries a la temporada (W)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")
dev.off()

