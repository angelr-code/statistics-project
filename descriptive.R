df <- read.csv("data/clean_data.csv")

library(corrplot)
library(RColorBrewer)
library(e1071)

correlation_matrix <-abs( cor(df, use = "complete.obs"))

corrplot(correlation_matrix, 
         method = "color",
         type = "lower",
         col = colorRampPalette(brewer.pal(8, "RdBu"))(200),
         addCoef.col = "black", # Correlation values
         tl.col = "black",      # Label text color
         tl.cex = 0.8,          # Label text size
         number.cex = 0.55      # Correlation values size
)

summary(df$age)

hist(df$age, breaks = 20, col = "skyblue", border = "white",
     main = "Age Distribution", xlab = "Age")

skewness(df$age, na.rm = TRUE)
kurtosis(df$age, na.rm = TRUE)

qqnorm(df$age)
qqline(df$age, col = "red")


hist(df$internet_time, breaks = 20, col = "skyblue", border = "white",
     main = "Internet Time", xlab = "Time (?)")

skewness(df$internet_time, na.rm = TRUE)
kurtosis(df$internet_time, na.rm = TRUE)

qqnorm(df$internet_time)
qqline(df$internet_time, col = "red")

# =========================================================
# 💍 Estado civil: agrupar (1,2) y (3,4); dejar 5 aparte
# =========================================================

# Crear variable agrupada
# Grupos:
#  - 1 & 2  -> "Casado/Separado"
#  - 3 & 4  -> "Divorciado/Viudo"
#  - 5      -> "Viudo"
#  - 6      -> "Soltero"
df$marital_group <- NA_integer_
df$marital_group[df$marital_status %in% c(1,2)] <- 12
df$marital_group[df$marital_status %in% c(3,4)] <- 34
df$marital_group[df$marital_status %in% 5]      <- 5
df$marital_group[df$marital_status %in% 6]      <- 6

# Convertir a factor con etiquetas ordenadas
df$marital_group <- factor(df$marital_group,
                           levels = c(12, 34, 5, 6),
                           labels = c("Married", "Divorced", "Widowed", "Single/Never Married"))

# Frecuencias y porcentajes
cat("\n--- Frecuencias (grupos) ---\n")
tabla_grp <- table(df$marital_group)
print(tabla_grp)

cat("\n--- Porcentajes (grupos) ---\n")
porc_grp <- round(prop.table(tabla_grp) * 100, 2)
print(porc_grp)

# --- Barplot con etiquetas debajo y valores encima ---
bar_colors <- c("#66c2a5", "#8da0cb", "#a6d854", "red")
labels_grp <- levels(df$marital_group)

bar_centers <- barplot(tabla_grp,
                       col = bar_colors,
                       main = "Estado civil (agrupado)",
                       xlab = "Categorías agrupadas",
                       ylab = "Frecuencia absoluta",
                       ylim = c(0, max(tabla_grp)*1.25),
                       axes = TRUE,
                       yaxt = "s",
                       names.arg = rep("", length(labels_grp)))  # dejamos hueco

# Etiquetas centradas debajo de cada barra
mtext(side = 1, at = bar_centers, text = labels_grp, line = 1, cex = 0.9)

# Valores encima de cada barra
text(x = bar_centers, y = tabla_grp,
     labels = tabla_grp, pos = 3, cex = 0.95, col = "black", font = 2)

# Comentario rápido
moda_idx <- which.max(tabla_grp)
cat(sprintf("\nCategoría más frecuente: %s (%.2f%%)\n",
            labels_grp[moda_idx], porc_grp[moda_idx]))

cat("\nModelo teórico: Multinomial con 3 categorías (n =", sum(tabla_grp), ")\n")


# =========================================================
# 🎓 Variable: years_edu — Años de educación (análisis univariante)
# =========================================================

v <- df$years_edu

cat("N total:", length(v), "\n")
cat("N válidos:", sum(!is.na(v)), "\n")
cat("N NA:", sum(is.na(v)), "\n\n")

# ---- Estadísticos básicos
cat("Resumen:\n")
print(summary(v))
cat("\nMedia:", round(mean(v, na.rm=TRUE), 3),
    " | Mediana:", round(median(v, na.rm=TRUE), 3),
    " | Desv. Típica:", round(sd(v, na.rm=TRUE), 3),
    " | Mín:", min(v, na.rm=TRUE),
    " | Máx:", max(v, na.rm=TRUE), "\n\n")

# ---- Forma de la distribución
suppressWarnings({
  if (!requireNamespace("e1071", quietly = TRUE)) {
    cat("Nota: paquete 'e1071' no instalado; salto asimetría/curtosis.\n")
    skew <- NA; kurt <- NA
  } else {
    skew <- e1071::skewness(v, na.rm = TRUE, type = 2)
    kurt <- e1071::kurtosis(v, na.rm = TRUE, type = 2)
  }
})
cat("Asimetría (skewness):", round(skew, 3), 
    " | Curtosis:", round(kurt, 3), "\n\n")

# ---- Test de normalidad (Shapiro con muestreo si hay muchos casos)
vv <- v[is.finite(v)]
if (length(vv) >= 3) {
  set.seed(123)
  nmax <- 5000
  vv_s <- if (length(vv) > nmax) sample(vv, nmax) else vv
  sh <- try(shapiro.test(vv_s), silent = TRUE)
  if (!inherits(sh, "try-error")) {
    cat("Shapiro-Wilk (muestra de", length(vv_s), "): W=",
        round(sh$statistic, 3), " p-value=", format(sh$p.value, digits = 3), "\n\n")
  }
}

# ---- Gráficos: histograma + densidad + boxplot + QQ-plot
op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
par(mfrow = c(2,2), mar = c(4,4,2,1))

# Histograma con ejes y densidad
h <- hist(v, breaks = "FD", col = "skyblue", border = "white",
          main = "Años de educación: histograma", xlab = "Años",
          ylim = c(0, max(hist(v, breaks="FD", plot=FALSE)$counts)*1.2))
rug(v)
lines(density(v, na.rm = TRUE), lwd = 2)

# Boxplot horizontal
boxplot(v, horizontal = TRUE, col = "lightgray", border = "black",
        main = "Boxplot de años de educación", xlab = "Años")

# QQ-plot vs Normal
qqnorm(v, main = "QQ-plot vs Normal"); qqline(v, col = "red", lwd = 2)

# Barras (conteo por cada año, útil si es muy discreta)
tab <- table(v)
barplot(tab, col = "steelblue", border = NA,
        main = "Frecuencia por año exacto", xlab = "Años", ylab = "Frecuencia")

# ---- Sugerencia de ajuste teórico
cat("Sugerencia de ajuste:\n",
    "- En encuestas como ESS, 'years_edu' suele ser unimodal, leve cola derecha.\n",
    "- Aproxima bien a una Normal discreta (o Normal truncada al rango plausible 0–40).\n",
    "- Si hubiese cola marcada, considerar Poisson/NegBin con media alta (≈ normal).\n")

# =========================================================
# 🎓 Variable: educ_level — Nivel educativo (ISCED)
# =========================================================

# Etiquetas estándar del ESS (pueden variar según país)
# Ajusta si en tu dataset los códigos difieren
labels_educ <- c(
  "Primaria o menos", 
  "Secundaria baja", 
  "Secundaria alta", 
  "Post-secundaria no universitaria", 
  "Universitaria", 
  "Máster/Doctorado"
)

# Comprobamos los valores únicos
cat("\nValores únicos en educ_level:\n")
print(sort(unique(df$educ_level)))

# Si hay más de 6 niveles, se truncarán al máximo
n_levels <- length(unique(df$educ_level))
if (n_levels > length(labels_educ)) {
  labels_educ <- labels_educ[1:n_levels]
}

# Tabla de frecuencias
tabla_educ <- table(df$educ_level)
porc_educ <- round(prop.table(tabla_educ) * 100, 2)

cat("\n--- Frecuencias absolutas ---\n")
print(tabla_educ)
cat("\n--- Porcentajes (%) ---\n")
print(porc_educ)

# --- Barplot ordenado con etiquetas y valores ---
bar_colors <- c("#b3cde3", "#8c96c6", "#8856a7", "#810f7c", "#4d004b", "#2a003a")
bar_centers <- barplot(tabla_educ,
                       col = bar_colors[1:length(tabla_educ)],
                       main = "Distribución del nivel educativo (ISCED)",
                       xlab = "Nivel educativo",
                       ylab = "Frecuencia absoluta",
                       ylim = c(0, max(tabla_educ) * 1.25),
                       names.arg = rep("", length(tabla_educ)))

# Etiquetas debajo de las barras
mtext(side = 1, at = bar_centers, text = labels_educ[1:length(tabla_educ)], line = 1, cex = 0.8)

# Valores encima de las barras
text(x = bar_centers, y = tabla_educ, labels = tabla_educ, pos = 3, cex = 0.9, font = 2)

# Moda (categoría más frecuente)
moda_idx <- which.max(tabla_educ)
moda_categoria <- labels_educ[moda_idx]
cat("\nModa (nivel más frecuente):", moda_categoria, "\n")

# Comentario descriptivo automático
total <- sum(tabla_educ)
cat(sprintf("\nEl nivel educativo más común es '%s' (%.1f%% del total).\n",
            moda_categoria, porc_educ[moda_idx]))

# Modelo teórico y nota sobre distribución
cat("\nModelo teórico: Distribución ordinal discreta (categorías ISCED).\n")
cat("Observación: la distribución suele concentrarse en 'Secundaria alta' o 'Universitaria',\n",
    "con ligera asimetría a la derecha (más educación superior que primaria).\n")