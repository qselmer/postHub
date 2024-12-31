rm(list = ls()); gc(reset = TRUE)
# Instalación y carga de librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(car)) install.packages("car")
library(ggplot2)
library(car)

# Generar datos simulados
set.seed(123) # Asegurar reproducibilidad
datos <- data.frame(
  zona = factor(rep(c("A", "B", "C"), each = 10)), # Grupos
  biomasa = c(rnorm(10, mean = 50, sd = 5),
              rnorm(10, mean = 55, sd = 5),
              rnorm(10, mean = 60, sd = 5)) # Datos simulados
)

# Exploración inicial de los datos
print(head(datos)) # Inspeccionar los primeros registros
summary(datos) # Resumen estadístico

# Gráfico de exploración: Caja por grupo
ggplot(datos, aes(x = zona, y = biomasa, fill = zona)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  theme_minimal() +
  labs(title = "Distribución de Biomasa por Zona",
       x = "Zona",
       y = "Biomasa (kg)") +
  scale_fill_brewer(palette = "Set3")

# Realizar ANOVA
anova_model <- aov(biomasa ~ zona, data = datos)
summary(anova_model) # Resumen de ANOVA

# Supuestos del ANOVA

## 1. Normalidad de los residuos
residuos <- residuals(anova_model)

# Test de Shapiro-Wilk para normalidad
shapiro_test <- shapiro.test(residuos)
print(shapiro_test) # p > 0.05 indica residuos normales

# Gráfico de residuos: Histograma y QQ-Plot
ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15,
                 fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(residuos), sd = sd(residuos)),
                color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Distribución de Residuos", x = "Residuos", y = "Densidad")

ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "QQ-Plot de Residuos")

## 2. Homogeneidad de varianzas (homocedasticidad)
# Test de Levene
levene_test <- leveneTest(biomasa ~ zona, data = datos)
print(levene_test) # p > 0.05 indica varianzas homogéneas

# Gráfico de residuos vs. valores ajustados
ggplot(data.frame(Ajustado = fitted(anova_model), Residuo = residuos), aes(x = Ajustado, y = Residuo)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residuos vs. Valores Ajustados", x = "Valores Ajustados", y = "Residuos")

## 3. Independencia de las observaciones
# Este supuesto depende del diseño experimental. Asegúrate de que los datos sean independientes.

# Comparaciones post-hoc si ANOVA es significativo
if (summary(anova_model)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey <- TukeyHSD(anova_model)
  print(tukey)
  plot(tukey, las = 1) # Gráfico de las comparaciones
} else {
  print("El ANOVA no es significativo, no se requieren comparaciones post-hoc.")
}
