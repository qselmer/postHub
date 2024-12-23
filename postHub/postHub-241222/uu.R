rm(list = ls())
# librerias -----------------------------------------------------------------------------------------------------------------
library(GA)
# Datos simulados -------------------------------------------------------------------------------------------------
von_bertalanffy <- function(params, t) {
  Linf <- params[1]  # L∞
  k <- params[2]     # k
  t0 <- params[3]    # t0
  LTT <- Linf * (1 - exp(-k * (t - t0)))
  return(LTT)
}

Linf <- 20; k    <- 0.99; t0   <- -0.25

set.seed(123)
t_obs <- runif(100, min = 0.2, max = 4)
t_obs <- t_obs[order(t_obs)]
L_obs <- von_bertalanffy(c(Linf, k, t0), t_obs) + rnorm(100, mean = 0, sd = 2)
plot(t_obs, L_obs, pch = 19, col = "blue", ylim = c(0, Linf),
     xlab = "Edad (t)", ylab = "Longitud (L)",
     main = "Ajuste del Modelo de Von Bertalanffy")

objective_function <- function(params) {
  # Predicciones basadas en los parámetros actuales
  L_pred <- von_bertalanffy(params, t_obs)

  # Calcular los errores residuales
  residuals <- L_obs - L_pred

  # Evitar resultados numéricamente inestables
  if (any(is.nan(residuals)) || any(is.infinite(residuals))) {
    return(-Inf)  # Penalizar soluciones inválidas
  }

  # Supongamos una distribución normal de errores
  sigma <- sd(residuals)  # Estimación de la desviación estándar
  log_likelihood <- -0.5 * length(L_obs) * log(2 * pi * sigma^2) - sum(residuals^2) / (2 * sigma^2)

  return(log_likelihood)  # Maximizar la log-verosimilitud
}


# Definir una función para rastrear parámetros y fitness
track_population <- function(obj) {
  generation <- obj@iter          # Número de la generación actual
  population <- obj@population    # Parámetros de la población
  fitness <- obj@fitness          # Valores de fitness para la población

  # Combinar parámetros y fitness en un data frame
  population_data <- data.frame(
    Generation = generation,
    matrix(population, ncol = ncol(population)),  # Parámetros
    Fitness = fitness
  )

  # Renombrar las columnas de los parámetros
  colnames(population_data)[2:(ncol(population_data) - 1)] <- paste0("Param", 1:(ncol(population_data) - 1))

  # Agregar a una lista global
  population_history[[generation]] <<- population_data
  cat(sprintf("Generación %d procesada\n", generation))
}

# Lista global para guardar datos de todas las generaciones
population_history <- list()

# Ejecutar el algoritmo genético con la función personalizada de monitoreo
ga_result <- ga(
  type = "real-valued",
  fitness = function(params) objective_function(params),
  lower = c(16, 0.5, -5),      # Límites inferiores
  upper = c(22, 1, 5),         # Límites superiores
  popSize = 50,                 # Tamaño de la población
  maxiter = 20,                 # Número de generaciones
  monitor = track_population,   # Función de monitoreo
  seed = 123                    # Semilla para reproducibilidad
)

# Combinar todas las generaciones en un único data frame
population_history_df <- do.call(rbind, population_history)

# Ver las primeras filas
tail(population_history_df)


# Resultados
summary(ga_result)

# Obtener los parámetros óptimos
best_params <- ga_result@solution
cat("Parámetros óptimos encontrados:\n")
cat("L∞ =", best_params[1], "\n")
cat("k =", best_params[2], "\n")
cat("t0 =", best_params[3], "\n")

# Visualización de los datos ajustados
# Calcular valores ajustados
L_pred_opt <- von_bertalanffy(best_params, t_obs)

# Gráfica de comparación

lines(t_obs, L_pred_opt, col = "red", lwd = 2, type = "p")
legend("bottomright", legend = c("Observado", "Ajustado"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))

