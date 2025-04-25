rm(list = ls())
# Librerías
library(GA)
library(ggplot2)
library(gganimate)
library(magick)
library(gridExtra)
library(plotly)
library(webshot)
library(htmlwidgets)
set.seed(1404)
# -----------------------------------------------------------------------------------------------------------------
# Función de crecimiento von Bertalanffy
von_bertalanffy <- function(params, t) {
  Linf <- params[1]; k <- params[2];
  t0   <- (-1) * (exp(-0.3922 - 0.2752 * log(Linf) - 1.038 * log(k)))
  LTT  <- Linf * (1 - exp(-k * (t - t0)))
  return(LTT)
}

# Datos simulados
Linf  <- 20; k <- 0.99
t_obs <- runif(100, min = 0.1, max = 4)
t_obs <- t_obs[order(t_obs)]
L_obs <- von_bertalanffy(c(Linf, k), t_obs) + rnorm(100, mean = 0, sd = 2)

# Función objetivo (log-verosimilitud)
objective_function <- function(params, t_obs, L_obs) {
  L_pred    <- von_bertalanffy(params, t_obs)
  residuals <- L_obs - L_pred
  if (any(is.nan(residuals)) || any(is.infinite(residuals))) {
    return(-Inf)  # Penalizar soluciones inválidas
  }
  sigma     <- sd(residuals)  # Desviación estándar
  llike     <- -0.5 * length(L_obs) * log(2 * pi * sigma^2) - sum(residuals^2) / (2 * sigma^2)
  return(llike)  # Maximizar log-verosimilitud
}

# Almacenamiento del historial de población
population_history <- list()

# Monitor mejorado para el progreso del GA
track_population <- function(obj) {
  generation <- obj@iter
  population <- obj@population
  fitness <- obj@fitness

  # Crear un data.frame para almacenar estadísticas
  population_data <- data.frame(
    Generation = generation,
    Param1 = population[, 1],  # Linf
    Param2 = population[, 2],  # k
    Fitness = fitness
  )

  # Agregar estadísticas por generación
  summary_stats <- data.frame(
    Generation = generation,
    Mean_Fitness = mean(fitness, na.rm = TRUE),
    Best_Fitness = max(fitness, na.rm = TRUE),
    Mean_Linf = mean(population[, 1], na.rm = TRUE),
    Mean_k = mean(population[, 2], na.rm = TRUE)
  )

  # Guardar en el historial
  population_history[[generation]] <<- list(Population = population_data, Summary = summary_stats)

  cat(sprintf("Generación %d procesada: Mejor aptitud = %.4f\n", generation, max(fitness, na.rm = TRUE)))
}

# Configuración y ejecución del algoritmo genético
ga_result <- ga(
  type = "real-valued",
  fitness = function(params) objective_function(params, t_obs, L_obs),
  lower = c(16, 0.5),
  upper = c(22, 1),
  pcrossover = 0.8,
  pmutation = 0.1,
  popSize = 50,
  maxiter = 50,
  monitor = track_population,
  seed = 123
)

population_history_df <- do.call(rbind, lapply(population_history, function(x) x$Summary))
generation_plots <- lapply(unique(population_history_df$Generation), function(gen) {
  data_gen <- population_history_df[population_history_df$Generation <= gen, ]
  data_gen$Best_top <- max(data_gen$Mean_Fitness, na.rm = TRUE)
  data_gen_arrows <- data_gen[-nrow(data_gen), ]
  data_gen_arrows$Next_Generation <- data_gen$Generation[-1]
  data_gen_arrows$Next_Mean_Fitness <- data_gen$Mean_Fitness[-1]

  ggplot(data_gen, aes(x = Generation)) +
    geom_segment(data = data_gen_arrows,
                 aes(xend = Next_Generation, yend = Next_Mean_Fitness,
                     y = Mean_Fitness, color = "Mean Fitness"),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_line(aes(y = Best_top, color = "Best Fitness"), linetype = 11) +
    # geom_point(aes(y = Best_top, color = "Best Fitness")) +
    labs(title = paste("GA: Generation", gen),
      x = "Generation", y = "Fitness") +
    theme_grey() +
    scale_color_manual(values = c("Mean Fitness" = "#0000ff", "Best Fitness" = "green")) +
    theme(legend.position = "none") +
    xlim(0, 50) + ylim(range(population_history_df$Mean_Fitness) + c(0, 2))
})


# PARAMETROS ------------------------------------------------------------------------------------------------------
param_plots <- lapply(c("Mean_Linf", "Mean_k"), function(param) {
  lapply(unique(population_history_df$Generation), function(gen) {
    data_gen <- population_history_df[population_history_df$Generation <= gen, ]
    ggplot(data_gen, aes(x = Generation, y = .data[[param]])) +
      geom_line(color  = "purple") +
      geom_point(color = "purple", shape = 21, fill = "white") +
      labs(
        title = paste(param, ": Generation", gen),
        x = "Generation", y = param
      ) +
      theme_grey() +
      xlim(0, 50) +
      ylim(range(population_history_df[[param]], na.rm = TRUE))
  })
})

tlgg <- list()
for(g in 1:length(generation_plots)) {
  print(g)
  gp <- generation_plots[[g]]
  pp <- param_plots[[1]][[g]]
  pk <- param_plots[[2]][[g]]
  gl <- list(pp, pk, gp)
  tlgg[[g]] <- grid.arrange(grobs = gl, widths = c(1, 1, 1, 1),
                       layout_matrix = rbind(c(1, 1, 2, 2), c(3, 3, 3, 3)),
  )
}

output_folder <- "cout2"; dir.create(file.path(tempdir(), output_folder), showWarnings = FALSE)
for (i in seq_along(generation_plots)) {
  print(i)
  ggsave(filename = file.path(file.path(tempdir(), output_folder), sprintf("gen_plot_%02d.png", i)),
         plot = tlgg[[i]], width = 7*2, height = 7*2, dpi = 300,
         units = "cm")
}

output_folder <- file.path(tempdir(), output_folder)
image_list <- list.files(output_folder, pattern = "gen_plot_\\d+\\.png", full.names = TRUE)
gif <- image_animate(image_join(lapply(image_list, image_read)), fps = 2)
image_write(gif, path =  "postHub-241222/cout/ga.gif")

best_params <- ga_result@solution
cat("Parámetros óptimos encontrados:\n")
cat("L∞ =", best_params[1], "\n")
cat("k =", best_params[2], "\n")

# -----------------------------------------------------------------------------------------------------------------

# Crear un data.frame con las combinaciones de parámetros y fitness
all_population <- do.call(rbind, lapply(population_history, function(x) x$Population))


ga_result@summary

library(mgcv)
mod <- gam(Fitness ~ te(Param1) + te(Param2), data = all_population)

hp.seq <- seq(min(all_population$Param1, na.rm=TRUE),
              max(all_population$Param1, na.rm=TRUE), length=25)
wt.seq <- seq(min(all_population$Param2, na.rm=TRUE),
              max(all_population$Param2, na.rm=TRUE), length=25)

predfun <- function(x,y){
  newdat <- data.frame(Param1 = x, Param2 = y)
  predict(mod, newdata = newdat)
}

fit <- outer(hp.seq, wt.seq, Vectorize(predfun))

# Asegúrate de que los datos para la línea sean vectores adecuados
x_string <- as.vector(population_history_df$Generation)
x_string <- c(1:5, rep(NA, length(x_string) - 5))
x_line <- as.vector(population_history_df$Mean_Linf)
y_line <- as.vector(population_history_df$Mean_k)
z_line <- as.vector(population_history_df$Mean_Fitness)

p <- plot_ly() %>%
  add_surface(x = ~hp.seq, y = ~wt.seq, z = t(fit)) %>%
  layout(
    title = "3D Surface Plot with Line and Points",
    scene = list(
      xaxis = list(title = "L_inf"),
      yaxis = list(title = "k"),
      zaxis = list(title = "Fitness")
    )
  )

p

q <- plot_ly() %>%
  add_trace(
    type = 'scatter3d',
    mode = 'lines',
    x = x_line,
    y = y_line,
    z = z_line,
    line = list(color = 'black', width = 2)
  ) %>%
  add_trace(
    type = 'scatter3d',
    mode = 'markers',
    x = x_line,
    y = y_line,
    z = z_line,
    marker = list(color = 'blue', size = 5, opacity = 1)
  ) %>%
  add_trace(
    type = 'scatter3d',
    mode = 'text',
    x = x_line,
    y = y_line,
    z = z_line, text = x_string,  # Mostrar texto sobre los puntos
    textposition = 'top center'
  ) %>%
  layout(
    title = "3D Fitness-Generation",
    scene = list(
      xaxis = list(title = "L_inf",
        range = c(min(x_line), max(x_line))  # Rango del eje x
      ),
      yaxis = list(title = "k",
        range = c(min(y_line), max(y_line))  # Rango del eje y
      ),
      zaxis = list(title = "Fitness",
        range = c(min(z_line), max(z_line))  # Rango del eje z
      )
    )
  )

q


