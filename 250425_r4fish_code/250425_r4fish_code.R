rm(list = ls()); gc(reset = TRUE)
# Librerias -------------------------------------------------------------------------------------------------------
library(ncdf4)
library(tidyverse)
library(viridis)
# install.packages("reticulate")
library(reticulate)
library(rnaturalearth)
library(rnaturalearthdata)

# virtualenv_create(envname = "CopernicusMarine")
# virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
#
# reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
# cmt <- import("copernicusmarine")
# cmt$login("equispesalazar1", "Komorebi@1991")
#
setwd("J:/")
# -----------------------------------------------------------------------------------------------------------------
reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
cmt <- import("copernicusmarine")

cmt$subset(
  dataset_id="cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1M-m",
  variables        = list("sea_surface_temperature_anomaly"),
  minimum_longitude= -88,
  maximum_longitude= -66,
  minimum_latitude = -20,
  maximum_latitude = 2,
  start_datetime   = "2022-12-01T00:00:00",
  end_datetime     = "2025-03-01T00:00:00",
  output_directory = "J:/"
)
# -----------------------------------------------------------------------------------------------------------------

filenc <- "cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1M-m_sea_surface_temperature_anomaly_88.00W-66.00W_20.00S-2.00N_2022-12-01-2025-03-01.nc"
nc <- nc_open(filenc)
print(nc)

# Extraer las variables
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
time <- ncvar_get(nc, "time")
atsm <- ncvar_get(nc, "sea_surface_temperature_anomaly")  # dimensiones: [lon, lat, time]

# Extraer nombre de tiempo y convertir a formato de fecha
time_units <- ncatt_get(nc, "time", "units")$value
start_date <- sub("hours since ", "", time_units)
time_dates <- as.POSIXct(start_date, format="%Y-%m-%d", tz = "UTC") + time * 3600

nc_close(nc)
# Creamos un dataframe para cada mes
df_list <- list()

for (i in 1:length(time_dates)) {
  print(time_dates[i])
  # Convertir [tob] en un dataframe por mes
  df <- expand.grid(lon = lon, lat = lat)
  df$atsm <- as.vector(atsm[, , i])  # extraemos la temperatura para el mes i

  # Añadir la fecha del mes
  df$time <- time_dates[i]

  # Añadir el dataframe a la lista
  df_list[[i]] <- df
}

df_all <- bind_rows(df_list)
range(df_all$atsm, na.rm = TRUE)
df_all$atsm[df_all$atsm < -2.5] <- -2.5
df_all$atsm[df_all$atsm > 5] <- 5

# quantile(df_all$atsm, na.rm = T, probs = c(.005, 0.995))

world <- ne_countries(scale = "medium", returnclass = "sf")


# Paleta adaptada al rango -2.5 a 5
colores_ostia <- c(
  "#08306B",  # azul oscuro (~ -2.5)
  "#2171B5",  # azul medio (~ -1.5)
  "#6BAED6",  # celeste (~ -0.5)
  "#F7FBFF",  # blanco (0)
  "#FD8D3C",  # naranja (~1.5)
  "#F03B20",  # rojo claro (~3)
  "#BD0026",  # rojo oscuro (~4)
  "#800026"   # rojo intenso (~5)
)

# Valores correspondientes al rango -2.5 a 5, normalizados
valores_ostia <- scales::rescale(c(-2.5, -1.5, -0.5, 0, 1.5, 3, 4, 5), to = c(0, 1))

# Crear una carpeta temporal para guardar imágenes
dir.create("plots_atsm", showWarnings = FALSE)

# Crear un gráfico por cada mes
df_all %>%
  split(.$time) %>%
  imap(function(data_mes, mes) {
    p <- ggplot() +
      geom_tile(data = data_mes, aes(x = lon, y = lat, fill = atsm)) +
      scale_fill_gradientn(
        colors = colores_ostia,
        values = valores_ostia,
        limits = c(-2.5, 5),
        oob = scales::squish,
        name = "ATSM (°C)",
        breaks = c(-2.5, -1.5, -0.5, 0, 1.5, 3, 4, 5),
        labels = c("≤ -2.5", "-1.5", "-0.5", "0", "1.5", "3", "4", "≥ 5")
      ) +
      geom_sf(data = world, fill = "grey80", color = "black", size = 0.2) +
      coord_sf(
        xlim = range(df_all$lon, na.rm = TRUE),
        ylim = c(-20, 0),
        expand = FALSE
      ) +
      labs(
        title = paste("ATSM mensual -", mes),
        x = "Longitud (°E)", y = "Latitud (°N)"
      ) +
      theme_minimal() +
      theme(legend.position = "right")

    # Guardar imagen
    ggsave(filename = paste0("plots_atsm/atsm_", mes, ".png"),
           plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  })

require(magick)
# Crear un gif a partir de las imágenes
# # Leer las imágenes y crear el GIF
imgs <- list.files("plots_atsm", pattern = "*.png", full.names = TRUE)
gif <- image_read(imgs) %>% image_animate(fps = 1)

# Guardar el GIF
image_write(gif, "anomalía_atsm.gif")
