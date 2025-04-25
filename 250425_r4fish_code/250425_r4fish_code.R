# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Script        : 250425_r4fish_code
# Descripción   : Ejemplo de uso de la Copernicus Marine Toolbox en R
# Versión       : 1.0.0
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Autor         : Elmer Quispe-Salazar
# Idea          : https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r
# Contacto      : qselmer@gmail.com | https://github.com/qselemrer
# Palabras clave: Copernicus, Oceanografía, R, Marine Toolbox, gif
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Apóyanos para seguir aprendiendo y compartiendo ciencia abierta:
# Support us to keep learning and sharing open science:
#
#    > buymeacoffee ☕: https://buymeacoffee.com/r4fish
#    > ko-fi ❤️       : https://ko-fi.com/r4fish
#    > paypal 🤝       : https://paypal.me/qselmer
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls()); gc(reset = TRUE)

# Librerias -------------------------------------------------------------------------------------------------------
library(ncdf4)
require(magick)
library(viridis)
library(lubridate)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
# install.packages("reticulate")
library(reticulate)

# Interfaz con entornos virtuales de Python: CopernicusMarine --------------------------------
# Crear un entorno virtual llamado "CopernicusMarine"
# virtualenv_create(envname = "CopernicusMarine")
# Instalar el paquete 'copernicusmarine' en el entorno creado
# virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
# Usar el entorno virtual "CopernicusMarine"
# reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
# Importar el módulo 'copernicusmarine'
# cmt <- import("copernicusmarine")
# Iniciar sesión en el servicio con tus credenciales
# cmt$login("<username>", "<password>")



# Descargar datos desde Copernicus Marine (necesita entorno y login activo) -----
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wd)

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
cmt <- import("copernicusmarine")

# Descargar subconjunto de datos de anomalía de TSM
cmt$subset(
  dataset_id="cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1M-m",
  variables        = list("sea_surface_temperature_anomaly"),
  minimum_longitude= -88,
  maximum_longitude= -66,
  minimum_latitude = -20,
  maximum_latitude = 2,
  start_datetime   = "2022-12-01T00:00:00",
  end_datetime     = "2025-03-01T00:00:00",
  output_directory = wd,
)

# Leer y procesar archivo NetCDF --------------------------------------
setwd(wd)

filenc <- "cmems_mod_glo_phy_anfc_0.083deg-sst-anomaly_P1M-m_sea_surface_temperature_anomaly_88.00W-66.00W_20.00S-2.00N_2022-12-01-2025-03-01.nc"
nc <- nc_open(filenc)
print(nc)

# Extraer variables
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
time <- ncvar_get(nc, "time")
atsm <- ncvar_get(nc, "sea_surface_temperature_anomaly")  # dimensiones: [lon, lat, time]

# Convertir tiempo a formato de fecha
time_units <- ncatt_get(nc, "time", "units")$value
start_date <- sub("hours since ", "", time_units)
time_dates <- as.POSIXct(start_date, format="%Y-%m-%d", tz = "UTC") + time * 3600

nc_close(nc)

# Graficar mapas mensuales y guardar como PNG ------------------------------------------------------------------

# Creamos data.frames para cada mes
df_list <- list()

for (i in 1:length(time_dates)) {
  print(time_dates[i])
  df <- expand.grid(lon = lon, lat = lat)
  df$atsm <- as.vector(atsm[, , i])
  df$time <- time_dates[i]
  df_list[[i]] <- df
}

df_all <- bind_rows(df_list)

# Ajustar valores extremos para mejorar la visualización
range(df_all$atsm, na.rm = TRUE)
df_all$atsm[df_all$atsm <= -4] <- -4
df_all$atsm[df_all$atsm >= 5] <- 5

# Cargar mapa base y definir paleta de colores
world <- ne_countries(scale = "medium", returnclass = "sf")

colores_ostia <- c(
  "#08306B", "#2171B5", "#6BAED6", "#F7FBFF",
  "#FD8D3C", "#F03B20", "#BD0026", "#800026"
)
valores_ostia <- scales::rescale(c(-4, -3, -1.5, 0, 1.5, 3, 4, 5), to = c(0, 1))

# Crear carpeta para guardar mapas
dir.create("plots_atsm", showWarnings = FALSE)

# mapas

df_all %>%
  split(.$time) %>%
  imap(function(data_mes, mes) {
    p <- ggplot() +
      geom_tile(data = data_mes, aes(x = lon, y = lat, fill = atsm)) +
      scale_fill_gradientn(
        colors = colores_ostia,
        values = valores_ostia,
        limits = c(-4, 5),
        oob = scales::squish,
        name = "ATSM (°C)",
        breaks = c(-4, -3, -1.5, 0, 1.5, 3, 4, 5),
        labels = c("<=-4", "  -3", "  -1.5",
                   "  0",
                   " +1.5", "  +3", "  +4", ">=+5")
      ) +
      geom_sf(data = world, fill = "grey80", color = "black", size = 0.2) +
      coord_sf(
        xlim = range(df_all$lon, na.rm = TRUE),
        ylim = c(-20, 0),
        expand = FALSE
      ) +
      labs(
        title = paste("ATSM mensual -", format(as.Date(mes) - 1, "%B %Y")),
        x = "Longitud (°E)", y = "Latitud (°N)"
      ) +
      theme_minimal() +
      theme(
        legend.position    = "right",
        legend.key.height  = unit(1, "cm"),    # Altura de cada recuadro de la leyenda
        legend.key.width   = unit(0.5, "cm"),  # Ancho de cada recuadro de la leyenda
        legend.title       = element_text(size = 12),
        legend.text        = element_text(size = 10)
      )

    ggsave(filename = paste0("plots_atsm/atsm_", mes, ".png"),
           plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  })


# GIF animado a partir de los mapas generados -----------------------
imgs <- list.files("plots_atsm", pattern = "*.png", full.names = TRUE)
gif <- image_read(imgs) %>% image_animate(fps = 1)
image_write(gif, "atsm_2022-12-01_to_2025-03-01.gif")
