# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Script        : 250503_r4fish_code
# Descripción   : OBIS y Darwin Core: datos abiertos para la biodiversidad marina
# Versión       : 1.0.0
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Autor         : Elmer Quispe-Salazar
# Contacto      : qselmer@gmail.com | https://github.com/qselemrer
# Palabras clave: OBIS, Darwin Core, R
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Apóyanos para seguir aprendiendo y compartiendo ciencia abierta:
# Support us to keep learning and sharing open science:
#
#    > buymeacoffee ☕: https://buymeacoffee.com/r4fish
#    > ko-fi ❤️       : https://ko-fi.com/r4fish
#    > paypal 🤝       : https://paypal.me/qselmer
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls()); gc(reset = TRUE, full = TRUE)

# Librerías -------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(robis)
library(gganimate)

# Descargar datos de ocurrencia del género Sebastes
oc <- occurrence("Thunnus")

unique(oc$scientificName)
unique(oc$basisOfRecord)

# Filtrado de datos y limpieza
data_oc <- oc %>%
  filter(scientificName != "Thunnus") %>%
  filter(!is.na(basisOfRecord)) %>%
  filter(!basisOfRecord %in% c("NomenclaturalChecklist", "D")) %>%
  mutate(
    bor = case_when(
      basisOfRecord %in% c("humanobservation", "Human observation", "HumanObservation") ~ "HumanObservation",
      basisOfRecord %in% c("preservedspecimen", "PreservedSpecimen") ~ "PreservedSpecimen",
      basisOfRecord %in% c("MATERIAL_SAMPLE", "materialSample", "MaterialSample") ~ "MaterialSample",
      basisOfRecord == "MachineObservation" ~ "MachineObservation",
      basisOfRecord == "Occurrence" ~ "Occurrence",
      basisOfRecord == "LivingSpecimen" ~ "LivingSpecimen",
      basisOfRecord == "O" ~ "Other",
      TRUE ~ basisOfRecord
    )
  ) %>%
  select(lon = decimalLongitude,
         lat = decimalLatitude,
         sc = scientificName,
         bor,
         year = date_year)

# Tabla de frecuencias por especie y tipo de registro
table_oc <- data_oc %>%
  count(sc, bor) %>%
  rename(Especie = sc, Freq = n)

# Ordenar especies por frecuencia total
orden_especies <- table_oc %>%
  group_by(Especie) %>%
  summarise(Total = sum(Freq), .groups = "drop") %>%
  arrange(desc(-1*Total)) %>%
  pull(Especie)

# Asignar orden a las especies
table_oc$Especie <- factor(table_oc$Especie, levels = orden_especies)

orden_bor <- table_oc %>%
  group_by(bor) %>%
  summarise(Total = sum(Freq), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  pull(bor)


table_oc$bor  <- factor(table_oc$bor, levels = orden_bor)
table_oc$Freq <- as.numeric(table_oc$Freq)/1000

paleta <- colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(table_oc$bor)))
paleta <- rev(paleta)

ggplot(table_oc, aes(x = Especie, y = Freq, fill = bor)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(
    title = "N° records of Thunnus species",
    x = "Species", y = "Frequency (x1000)",
    fill = "Type of record"
  ) +
  theme_cowplot() +
  scale_fill_manual(values = paleta) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(face = "italic")
  ) + coord_flip()

ggsave("plot_scomber_records.png",
       plot = last_plot(),  # o reemplaza con `p` si guardaste el plot en un objeto
       width = 16*1.2, height = 10*1.2, dpi = 300, units = "cm")

# -----------------------------------------------------------------------------------------------------------------
data_oc2 <- data_oc %>%
  filter(bor %in% c("HumanObservation", "Ocurrence")) %>%
  filter(!is.na(lon) & !is.na(lat) & year > 1985) %>%
  filter(sc %in% rev(orden_especies)[1:3])

data_oc2$year <- as.integer(data_oc2$year)
years_seq <- sort(unique(data_oc2$year))

data_acum <- lapply(years_seq, function(y) {
  data_oc2 %>% filter(year <= y) %>% mutate(year_acum = y)
}) %>% bind_rows()

paleta <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(data_acum$sc))))

world_map <- map_data("world")

p <- ggplot() +
  theme_cowplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "#1a1a1a", color = "#4d4d4d", linewidth = 0.1) +
  geom_point(data = data_acum, aes(x = lon, y = lat, color = sc),
             size = 1.5, alpha = 0.6) +
  scale_color_manual(values = paleta) +
  labs(
    title = "Distribución acumulada de registros de
    *Thunnus* (Año: {closest_state})",
    x = "Longitud", y = "Latitud", color = "Especie"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  ) +
  transition_states(year_acum, transition_length = 1, state_length = 1) +
  ease_aes("linear")

# Animar
animate(p, nframes = length(years_seq), fps = 4,
        width = 900, height = 600,
        renderer = gifski_renderer())

anim_save("mapa_ocurr_Thunnus.gif")
