install.packages("DBI")        # Interfaz general para bases de datos
install.packages("RSQLite")    # Para bases de datos SQLite
install.packages("RMariaDB")   # Para MySQL/MariaDB
install.packages("RPostgres")  # Para PostgreSQL
install.packages("RPostgres")

# Cargar entorno si es necesario
load("~/.RData")
# ============================================================
# ============================================================
# 1. Cargar librerías
# ============================================================
libs <- c("sf", "dplyr", "ggplot2", "biscale", "cowplot", "DBI", "RPostgres", "stringr")
lapply(libs, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# ============================================================
# 2. Conexión a PostgreSQL
# ============================================================
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "Vregion",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "Kurt12345*"
)

# ============================================================
# 3. Consultas SQL para las 6 comunas
# ============================================================
comunas_seleccionadas <- c("5101", "5103", "5109", "5801", "5802", "5804")

# Conteo por sexo
sexo_df <- dbGetQuery(con, sprintf("
  SELECT 
    p10comuna AS comuna,
    SUM(CASE WHEN p08 = '1' THEN 1 ELSE 0 END) AS hombres,
    SUM(CASE WHEN p08 = '2' THEN 1 ELSE 0 END) AS mujeres
  FROM personas
  WHERE p10comuna IN (%s)
  GROUP BY p10comuna;
", paste0("'", comunas_seleccionadas, "'", collapse = ",")))

# Conteo de tercera edad (60+)
etario_df <- dbGetQuery(con, sprintf("
  SELECT 
    p10comuna AS comuna,
    SUM(CASE WHEN p09 >= 60 THEN 1 ELSE 0 END) AS tercera_edad
  FROM personas
  WHERE p10comuna IN (%s)
  GROUP BY p10comuna;
", paste0("'", comunas_seleccionadas, "'", collapse = ",")))

# Cerrar conexión
dbDisconnect(con)

# ============================================================
# 4. Superficie por comuna (valores reales o aproximados)
# ============================================================
superficie <- tibble(
  comuna = str_pad(comunas_seleccionadas, width = 5, pad = "0"),
  km2 = c(401.6, 76.0, 121.6, 536.9, 293.8, 96.5)
)

# ============================================================
# 5. Procesar indicadores
# ============================================================
sexo_df <- sexo_df %>% mutate(comuna = str_pad(as.character(comuna), width = 5, pad = "0"))
etario_df <- etario_df %>% mutate(comuna = str_pad(as.character(comuna), width = 5, pad = "0"))
superficie <- superficie %>% mutate(comuna = as.character(comuna))

densidad_df <- sexo_df %>%
  mutate(total = hombres + mujeres) %>%
  left_join(superficie, by = "comuna") %>%
  mutate(densidad_total = round(total / km2, 2)) %>%
  select(comuna, densidad_total)

etario_df <- etario_df %>%
  select(comuna, tercera_edad)

# ============================================================
# 6. Cargar shapefile y corregir códigos
# ============================================================
ruta_shape <- "C:/Users/Kurta/Desktop/Universidad/Geomarketing/DPA/DPA_2023/COMUNAS/COMUNAS_v1.shp"
comunas <- st_read(ruta_shape)

comunas <- comunas %>%
  mutate(CUT_COM = str_pad(CUT_COM, width = 5, pad = "0"))

subset_comunas <- comunas %>%
  filter(CUT_COM %in% densidad_df$comuna) %>%
  left_join(densidad_df, by = c("CUT_COM" = "comuna")) %>%
  left_join(etario_df, by = c("CUT_COM" = "comuna"))

# Verificación
print("Verificación de datos unidos:")
print(subset_comunas %>% select(CUT_COM, densidad_total, tercera_edad))

# ============================================================
# 7. Clasificación bivariada con validación robusta
# ============================================================
subset_comunas_clean <- subset_comunas %>%
  filter(!is.na(densidad_total) & !is.na(tercera_edad)) %>%
  mutate(
    densidad_total = as.numeric(densidad_total),
    tercera_edad = as.numeric(tercera_edad)
  )

n_densidad <- length(unique(na.omit(subset_comunas_clean$densidad_total)))
n_edad <- length(unique(na.omit(subset_comunas_clean$tercera_edad)))

dim_final <- if (n_densidad < 2 || n_edad < 2) {
  stop("No hay suficientes valores únicos para clasificar. Revisa los datos.")
} else if (n_densidad < 3 || n_edad < 3) {
  2
} else {
  3
}

subset_bi <- bi_class(
  subset_comunas_clean,
  x = densidad_total,
  y = tercera_edad,
  style = "quantile",
  dim = dim_final
)

# ============================================================
# 8. Graficar mapa bivariado
# ============================================================
mapa <- ggplot() +
  geom_sf(data = subset_bi, aes(fill = bi_class), color = "white", size = 0.2) +
  bi_scale_fill(pal = "DkBlue", dim = dim_final) +
  labs(
    title = "Mapa bivariado: Densidad poblacional vs población de tercera edad",
    subtitle = "Valparaíso, Concón, Viña del Mar, Quilpué, Limache y Villa Alemana",
    caption = "Fuente: Censo 2017 + Shapefile DPA 2023"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "none"
  )

# ============================================================
# 9. Leyenda bivariada
# ============================================================
leyenda <- bi_legend(
  pal = "DkBlue",
  dim = dim_final,
  xlab = "Mayor densidad poblacional ⟶",
  ylab = "↑ Mayor población de tercera edad",
  size = 7
)

# ============================================================
# 10. Combinar mapa + leyenda
# ============================================================
final <- cowplot::ggdraw() +
  cowplot::draw_plot(mapa, 0, 0, 1, 1) +
  cowplot::draw_plot(leyenda, 0.65, 0.05, 0.3, 0.3)

print(final)

ggsave("mapa_bivariado.png", plot = final, width = 8, height = 6, dpi = 300)

install.packages("DBI", repos = "https://cloud.r-project.org")