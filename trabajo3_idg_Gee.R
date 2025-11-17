## ============================================================
## 1. LIBRERÍAS
## ============================================================
library(factoextra)
library(ggfortify)
library(plotly)
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(GGally)
library(dplyr)

## ============================================================
## 2. ENTRADA: ZONAS CON BRECHA DIGITAL (DESDE TRABAJO 2)
## ============================================================

# Capa exportada en el Trabajo 2
# Debe existir el archivo: output/zonas_gs_bamovil.geojson
zonas_gs_bamovil <- st_read("output/zonas_gs_bamovil.geojson")

# Revisión rápida
print(zonas_gs_bamovil)
names(zonas_gs_bamovil)
# Esperado: geocodigo, nom_comuna, nom_provin, nom_zona, urbano, porc_bamovil, geometry

## ============================================================
## 3. CONEXIÓN A BD: CENSO 2017 RM
## ============================================================

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "censo2017_rm",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "Kurt12345*"
)

## ============================================================
## 4. CONSULTA SQL: INDICADORES SOCIODEMOGRÁFICOS
## ============================================================

sql_indicadores <- "
SELECT
  z.geocodigo AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
    2
  ) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor o igual a 16 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
    2
  ) AS ptje_esc_mayor_16

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_16 DESC;
"

# Ejecutar consulta y traer a R
df_indicadores <- dbGetQuery(con, sql_indicadores)

# Revisión rápida
head(df_indicadores)

## ============================================================
## 5. UNIÓN: BRECHA DIGITAL + INDICADORES
## ============================================================

# Asegurar tipos compatibles para el join
zonas_gs_bamovil$geocodigo <- as.character(zonas_gs_bamovil$geocodigo)
df_indicadores$geocodigo   <- as.character(df_indicadores$geocodigo)

# Unir por geocodigo (zona censal)
zonas_gs_indicadores <- zonas_gs_bamovil %>%
  left_join(df_indicadores, by = "geocodigo")

# Resolver nombres duplicados de comuna: nom_comuna.x (geojson), nom_comuna.y (SQL)
zonas_gs_indicadores <- zonas_gs_indicadores %>%
  mutate(
    nom_comuna = dplyr::coalesce(nom_comuna.x, nom_comuna.y)
  ) %>%
  select(
    tid,
    geocodigo,
    nom_comuna,
    nom_provin,
    nom_zona,
    urbano,
    porc_bamovil,
    ptje_migrantes,
    ptje_esc_mayor_16,
    geometry
  )

# Eliminar filas con NA en variables clave
zonas_gs_indicadores <- zonas_gs_indicadores %>%
  filter(
    !is.na(porc_bamovil),
    !is.na(ptje_migrantes),
    !is.na(ptje_esc_mayor_16)
  )

# Revisión rápida sin geometría
zonas_gs_indicadores %>%
  st_drop_geometry() %>%
  head()

## ============================================================
## 6. PREPARACIÓN DE VARIABLES PARA CLUSTERING
## ============================================================

# Seleccionar variables numéricas para el análisis
vars_clusters <- zonas_gs_indicadores %>%
  st_drop_geometry() %>%
  dplyr::select(
    porc_bamovil,        # indicador de brecha digital / acceso BAMóvil
    ptje_migrantes,      # % migrantes
    ptje_esc_mayor_16    # % con >= 16 años de escolaridad
  )

# Escalamiento de variables
vars_scaled <- scale(vars_clusters)

# Revisión
summary(vars_clusters)
summary(vars_scaled)

## ============================================================
## 7. ELECCIÓN DE K Y K-MEANS
## ============================================================

# Método del codo para elegir K
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(
    title = "Método del codo",
    x = "Número de clusters (k)",
    y = "Suma de cuadrados intra-cluster (WSS)"
  )

# Fijamos K = 4 según el quiebre observado y el interés en diferenciar más perfiles
set.seed(123)
km <- kmeans(vars_scaled, centers = 4, nstart = 25)

# Agregar cluster a la capa espacial
zonas_gs_indicadores$cluster <- as.factor(km$cluster)

# Resumen por cluster (promedio de variables en cada grupo)
aggregate(
  vars_clusters,
  by = list(cluster = zonas_gs_indicadores$cluster),
  FUN = mean
)

## ============================================================
## 8. GRÁFICOS DE DISPERSIÓN
## ============================================================

# Escolaridad vs Migrantes
ggplot(
  zonas_gs_indicadores %>% st_drop_geometry(),
  aes(x = ptje_esc_mayor_16, y = ptje_migrantes, color = cluster)
) +
  geom_point(size = 2) +
  labs(
    title = "Escolaridad vs Migración",
    x = "% Población con >= 16 años de escolaridad",
    y = "% Población migrante"
  ) +
  theme_minimal()

# Escolaridad vs Brecha Digital (BAMóvil)
ggplot(
  zonas_gs_indicadores %>% st_drop_geometry(),
  aes(x = ptje_esc_mayor_16, y = porc_bamovil, color = cluster)
) +
  geom_point(size = 2) +
  labs(
    title = "Escolaridad vs Acceso a Internet Móvil",
    x = "% Población con >= 16 años de escolaridad",
    y = "% Hogares con acceso a BAMóvil"
  ) +
  theme_minimal()

# Parejas de variables con GGally (opcional)
GGally::ggpairs(
  zonas_gs_indicadores %>%
    st_drop_geometry() %>%
    dplyr::select(porc_bamovil, ptje_migrantes, ptje_esc_mayor_16, cluster),
  aes(color = cluster)
)

## ============================================================
## 9. MAPA DE CLUSTERS
## ============================================================

# Geometría comunal (para referencia)
sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"

sf_comunas_santiago <- st_read(con, query = sql_comunas)

# Bounding box del área de estudio
bbox <- st_bbox(zonas_gs_indicadores)

# Mapa de clusters
mapa_clusters <- ggplot() +
  geom_sf(
    data = zonas_gs_indicadores,
    aes(fill = cluster),
    color = NA
  ) +
  geom_sf(
    data = sf_comunas_santiago,
    fill = NA,
    color = "black",
    size = 0.4
  ) +
  geom_sf_text(
    data = st_centroid(sf_comunas_santiago),
    aes(label = nom_comuna),
    size = 2,
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Clusters de Zonas Censales según Brecha Digital y Condiciones Sociodemográficas",
    subtitle = "Provincia de Santiago, Región Metropolitana"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

print(mapa_clusters)

## ============================================================
## Cálculo del Índice de Shannon por comuna según los clusters
## ============================================================

# Función Shannon
shannon_index <- function(x) {
  x <- x[x > 0]
  -sum(x * log(x))
}

# Calcular proporciones por comuna
tabla_clusters <- zonas_gs_indicadores %>%
  st_drop_geometry() %>%
  group_by(nom_comuna) %>%
  count(cluster) %>%
  group_by(nom_comuna) %>%
  mutate(prop = n / sum(n))

# Aplicar índice de Shannon por comuna
shannon_comunas <- tabla_clusters %>%
  summarise(H = shannon_index(prop)) %>%
  arrange(desc(H))

print(shannon_comunas)

## ============================================================
## ÍNDICE DE SHANNON POR COMUNA Y MAPA
## ============================================================

library(viridis)

## 1. Función índice de Shannon (ya definida arriba) ------------

shannon_index <- function(p) {
  p <- p[p > 0 & !is.na(p)]
  -sum(p * log(p))
}

## 2. Tabla de proporciones por comuna y cluster ---------------

tabla_clusters <- zonas_gs_indicadores %>%
  st_drop_geometry() %>%
  group_by(nom_comuna, cluster) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

## 3. Cálculo del índice de Shannon por comuna -----------------

shannon_comunas <- tabla_clusters %>%
  summarise(H = shannon_index(prop)) %>%
  ungroup()

# Revisión rápida
print(shannon_comunas %>% arrange(desc(H)) %>% head())

## 4. Geometría comunal (a partir de las zonas censales) -------

comunas_geom <- zonas_gs_indicadores %>%
  group_by(nom_comuna) %>%
  summarise(
    geometry = sf::st_union(geometry),
    .groups = "drop"
  ) %>%
  st_as_sf()

## 5. Unir índice de Shannon a la geometría --------------------

comunas_shannon <- comunas_geom %>%
  left_join(shannon_comunas, by = "nom_comuna")

## 6. Mapa del índice de Shannon -------------------------------

mapa_shannon <- ggplot(comunas_shannon) +
  geom_sf(aes(fill = H), color = "white", size = 0.3) +
  scale_fill_viridis_c(
    option = "plasma",
    name   = "Índice de Shannon (H)"
  ) +
  labs(
    title    = "Índice de Shannon por comuna",
    subtitle = "Diversidad interna de clusters socio-digitales\nProvincia de Santiago, Región Metropolitana",
    caption  = "Fuente: Elaboración propia en base a Censo 2017 y clusters de brecha digital"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text     = element_blank(),
    axis.title    = element_blank(),
    panel.grid    = element_blank()
  )

print(mapa_shannon)

## 7. (Opcional) Exportar capa con el índice de Shannon --------

if (!dir.exists("output")) dir.create("output")

st_write(
  comunas_shannon,
  "output/comunas_shannon.geojson",
  delete_dsn = TRUE
)

## ============================================================
## 10. CIERRE DE CONEXIÓN
## ============================================================

dbDisconnect(con)
