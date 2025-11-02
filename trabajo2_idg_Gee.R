## ============================================================
## 1. LIBRERÍAS ####
## ============================================================
library(rakeR)
library(RPostgres)
library(DBI)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(stringr)
library(cowplot)

## ============================================================
## 2. ENTRADAS ####
## ============================================================

ruta_casen = "C:/Users/Kurta/Desktop/Universidad/Geomarketing/Trabajo 2/casen_rm (1).rds"
ruta_censo = "C:/Users/Kurta/Desktop/Universidad/Geomarketing/Trabajo 2/cons_censo_df (1).rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)

## ============================================================
## 3. PRE-PROCESAMIENTO ####
## ============================================================

### 3.1 CENSO ####
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

### 3.2 CASEN ####
vars_base = c("estrato", "esc", "edad", "sexo", "e6a", "r17b")
casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

casen$esc  = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a  = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$r17b = as.integer(unclass(casen$r17b))

# Variable binaria: 1 = acceso a internet móvil
casen$r17b_bin = ifelse(casen$r17b == 1, 1L,
                        ifelse(casen$r17b == 2, 0L, NA_integer_))

# Imputación lineal de esc ~ e6a
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na, ])
pred = predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

casen$ID = as.character(seq_len(nrow(casen)))

# Categorizaciones
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat = factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)

## ============================================================
## 4. MICROSIMULACIÓN ####
## ============================================================

cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  if (!zona %in% names(inds_list)) return(NULL)
  
  tmp = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  out = try({
    w_frac  = weight(cons = cons_i, inds = inds_i,
                     vars = c("Edad","Escolaridad","Sexo"))
    sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
    merge(sim_i,
          tmp[, c("ID","r17b_bin")],
          by = "ID", all.x = TRUE)
  }, silent = TRUE)
  
  if (inherits(out, "try-error")) return(NULL)
  out
})

sim_list = Filter(Negate(is.null), sim_list)
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

zonas_internet = aggregate(
  r17b_bin ~ zone,
  data = sim_df,
  FUN  = function(x) mean(x, na.rm = TRUE) * 100
)
names(zonas_internet) <- c("geocodigo", "porc_bamovil")

## ============================================================
## 5. CONEXIÓN A BD ####
## ============================================================

con = dbConnect(
  RPostgres::Postgres(),
  dbname   = "censo2017_rm",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "Kurt12345*"
)

DBI::dbWriteTable(
  con,
  name = DBI::SQL("output.zonas_internet_tmp"),
  value = zonas_internet,
  row.names = FALSE,
  overwrite = TRUE
)

query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
    nom_provin = 'SANTIAGO' OR
    nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
)"
zonas_gs = sf::st_read(con, query = query_gs, quiet = TRUE)

zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)
zonas_gs_internet = left_join(zonas_gs, zonas_internet, by = "geocodigo")

sf::st_write(
  obj   = zonas_gs_internet,
  dsn   = con,
  layer = "output.zc_bamovil_microsim",
  append = FALSE
)
dbDisconnect(con)

## ============================================================
## 6. AGRUPACIÓN POR COMUNA ####
## ============================================================

geom_col <- attr(zonas_gs_internet, "sf_column")

zonas_comunas_internet <- zonas_gs_internet |>
  dplyr::group_by(nom_comuna) |>
  dplyr::summarise(
    porc_bamovil = mean(porc_bamovil, na.rm = TRUE),
    geometry = sf::st_union(.data[[geom_col]]),
    .groups = "drop"
  ) |>
  sf::st_as_sf()

## ============================================================
## 7. ORDENAR COMUNAS DE MAYOR A MENOR ####
## ============================================================

zonas_comunas_internet <- zonas_comunas_internet |>
  dplyr::arrange(desc(porc_bamovil)) |>
  dplyr::mutate(
    nom_comuna = factor(nom_comuna, levels = unique(nom_comuna)),
    label_comuna = paste0(
      stringr::str_to_title(nom_comuna),
      " (", round(porc_bamovil, 1), "%)"
    )
  )

## ============================================================
## 8. MAPA SIN LEYENDA ####
## ============================================================

mapa_comunas <- ggplot(zonas_comunas_internet) +
  geom_sf(aes(fill = label_comuna), color = "white", size = 0.25) +
  scale_fill_viridis_d(
    option = "plasma",
    name = "Comuna (% hogares con acceso)"
  ) +
  labs(
    title = "Acceso a Internet por Banda Ancha Móvil\nComunas del Gran Santiago",
    subtitle = "Cada color representa una comuna distinta según su porcentaje de acceso",
    caption = "Fuente: Elaboración propia con datos CASEN y CENSO"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5,
                              lineheight = 1.1, margin = margin(b = 6, t = 5)),
    plot.subtitle = element_text(size = 10, hjust = 0.5,
                                 margin = margin(b = 5)),
    plot.caption = element_text(size = 9, hjust = 0.5,
                                margin = margin(t = 10)),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(5, 10, 5, 10)
  ) +
  coord_sf(expand = FALSE)

mapa_comunas

## ============================================================
## 9. LEYENDA ORDENADA DE MAYOR A MENOR ####
## ============================================================

# Crear una versión de label_comuna como factor ordenado (mayor → menor)
zonas_comunas_internet <- zonas_comunas_internet |>
  dplyr::arrange(desc(porc_bamovil)) |>
  dplyr::mutate(
    label_comuna = factor(
      paste0(stringr::str_to_title(nom_comuna), " (", round(porc_bamovil, 1), "%)"),
      levels = paste0(stringr::str_to_title(nom_comuna), " (", round(porc_bamovil, 1), "%)")[order(porc_bamovil, decreasing = TRUE)]
    )
  )

# Generar la leyenda ordenada
leyenda_plot <- ggplot(zonas_comunas_internet) +
  geom_sf(aes(fill = label_comuna)) +
  scale_fill_manual(
    values = viridis::viridis(n = nrow(zonas_comunas_internet), option = "plasma"),
    name = "Comuna (% hogares con acceso)"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.position = "right"
  )

# Función para extraer la leyenda
get_legend <- function(a_plot) {
  ggplotGrob(a_plot)$grobs[[which(sapply(ggplotGrob(a_plot)$grobs,
                                         function(x) x$name) == "guide-box")]]
}

# Extraer y mostrar la leyenda ordenada
leyenda <- get_legend(leyenda_plot)
grid::grid.newpage()
grid::grid.draw(leyenda)