# ============================================================
# TAREA 2 IDG - GEOMARKETING
# Servicio: Internet fijo hogar (vivienda principal)
# CCIF: 08.3.3.01.01
# EPF (hogar/folio) -> 2-part model -> CASEN imputación REALISTA CALIBRADA
# ============================================================

rm(list = ls())

# ---------------------------
# 0) SETUP
# ---------------------------
base_dir <- "D:/Universidad/Geomarketing/Geom_2025_2s/push_bdd"
setwd(base_dir)

pkgs <- c("haven","dplyr","pROC","ggplot2")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

find_file <- function(fname, root = getwd()) {
  p1 <- file.path(root, fname)
  if (file.exists(p1)) return(p1)
  hits <- list.files(root, pattern = paste0("^", gsub("\\.", "\\\\.", fname), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) stop("No encontré el archivo: ", fname, " dentro de: ", root)
  hits[1]
}

# ---------------------------
# 1) CARGA DE DATOS
# ---------------------------
path_personas <- find_file("base-personas-ix-epf-stata.dta", base_dir)
path_gastos   <- find_file("base-gastos-ix-epf-stata.dta", base_dir)
path_ccif     <- find_file("ccif-ix-epf-stata.dta", base_dir)
path_casen    <- find_file("casen_base_preprocesado.rds", base_dir)

personas <- haven::read_dta(path_personas)
gastos   <- haven::read_dta(path_gastos)
ccif     <- haven::read_dta(path_ccif)  # (no es obligatorio, pero lo dejamos cargado)
casen    <- readRDS(path_casen)

cat("Cargado OK:",
    "\n- personas:", nrow(personas),
    "\n- gastos:", nrow(gastos),
    "\n- casen:", nrow(casen), "\n\n")

# ---------------------------
# 2) PARÁMETROS
# ---------------------------
valores_invalidos <- c(-99, -88, -77)
ccif_internet <- "08.3.3.01.01"
macrozona_gs  <- 2

# detectar columna de monto en base-gastos
amount_col <- intersect(c("gasto","monto","valor","gasto_mensual"), names(gastos))
if (length(amount_col) == 0) stop("No encontré columna de monto en base-gastos. Revisa names(gastos).")
amount_col <- amount_col[1]
cat("Columna monto usada en base-gastos:", amount_col, "\n\n")

# ---------------------------
# 3) EPF: LIMPIEZA + X (Gran Santiago)
# ---------------------------
personas_gs <- personas %>%
  dplyr::mutate(
    macrozona_num = as.numeric(macrozona),
    edad_num      = as.numeric(edad),
    edue_num      = as.numeric(edue),
    sexo_num      = as.numeric(sexo),
    npersonas_num = as.numeric(npersonas),
    ing_hog_num   = as.numeric(ing_disp_hog_hd_ai)
  ) %>%
  dplyr::filter(
    macrozona_num == macrozona_gs,
    !is.na(folio), !is.na(n_linea),
    !is.na(edad_num), !(edad_num %in% valores_invalidos),
    !is.na(edue_num), !(edue_num %in% valores_invalidos),
    !is.na(ing_hog_num), ing_hog_num >= 0,
    !is.na(npersonas_num), npersonas_num > 0
  ) %>%
  dplyr::mutate(
    ing_pc     = ing_hog_num / npersonas_num,
    log_ing_pc = log1p(ing_pc),
    
    sexo_f = factor(sexo_num, levels = c(1,2), labels = c("Hombre","Mujer")),
    
    grupo_escolaridad = cut(
      edue_num,
      breaks = c(-Inf, 12, 14, 16, Inf),
      labels = c("Escolar","Tecnico","Universitaria","Postgrado"),
      right  = TRUE
    ),
    
    rango_edad = cut(
      edad_num,
      breaks = c(0, 29, 44, 64, Inf),
      labels = c("jovenes","adultos_jovenes","adultos","adultos_mayores")
    )
  )

cat("EPF personas GS:", nrow(personas_gs), "\n")

# ---------------------------
# 4) EPF: INTERNET (hogar/folio) desde base-gastos
# ---------------------------
gasto_internet_hog <- gastos %>%
  dplyr::mutate(
    macrozona_num = as.numeric(macrozona),
    ccif_chr      = as.character(ccif),
    monto_num     = as.numeric(.data[[amount_col]])
  ) %>%
  dplyr::filter(macrozona_num == macrozona_gs, ccif_chr == ccif_internet) %>%
  dplyr::group_by(folio) %>%
  dplyr::summarise(gasto_internet = sum(monto_num, na.rm = TRUE), .groups = "drop")

# 1 obs por hogar: n_linea mínimo
epf_hog <- personas_gs %>%
  dplyr::left_join(gasto_internet_hog, by = "folio") %>%
  dplyr::mutate(
    gasto_internet = ifelse(is.na(gasto_internet), 0, gasto_internet),
    incurre_internet = as.integer(gasto_internet > 0)
  ) %>%
  dplyr::group_by(folio) %>%
  dplyr::slice_min(order_by = n_linea, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(sexo_f), !is.na(rango_edad), !is.na(grupo_escolaridad), is.finite(log_ing_pc))

cat("EPF hogares modelables:", nrow(epf_hog), "\n")
prev_epf <- mean(epf_hog$incurre_internet)
cat("Prevalencia EPF (internet>0):", round(prev_epf, 4), "\n\n")

# ---------------------------
# 5) TRAIN/TEST
# ---------------------------
set.seed(123)
idx <- sample(seq_len(nrow(epf_hog)), size = floor(0.8*nrow(epf_hog)))
train <- epf_hog[idx, ]
test  <- epf_hog[-idx, ]

# ---------------------------
# 6) MODELO 2 PARTES
# ---------------------------
# Parte 1: LOGIT
m_logit <- glm(
  incurre_internet ~ sexo_f + rango_edad + grupo_escolaridad + log_ing_pc,
  data = train, family = binomial
)

p_test <- predict(m_logit, newdata = test, type = "response")
roc_obj <- pROC::roc(test$incurre_internet, p_test, quiet = TRUE)
auc_val <- as.numeric(pROC::auc(roc_obj))
cat("AUC (test) logit:", round(auc_val, 3), "\n")

# Parte 2: MONTO condicional con control de outliers
train_pos <- subset(train, gasto_internet > 0)
test_pos  <- subset(test,  gasto_internet > 0)

q99  <- as.numeric(quantile(train_pos$gasto_internet, 0.99, na.rm = TRUE))
tope <- min(q99, 60000)

train_pos <- train_pos %>%
  dplyr::mutate(
    gasto_adj = pmin(gasto_internet, tope),
    log_gasto_adj = log1p(gasto_adj)
  )

m_monto <- lm(
  log_gasto_adj ~ sexo_f + rango_edad + grupo_escolaridad + log_ing_pc,
  data = train_pos
)

# Métricas (test gastadores)
pred_log_pos <- predict(m_monto, newdata = test_pos)
pred_m_pos   <- pmax(0, exp(pred_log_pos) - 1)

mae  <- mean(abs(test_pos$gasto_internet - pred_m_pos), na.rm = TRUE)
rmse <- sqrt(mean((test_pos$gasto_internet - pred_m_pos)^2, na.rm = TRUE))
cat("MAE monto (test, gastadores):", round(mae, 2), "\n")
cat("RMSE monto (test, gastadores):", round(rmse, 2), "\n")

# RMSE 2-part (todos): E[gasto]=p*m_hat
m_hat_all_test <- pmax(0, exp(predict(m_monto, newdata = test)) - 1)
gasto_esp_test <- p_test * m_hat_all_test
rmse_2part <- sqrt(mean((test$gasto_internet - gasto_esp_test)^2, na.rm = TRUE))
cat("RMSE gasto esperado 2-part (test, todos):", round(rmse_2part, 2), "\n\n")

# ---------------------------
# 7) CASEN: PREPARAR X + PREDICCIONES
# ---------------------------
needed_casen <- c("edad","sexo","esc","ypc","ID")
miss <- setdiff(needed_casen, names(casen))
if (length(miss) > 0) stop("Faltan columnas en CASEN: ", paste(miss, collapse = ", "))

casen2 <- casen %>%
  dplyr::mutate(
    edad_num = as.numeric(edad),
    sexo_num = as.numeric(sexo),
    esc_num  = as.numeric(esc),
    ing_pc   = as.numeric(ypc),
    log_ing_pc = log1p(ing_pc),
    
    sexo_f = factor(sexo_num, levels = c(1,2), labels = c("Hombre","Mujer")),
    
    grupo_escolaridad = cut(
      esc_num,
      breaks = c(-Inf, 12, 14, 16, Inf),
      labels = c("Escolar","Tecnico","Universitaria","Postgrado"),
      right  = TRUE
    ),
    
    rango_edad = cut(
      edad_num,
      breaks = c(0, 29, 44, 64, Inf),
      labels = c("jovenes","adultos_jovenes","adultos","adultos_mayores")
    )
  )

# Forzar mismos niveles (evita NA por levels distintos)
casen2$sexo_f <- factor(casen2$sexo_f, levels = levels(epf_hog$sexo_f))
casen2$grupo_escolaridad <- factor(casen2$grupo_escolaridad, levels = levels(epf_hog$grupo_escolaridad))
casen2$rango_edad <- factor(casen2$rango_edad, levels = levels(epf_hog$rango_edad))

ok <- with(casen2, !is.na(sexo_f) & !is.na(grupo_escolaridad) & !is.na(rango_edad) & is.finite(log_ing_pc))

casen2$p_hat <- NA_real_
casen2$log_m_hat <- NA_real_
casen2$m_hat <- NA_real_

casen2$p_hat[ok]     <- predict(m_logit, newdata = casen2[ok, ], type = "response")
casen2$log_m_hat[ok] <- predict(m_monto, newdata = casen2[ok, ])
casen2$m_hat[ok]     <- pmax(0, exp(casen2$log_m_hat[ok]) - 1)

# ---------------------------
# 8) CALIBRACIÓN: igualar prevalencia CASEN a EPF (ajuste intercepto)
# ---------------------------
p0 <- casen2$p_hat
p0 <- pmin(pmax(p0, 1e-6), 1 - 1e-6)
logit_raw <- qlogis(p0)

f <- function(delta) mean(plogis(logit_raw[ok] + delta), na.rm = TRUE) - prev_epf
delta_star <- uniroot(f, interval = c(-6, 6))$root

casen2$p_cal <- NA_real_
casen2$p_cal[ok] <- plogis(logit_raw[ok] + delta_star)

cat("Chequeo prevalencia calibrada (promedio p_cal):",
    round(mean(casen2$p_cal[ok], na.rm = TRUE), 4), "\n\n")

# ---------------------------
# 9) IMPUTACIÓN REALISTA (bootstrap residuos + Bernoulli p_cal)
# ---------------------------
# gasto esperado (técnico)
casen2$gasto_esperado <- casen2$p_cal * casen2$m_hat

# Bootstrap de residuos reales del modelo de monto
resid_pool <- resid(m_monto)
resid_pool <- resid_pool - mean(resid_pool, na.rm = TRUE)

set.seed(123)
eps_boot <- sample(resid_pool, size = nrow(casen2), replace = TRUE)
m_sim_boot <- pmax(0, exp(casen2$log_m_hat + eps_boot) - 1)

set.seed(123)
u <- runif(nrow(casen2))
casen2$gasto_imputado_realista_cal <- ifelse(ok & (u < casen2$p_cal), m_sim_boot, 0)

prev_casen_cal <- mean(casen2$gasto_imputado_realista_cal > 0, na.rm = TRUE)
cat("Prevalencia CASEN imputada calibrada (internet>0):", round(prev_casen_cal, 4), "\n\n")

# ---------------------------
# 10) CHEQUEOS (PPT)
# ---------------------------
epf_pos <- epf_hog$gasto_internet[epf_hog$gasto_internet > 0]
cas_pos <- casen2$gasto_imputado_realista_cal[casen2$gasto_imputado_realista_cal > 0]

cat("EPF percentiles (gastadores):\n")
print(quantile(epf_pos, probs = c(.5,.9,.95,.99), na.rm = TRUE))

cat("\nCASEN imputado REALISTA CALIBRADO percentiles (gastadores):\n")
print(quantile(cas_pos, probs = c(.5,.9,.95,.99), na.rm = TRUE))

# ---------------------------
# 11) GRÁFICOS (PNG) LISTOS PARA PPT
# ---------------------------

# Hist EPF con foco
p_hist <- ggplot(epf_hog, aes(x = gasto_internet)) +
  geom_histogram(bins = 40) +
  coord_cartesian(xlim = c(0, 60000)) +
  labs(title = "EPF GS: Gasto mensual Internet (hogar) - foco 0 a 60 mil",
       x = "Gasto internet", y = "Frecuencia") +
  theme_minimal()
ggsave("01_hist_epf_internet_foco.png", p_hist, width = 9, height = 5, dpi = 150)

# Densidad comparativa con leyenda (log1p)
df_den <- dplyr::bind_rows(
  epf_hog %>% dplyr::transmute(fuente = "EPF", log_g = log1p(gasto_internet)),
  casen2  %>% dplyr::transmute(fuente = "CASEN (imputado realista calibrado)", log_g = log1p(gasto_imputado_realista_cal))
)

p_den <- ggplot(df_den, aes(x = log_g, colour = fuente)) +
  geom_density(linewidth = 1) +
  labs(title = "EPF vs CASEN imputado (Internet) - log(1+gasto)",
       x = "log(1+gasto)", y = "Densidad", colour = "") +
  theme_minimal()
ggsave("02_densidad_epf_vs_casen_realista_cal_log.png", p_den, width = 9, height = 5, dpi = 150)

# ROC CORREGIDO (sin "X"): eje x = 1-Especificidad (0->1)
png("03_roc_logit_internet.png", width = 900, height = 650)
plot(roc_obj,
     legacy.axes = FALSE,              # <-- clave
     xlab = "1 - Especificidad",
     ylab = "Sensibilidad",
     main = paste0("ROC Logit Internet (AUC=", round(auc_val,3), ")"))
abline(a = 0, b = 1, lty = 2, col = "grey")
dev.off()

# ---------------------------
# 12) EXPORTAR RESULTADOS
# ---------------------------
resumen_modelo <- data.frame(
  producto = "Internet fijo hogar",
  ccif = ccif_internet,
  n_hog_epf = nrow(epf_hog),
  prevalencia_epf = prev_epf,
  auc_test = auc_val,
  mae_monto_test = mae,
  rmse_monto_test = rmse,
  rmse_2part_test = rmse_2part,
  tope_outliers_monto = tope,
  prevalencia_casen_imputada_cal = prev_casen_cal
)

write.csv(resumen_modelo, "resumen_modelo_internet_realista_cal.csv", row.names = FALSE)
saveRDS(casen2, "casen_con_imputacion_internet_realista_cal.rds")

cat("\nListo. Guardado:\n",
    "- 01_hist_epf_internet_foco.png\n",
    "- 02_densidad_epf_vs_casen_realista_cal_log.png\n",
    "- 03_roc_logit_internet.png\n",
    "- resumen_modelo_internet_realista_cal.csv\n",
    "- casen_con_imputacion_internet_realista_cal.rds\n")
