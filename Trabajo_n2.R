## ============================================================
## TAREA 2 - INTERNET (EPF -> CASEN RM) | Gran Santiago / RM
## Modelo two-part: (1) Logit incurrencia  (2) Monto condicional
## + imputación realista (calibración + bootstrap de residuos)
## + gráficos + tablas para PPT
## ============================================================

## -------------------------
## 0) LIBRERÍAS
## -------------------------
pkgs <- c("haven","dplyr","ggplot2","pROC","hexbin","scales","readr")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if(length(to_install) > 0) install.packages(to_install)

library(haven)
library(dplyr)
library(ggplot2)
library(pROC)
library(hexbin)
library(scales)
library(readr)

set.seed(123)
options(scipen = 999)  # evita notación científica en ejes

## -------------------------
## 1) RUTAS
## -------------------------
BASE_DIR <- "C:/Users/Kurta/OneDrive/Documentos/Geom_2025_2s/push_bdd"
OUT_DIR  <- file.path(BASE_DIR, "outputs_internet")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

## Buscar archivos aunque estén en subcarpetas
find_file <- function(base_dir, filename) {
  hits <- list.files(base_dir,
                     pattern = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
                     recursive = TRUE, full.names = TRUE)
  hits <- hits[!grepl("/\\._", hits)]  # descarta archivos fantasma ._
  if(length(hits) == 0) stop(paste("No encontré el archivo:", filename, "en", base_dir))
  hits[1]
}

EPF_PERSONAS <- find_file(BASE_DIR, "base-personas-ix-epf-stata.dta")
EPF_GASTOS   <- find_file(BASE_DIR, "base-gastos-ix-epf-stata.dta")
CASEN_RM_RDS <- find_file(BASE_DIR, "casen_rm.rds")

## -------------------------
## 2) FUNCIONES AUXILIARES
## -------------------------
to_num <- function(x) suppressWarnings(as.numeric(x))

make_grupo_escolaridad <- function(anios) {
  cut(
    to_num(anios),
    breaks = c(-Inf, 12, 14, 16, Inf),
    labels = c("Escolar", "Tecnico", "Universitaria", "Postgrado"),
    right = TRUE
  )
}

## -------------------------
## 3) CARGA EPF
## -------------------------
personas <- read_dta(EPF_PERSONAS)
gastos   <- read_dta(EPF_GASTOS)

valores_invalidos <- c(-99, -88, -77)

## EPF: Gran Santiago + variables limpias (nivel persona)
personas_gs <- personas %>%
  mutate(
    macrozona = to_num(macrozona),
    edad      = to_num(edad),
    edue      = to_num(edue),
    npersonas = to_num(npersonas),
    ing_disp_hog_hd_ai = to_num(ing_disp_hog_hd_ai),
    sexo      = to_num(sexo),
    n_linea   = to_num(n_linea)
  ) %>%
  filter(
    macrozona == 2,
    !(edad %in% valores_invalidos),
    !(edue %in% valores_invalidos),
    ing_disp_hog_hd_ai >= 0,
    npersonas > 0
  ) %>%
  mutate(
    ing_pc = ing_disp_hog_hd_ai / npersonas,
    log_ing_pc = log1p(ing_pc),
    sexo_f = factor(sexo, levels = c(1,2), labels = c("Hombre","Mujer")),
    grupo_escolaridad = make_grupo_escolaridad(edue)
  )

## -------------------------
## 4) EPF: GASTO INTERNET (hogar) por folio
## -------------------------
CCIF_INTERNET <- "08.3.3.01.01"

gasto_internet_folio <- gastos %>%
  mutate(macrozona = to_num(macrozona)) %>%
  filter(macrozona == 2, ccif == CCIF_INTERNET) %>%
  group_by(folio) %>%
  summarise(gasto_internet = sum(to_num(gasto), na.rm = TRUE), .groups = "drop")

## Merge a EPF (a todas las personas del hogar) + binaria
personas_gs <- personas_gs %>%
  left_join(gasto_internet_folio, by = "folio") %>%
  mutate(
    gasto_internet = ifelse(is.na(gasto_internet), 0, gasto_internet),
    incurre_internet = ifelse(gasto_internet > 0, 1, 0)
  )

## EPF a nivel HOGAR (una fila por folio): usamos persona “referencia” (n_linea mínima)
epf_hogar <- personas_gs %>%
  group_by(folio) %>%
  slice_min(order_by = n_linea, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(folio, sexo_f, edad, grupo_escolaridad, ing_pc, log_ing_pc, gasto_internet, incurre_internet) %>%
  filter(!is.na(sexo_f), !is.na(grupo_escolaridad), !is.na(log_ing_pc))

## Base monto (solo gastadores) para exploración/gráficos
tabla_gasto <- epf_hogar %>%
  filter(gasto_internet > 0)

## Trim 1%-99% (robustez)
q_ing   <- quantile(tabla_gasto$ing_pc, probs = c(0.01, 0.99), na.rm = TRUE)
q_gasto <- quantile(tabla_gasto$gasto_internet, probs = c(0.01, 0.99), na.rm = TRUE)

tabla_gasto <- tabla_gasto %>%
  filter(
    ing_pc >= q_ing[1], ing_pc <= q_ing[2],
    gasto_internet >= q_gasto[1], gasto_internet <= q_gasto[2]
  )

## ============================================================
## 5) GRÁFICOS EPF
## ============================================================

png(file.path(OUT_DIR, "plot_01_hist_ing_pc_epf.png"), width = 980, height = 620)
hist(tabla_gasto$ing_pc, breaks = 30, col = "lightblue",
     main = "Distribución del Ingreso per cápita (EPF - hogares gastadores)",
     xlab = "Ingreso per cápita", ylab = "Frecuencia")
dev.off()

png(file.path(OUT_DIR, "plot_02_hist_gasto_internet_epf_foco.png"), width = 980, height = 620)
hist(tabla_gasto$gasto_internet, breaks = 30, col = "gray50",
     main = "EPF GS: Gasto mensual Internet (hogar) - foco 0 a 60 mil",
     xlab = "Gasto internet", ylab = "Frecuencia",
     xlim = c(0, 60000))
dev.off()

png(file.path(OUT_DIR, "plot_03_box_sexo.png"), width = 980, height = 620)
boxplot(gasto_internet ~ sexo_f, data = tabla_gasto,
        main = "Gasto en Internet según Sexo (EPF)",
        xlab = "Sexo", ylab = "Gasto internet")
dev.off()

png(file.path(OUT_DIR, "plot_04_box_escolaridad.png"), width = 980, height = 620)
boxplot(gasto_internet ~ grupo_escolaridad, data = tabla_gasto,
        main = "Gasto en Internet según Escolaridad (EPF)",
        xlab = "Escolaridad", ylab = "Gasto internet")
dev.off()

p_hex_edad <- ggplot(tabla_gasto, aes(x = edad, y = gasto_internet)) +
  geom_hex(bins = 35) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Frecuencia") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Edad vs Gasto en Internet (EPF)", x = "Edad", y = "Gasto internet") +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_DIR, "plot_05_hex_edad.png"),
       p_hex_edad, width = 9, height = 5, dpi = 130)

p_hex_ing <- ggplot(tabla_gasto, aes(x = ing_pc, y = gasto_internet)) +
  geom_hex(bins = 35) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Frecuencia") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Ingreso vs Gasto en Internet (EPF)", x = "Ingreso per cápita", y = "Gasto internet") +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_DIR, "plot_06_hex_ingreso.png"),
       p_hex_ing, width = 9, height = 5, dpi = 130)

## ============================================================
## 6) MODELO TWO-PART EN EPF (train/test)
## ============================================================
set.seed(123)
idx <- sample.int(nrow(epf_hogar), size = floor(0.7 * nrow(epf_hogar)))
train <- epf_hogar[idx, ]
test  <- epf_hogar[-idx, ]

## Parte 1: Logit (incurre)
m_logit <- glm(incurre_internet ~ sexo_f + edad + grupo_escolaridad + log_ing_pc,
               data = train, family = binomial)

test$p_hat <- predict(m_logit, newdata = test, type = "response")
roc_obj <- roc(test$incurre_internet, test$p_hat, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))

png(file.path(OUT_DIR, "plot_07_roc_logit_internet.png"), width = 980, height = 720)
plot(roc_obj, col = "blue", lwd = 2, legacy.axes = FALSE,
     main = paste0("Curva ROC - Logit Internet (AUC=", round(auc_val,3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

## Parte 2: Monto (solo gastadores)
train_g <- train %>% filter(gasto_internet > 0)
test_g  <- test  %>% filter(gasto_internet > 0)

tope <- min(quantile(train_g$gasto_internet, 0.99, na.rm = TRUE), 60000)
train_g$gasto_wins <- pmin(train_g$gasto_internet, tope)
test_g$gasto_wins  <- pmin(test_g$gasto_internet,  tope)

m_monto <- lm(log1p(gasto_wins) ~ sexo_f + edad + grupo_escolaridad + log_ing_pc,
              data = train_g)

test_g$log_m_hat <- predict(m_monto, newdata = test_g)
test_g$m_hat <- pmax(exp(test_g$log_m_hat) - 1, 0)

mae_monto  <- mean(abs(test_g$gasto_wins - test_g$m_hat), na.rm = TRUE)
rmse_monto <- sqrt(mean((test_g$gasto_wins - test_g$m_hat)^2, na.rm = TRUE))

## RMSE two-part (esperado) en test completo
test$gasto_wins <- pmin(test$gasto_internet, tope)
test$log_m_hat_all <- predict(m_monto, newdata = test)
test$m_hat_all <- pmax(exp(test$log_m_hat_all) - 1, 0)
test$gasto_esperado <- test$p_hat * test$m_hat_all

rmse_2part <- sqrt(mean((test$gasto_wins - test$gasto_esperado)^2, na.rm = TRUE))

## ============================================================
## 7) CARGA + PREP CASEN RM (hogar)
## ============================================================
casen <- readRDS(CASEN_RM_RDS)

casen_hogar <- casen %>%
  mutate(
    pco1      = to_num(pco1),
    edad      = to_num(edad),
    sexo      = to_num(sexo),
    esc       = to_num(esc),
    ytotcorh  = to_num(ytotcorh),
    tot_per_h = to_num(tot_per_h)
  ) %>%
  filter(pco1 == 1, tot_per_h > 0, ytotcorh >= 0) %>%   # jefe/a de hogar + ingreso válido
  mutate(
    ing_pc = ytotcorh / tot_per_h,
    log_ing_pc = log1p(ing_pc),
    sexo_f = factor(sexo, levels = c(1,2), labels = c("Hombre","Mujer")),
    grupo_escolaridad = make_grupo_escolaridad(esc)
  ) %>%
  select(folio, hogar, sexo_f, edad, grupo_escolaridad, ing_pc, log_ing_pc)

## Igualar niveles de factores con EPF
casen_hogar$sexo_f <- factor(casen_hogar$sexo_f, levels = levels(epf_hogar$sexo_f))
casen_hogar$grupo_escolaridad <- factor(casen_hogar$grupo_escolaridad, levels = levels(epf_hogar$grupo_escolaridad))

## ============================================================
## 8) IMPUTACIÓN EN CASEN (calibración + realismo)
## ============================================================
## Probabilidad (p_hat)
casen_hogar$p_hat <- predict(m_logit, newdata = casen_hogar, type = "response")

## Calibración a prevalencia EPF (train)
target_prev <- mean(train$incurre_internet, na.rm = TRUE)

calibrate_delta <- function(p, target) {
  f <- function(d) mean(plogis(qlogis(p) + d), na.rm = TRUE) - target
  uniroot(f, interval = c(-10, 10))$root
}

delta <- calibrate_delta(casen_hogar$p_hat, target_prev)
casen_hogar$p_cal <- plogis(qlogis(casen_hogar$p_hat) + delta)

set.seed(123)
casen_hogar$incurre_pred <- rbinom(nrow(casen_hogar), 1, casen_hogar$p_cal)

## Monto predicho + bootstrap de residuos
casen_hogar$log_m_hat <- predict(m_monto, newdata = casen_hogar)

resid_pool <- resid(m_monto)
set.seed(123)
eps_boot <- sample(resid_pool, size = nrow(casen_hogar), replace = TRUE)

casen_hogar$log_m_sim <- casen_hogar$log_m_hat + eps_boot
casen_hogar$m_sim <- pmax(exp(casen_hogar$log_m_sim) - 1, 0)

casen_hogar$gasto_imputado <- ifelse(casen_hogar$incurre_pred == 1, casen_hogar$m_sim, 0)

## Winsor para gráficos en CASEN (1%-99% sobre positivos)
pos <- casen_hogar$gasto_imputado[casen_hogar$gasto_imputado > 0]
if(length(pos) > 50) {
  q <- quantile(pos, probs = c(0.01, 0.99), na.rm = TRUE)
  casen_hogar$gasto_imputado_wins <- ifelse(casen_hogar$gasto_imputado > 0,
                                            pmin(pmax(casen_hogar$gasto_imputado, q[1]), q[2]),
                                            0)
} else {
  casen_hogar$gasto_imputado_wins <- casen_hogar$gasto_imputado
}

## ============================================================
## 9) GRÁFICOS CASEN + COMPARACIÓN EPF vs CASEN
## ============================================================

png(file.path(OUT_DIR, "plot_08_hist_casen_imputado.png"), width = 980, height = 620)
hist(casen_hogar$gasto_imputado_wins[casen_hogar$gasto_imputado_wins > 0],
     breaks = 30, col = "gray80",
     main = "CASEN RM: gasto imputado en Internet (hogar)",
     xlab = "Gasto imputado (winsorizado)", ylab = "Frecuencia")
dev.off()

## Densidad en log(1+gasto) con ejes legibles (sin 2e-06)
epf_log <- log1p(tabla_gasto$gasto_internet)
cas_log <- log1p(casen_hogar$gasto_imputado_wins[casen_hogar$gasto_imputado_wins > 0])

p_den <- ggplot() +
  geom_density(aes(x = epf_log, color = "EPF"), linewidth = 1.2) +
  geom_density(aes(x = cas_log, color = "CASEN imputado"), linewidth = 1.2) +
  scale_y_continuous(labels = label_number()) +
  scale_x_continuous(labels = label_number()) +
  labs(
    title = "EPF vs CASEN imputado (Internet) - log(1+gasto)",
    x = "log(1+gasto)",
    y = "Densidad",
    color = ""
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(OUT_DIR, "plot_09_densidad_epf_vs_casen_log.png"),
       p_den, width = 9, height = 5, dpi = 130)

## ============================================================
## 10) TABLAS DE RESULTADOS (CSV) + RDS final
## ============================================================

## Percentiles (positivos, escala original)
epf_pos <- tabla_gasto$gasto_internet
cas_pos <- casen_hogar$gasto_imputado_wins[casen_hogar$gasto_imputado_wins > 0]

pct <- c(0.50, 0.90, 0.95, 0.99)
epf_pct <- quantile(epf_pos, probs = pct, na.rm = TRUE)
cas_pct <- quantile(cas_pos, probs = pct, na.rm = TRUE)

resumen_metricas <- tibble(
  metrica = c("AUC_logit_test", "MAE_monto_test_gastadores", "RMSE_monto_test_gastadores", "RMSE_two_part_test"),
  valor   = c(auc_val, mae_monto, rmse_monto, rmse_2part)
)

resumen_percentiles <- tibble(
  percentil = c("p50","p90","p95","p99"),
  EPF = as.numeric(epf_pct),
  CASEN_imputado = as.numeric(cas_pct)
)

write_csv(resumen_metricas, file.path(OUT_DIR, "resumen_metricas_internet.csv"))
write_csv(resumen_percentiles, file.path(OUT_DIR, "percentiles_epf_vs_casen_internet.csv"))

saveRDS(casen_hogar, file.path(OUT_DIR, "casen_rm_con_imputacion_internet.rds"))

cat("\nOK ✅ Todo guardado en:\n", OUT_DIR, "\n")
cat("AUC:", round(auc_val, 3),
    "| MAE:", round(mae_monto, 2),
    "| RMSE_monto:", round(rmse_monto, 2),
    "| RMSE_2part:", round(rmse_2part, 2), "\n")
