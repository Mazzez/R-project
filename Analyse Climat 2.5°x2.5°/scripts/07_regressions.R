# ============================================================
# 07_regressions.R
# Régressions multivariées CO2 ~ variables climatiques.
# On utilise les RÉSIDUS (anomalies désaisonnées et détendrées)
# pour étudier les liens interannuels propres, puis lasso pour
# gérer la multicolinéarité forte entre flux radiatifs.
#
# Entrée : Analyse Climat 2.5°x2.5°/outputs/series_transformed.rds
# Sortie : Analyse Climat 2.5°x2.5°/outputs/regression_summary.txt
#          Analyse Climat 2.5°x2.5°/outputs/lasso_path.csv
#          Analyse Climat 2.5°x2.5°/outputs/plots/07*.png
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(glmnet)
})

OUT_DIR <- "/home/mazzez/Bureau/R project/Final Version/Analyse Climat 2.5°x2.5°/outputs"
PLOT    <- file.path(OUT_DIR, "plots")
ds      <- readRDS(file.path(OUT_DIR, "series_transformed.rds"))
theme_set(theme_minimal(base_size = 12))

clim_vars <- c("T2m","T500","SPFH2m","PWAT","APCP","TCDC",
               "DLWRF","ULWRF","DSWRF","USWRF","PRMSL",
               "CSDSF","CSUSF","CSDLF","CSULF","CDUVB","DUVB","ALBDO",
               "CRE_SW","CRE_LW","CRE_net")

X <- ds$resid[, clim_vars]
y <- ds$resid$co2_trend

cat("Régression sur résidus :", nrow(X), "obs,", ncol(X), "prédicteurs\n\n")

# ============================================================
# 1. Multicolinéarité — corrélations entre prédicteurs
# ============================================================
cat("=== Top 10 corrélations entre prédicteurs (multicolinéarité) ===\n")
M <- cor(X)
M[upper.tri(M, diag = TRUE)] <- NA
pairs <- as.data.frame(as.table(M)) |>
  filter(!is.na(Freq)) |>
  arrange(desc(abs(Freq)))
print(head(pairs, 10), digits = 3)

# ============================================================
# 2. Régression linéaire complète (toutes vars)
# ============================================================
fit_full <- lm(y ~ ., data = X)
sumcap <- capture.output(summary(fit_full))
cat("\n=== Régression complète (y = co2_trend résid) ===\n")
cat(sumcap, sep = "\n")

# ============================================================
# 3. Sélection AIC stepwise (backward)
# ============================================================
fit_step <- step(fit_full, direction = "backward", trace = 0)
sumstep <- capture.output(summary(fit_step))
cat("\n=== Modèle stepwise AIC (backward) ===\n")
cat(sumstep, sep = "\n")
cat("\nVariables retenues :",
    paste(setdiff(names(coef(fit_step)), "(Intercept)"), collapse = ", "), "\n")

# ============================================================
# 4. Lasso (glmnet) avec validation croisée
# ============================================================
set.seed(42)
Xm <- as.matrix(X)
cv <- cv.glmnet(Xm, y, alpha = 1, nfolds = 10, standardize = TRUE)
fit_lasso <- glmnet(Xm, y, alpha = 1, standardize = TRUE,
                    lambda = cv$lambda.min)

coefs <- as.numeric(coef(fit_lasso))
names(coefs) <- c("(Intercept)", colnames(Xm))
nonzero <- coefs[coefs != 0]
cat("\n=== Lasso (lambda.min =", round(cv$lambda.min, 4), ") ===\n")
cat("Variables retenues  :", length(nonzero) - 1, "/ ", ncol(Xm), "\n")
cat("\nCoefficients non nuls (standardisés) :\n")
print(round(sort(nonzero[-1], decreasing = TRUE), 4))

# Path lasso plot — on parcourt la grille complète de lambdas
glm_full <- glmnet(Xm, y, alpha = 1, standardize = TRUE)
beta_path <- as.matrix(glm_full$beta)              # rows = vars, cols = "s0".."sN"
lambdas   <- glm_full$lambda                       # 1 lambda par colonne

# IMPORTANT : glmnet retourne les coefficients dans l'échelle ORIGINALE
# des variables (même avec standardize=TRUE). Pour comparer entre variables
# de natures très différentes (kg/kg vs Pa vs K), on les rescale en
# "coefficients standardisés" = beta * sd(X[,j]) qui se lisent comme
# "variation de y attribuée à 1 sd de variation de X[j]".
sd_X <- apply(Xm, 2, sd)
beta_std_path <- sweep(beta_path, 1, sd_X, FUN = "*")

path_df <- as.data.frame(beta_std_path) |>
  mutate(var = rownames(beta_std_path)) |>
  pivot_longer(-var, names_to = "step_col", values_to = "coef") |>
  mutate(step_idx = as.integer(sub("^s", "", step_col)) + 1,  # s0 -> 1
         lambda   = lambdas[step_idx],
         log_lambda = log(lambda))

# Variables non triviales (au moins une fois |coef| > seuil) -> dans la légende
keep <- path_df |>
  group_by(var) |>
  summarise(max_abs = max(abs(coef)), .groups = "drop") |>
  filter(max_abs > 0.05) |>
  pull(var)

p_path <- ggplot(path_df |> filter(var %in% keep),
                 aes(log_lambda, coef, color = var)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = log(cv$lambda.min),
             linetype = "dashed", color = "darkred", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  scale_x_reverse() +   # plus intuitif : peu de pénalisation -> droite
  labs(title    = "Chemin de régularisation Lasso (coefficients standardisés)",
       subtitle = sprintf("Ligne rouge : log(lambda.min) = %.3f (lambda.min = %.4f)\nCoef = beta * sd(X) — variation de y par 1 sd de variation de X[j]",
                          log(cv$lambda.min), cv$lambda.min),
       x = "log(lambda)  (axe inversé : gauche = sélection forte ; droite = peu de pénalisation)",
       y = "Coefficient standardisé (ppm / sd)") +
  theme(legend.position   = "right",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(0.5, "cm"))
ggsave(file.path(PLOT, "07a_lasso_path.png"), p_path,
       width = 12, height = 7, dpi = 130)

write.csv(path_df, file.path(OUT_DIR, "lasso_path.csv"), row.names = FALSE)

# ============================================================
# 5. Diagnostic du modèle stepwise
# ============================================================
diag_df <- data.frame(
  date    = ds$resid$date,
  fitted  = predict(fit_step),
  resid   = residuals(fit_step),
  observed = y
)

p_fit <- ggplot(diag_df, aes(date)) +
  geom_line(aes(y = observed, color = "Observé"),  linewidth = 0.6) +
  geom_line(aes(y = fitted,   color = "Prédit"),   linewidth = 0.6) +
  scale_color_manual(values = c("Observé" = "steelblue", "Prédit" = "tomato")) +
  labs(title    = sprintf("Modèle stepwise — R²  = %.3f",
                          summary(fit_step)$r.squared),
       subtitle = "y = co2_trend résiduel (anomalie désaisonnée et détendrée)",
       x = NULL, y = "ppm", color = NULL)
ggsave(file.path(PLOT, "07b_stepwise_fit.png"), p_fit,
       width = 11, height = 6, dpi = 130)

p_res <- ggplot(diag_df, aes(date, resid)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title    = "Résidus du modèle stepwise",
       subtitle = "Doit ressembler à un bruit blanc",
       x = NULL, y = "ppm")
ggsave(file.path(PLOT, "07c_stepwise_residuals.png"), p_res,
       width = 11, height = 5, dpi = 130)

# Sauvegarde résumé texte
sink(file.path(OUT_DIR, "regression_summary.txt"))
cat("=== Régression complète ===\n\n"); cat(sumcap, sep = "\n")
cat("\n\n=== Modèle stepwise AIC (backward) ===\n\n"); cat(sumstep, sep = "\n")
cat("\n\n=== Lasso (lambda.min) — coefs non nuls ===\n")
print(round(sort(nonzero[-1], decreasing = TRUE), 4))
sink()

cat("\n=== Sauvegardes ===\n")
cat(" - regression_summary.txt\n")
cat(" - lasso_path.csv\n")
cat(" - plots/07a_lasso_path.png, 07b_stepwise_fit.png, 07c_stepwise_residuals.png\n")
