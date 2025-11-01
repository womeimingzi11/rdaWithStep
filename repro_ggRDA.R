#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(vegan)
  library(ggplot2)
  library(ggvegan)
})

# Source project functions
source("R/envfit_to_df.R")
source("R/ggRDA.R")

# Read data
com_raw <- readr::read_csv("resource/data/df_com_smp.csv", show_col_types = FALSE)
env_raw <- readr::read_csv("resource/data/df_env_smp.csv", show_col_types = FALSE)

# Keep only numeric columns; drop all-NA and zero-variance columns
not_all_na <- function(x) !all(is.na(x))
non_const <- function(x) sd(x, na.rm = TRUE) > 0

com_num <- com_raw %>% select(where(is.numeric)) %>% select(where(not_all_na)) %>% select(where(non_const))
env_num <- env_raw %>% select(where(is.numeric)) %>% select(where(not_all_na)) %>% select(where(non_const))

cat("com_num dims:", nrow(com_num), "x", ncol(com_num), "\n")
cat("env_num dims:", nrow(env_num), "x", ncol(env_num), "\n")

# Ensure rows match across matrices (inner join by row index if needed)
min_rows <- min(nrow(com_num), nrow(env_num))
if (nrow(com_num) != nrow(env_num)) {
  com_num <- com_num %>% slice(1:min_rows)
  env_num <- env_num %>% slice(1:min_rows)
  cat("Row counts differ; sliced both to", min_rows, "rows\n")
}

# Build RDA object
rda_obj <- vegan::rda(com_num, env_num, scale = TRUE)

# Diagnostics: fortify and biplot rows
fmod <- ggvegan::fortify(rda_obj)
if ("score" %in% names(fmod)) names(fmod)[names(fmod) == "score"] <- "Score"
if ("label" %in% names(fmod)) names(fmod)[names(fmod) == "label"] <- "Label"

cat("fortify rows:", nrow(fmod), ", cols:", paste(names(fmod), collapse = ","), "\n")
fmod_bp <- dplyr::filter(fmod, Score == "biplot")
fmod_sp <- dplyr::filter(fmod, Score == "species")
cat("biplot rows:", nrow(fmod_bp), ", bp cols:", paste(names(fmod_bp), collapse = ","), "\n")

# Adaptive arrow multiplier based on ranges (avoids NULL)
num_cols <- names(fmod_bp)[vapply(fmod_bp, is.numeric, logical(1))]
if (length(num_cols) >= 2) {
  b1 <- num_cols[1]; b2 <- num_cols[2]
  fnum_cols <- names(fmod_sp)[vapply(fmod_sp, is.numeric, logical(1))]
  s1 <- fnum_cols[1]; s2 <- fnum_cols[2]
  sp_rx <- max(abs(fmod_sp[[s1]]), na.rm = TRUE)
  sp_ry <- max(abs(fmod_sp[[s2]]), na.rm = TRUE)
  bp_rx <- max(abs(fmod_bp[[b1]]), na.rm = TRUE)
  bp_ry <- max(abs(fmod_bp[[b2]]), na.rm = TRUE)
  m_x <- if (bp_rx > 0) sp_rx / bp_rx else NA_real_
  m_y <- if (bp_ry > 0) sp_ry / bp_ry else NA_real_
  arrow_mul <- suppressWarnings(min(m_x, m_y, na.rm = TRUE))
  if (!is.finite(arrow_mul) || is.na(arrow_mul)) arrow_mul <- 1
} else {
  arrow_mul <- 1
}
cat("arrow_mul (adaptive):", arrow_mul, "\n")

# Envfit and df conversion
fit <- vegan::envfit(rda_obj, env_num, permutations = 199)
envfit_df <- envfit_to_df(fit, r2_dig = 3)
print(head(envfit_df))

# Run ggRDA and save figure
p <- ggRDA(rda_obj, envfit_df = envfit_df)
print(p)

out_path <- "resource/figure/repro_ggRDA.png"
ggplot2::ggsave(out_path, p, width = 8, height = 6, dpi = 150)
cat("Saved figure to:", out_path, "\n")