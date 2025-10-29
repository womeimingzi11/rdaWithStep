#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(vegan)
  library(ggplot2)
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
fmod <- fortify(rda_obj)
if ("score" %in% names(fmod)) names(fmod)[names(fmod) == "score"] <- "Score"
if ("label" %in% names(fmod)) names(fmod)[names(fmod) == "label"] <- "Label"

cat("fortify rows:", nrow(fmod), ", cols:", paste(names(fmod), collapse = ","), "\n")
fmod_bp <- dplyr::filter(fmod, Score == "biplot")
cat("biplot rows:", nrow(fmod_bp), ", bp cols:", paste(names(fmod_bp), collapse = ","), "\n")

# Arrow multiplier (from base plot attributes)
bplot <- plot(rda_obj)
arrow_mul <- attributes(bplot$biplot)$arrow.mul
cat("arrow.mul:", narrow_mul, "\n")

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