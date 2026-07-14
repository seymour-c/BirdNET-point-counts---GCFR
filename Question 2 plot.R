# Bird Survey Comparison: Human Point Counts (PC) vs BirdNET
# Restricted to 633 confirmed paired PC + ARU surveys 

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)

# ---- Load data (restricted n = 633) ----
#setwd("/path to your working directory/")
pc_data      <- read.csv("pc.spp.site.n633.csv",      header = TRUE, check.names = FALSE)
birdnet_data <- read.csv("birdnet.spp.site.n633.csv", header = TRUE, check.names = FALSE)

dim(pc_data)       # 67 species x 634 cols (1 species name + 633 surveys)
dim(birdnet_data)

# ---- Extract species and site names ----
pc_species      <- pc_data[, 1]
birdnet_species <- birdnet_data[, 1]

common_species <- intersect(pc_species, birdnet_species)

pc_sites      <- colnames(pc_data)[-1]
birdnet_sites <- colnames(birdnet_data)[-1]
common_sites  <- intersect(pc_sites, birdnet_sites)


# ---- Filter both datasets to common species and common sites ----
pc_filtered <- pc_data %>%
  filter(WMshort %in% common_species) %>%
  select(WMshort, all_of(common_sites)) %>%
  arrange(WMshort)

birdnet_filtered <- birdnet_data %>%
  filter(WMshort %in% common_species) %>%
  select(WMshort, all_of(common_sites)) %>%
  arrange(WMshort)

# ---- Metrics for each species ----
comparison_df <- data.frame(
  species        = pc_filtered$WMshort,
  n_sites_pc     = NA,
  n_sites_birdnet = NA,
  n_sites_both   = NA,
  overlap_pct    = NA
)

for (i in 1:nrow(comparison_df)) {
  sp <- comparison_df$species[i]
  pc_vector      <- as.numeric(pc_filtered[pc_filtered$WMshort == sp, -1])
  birdnet_vector <- as.numeric(birdnet_filtered[birdnet_filtered$WMshort == sp, -1])

  n_pc      <- sum(pc_vector > 0, na.rm = TRUE)
  n_birdnet <- sum(birdnet_vector > 0, na.rm = TRUE)
  n_both    <- sum(pc_vector > 0 & birdnet_vector > 0, na.rm = TRUE)
  n_either  <- sum(pc_vector > 0 | birdnet_vector > 0, na.rm = TRUE)
  overlap_pct <- if (n_either > 0) (n_both / n_either) * 100 else 0

  comparison_df$n_sites_pc[i]      <- n_pc
  comparison_df$n_sites_birdnet[i] <- n_birdnet
  comparison_df$n_sites_both[i]    <- n_both
  comparison_df$overlap_pct[i]     <- overlap_pct
}

# ---- Summary ----
cat("Summary of comparison:\n")
cat("Species detected by point counts only:",
    sum(comparison_df$n_sites_pc > 0 & comparison_df$n_sites_birdnet == 0), "\n")
cat("Species detected by BirdNET only:",
    sum(comparison_df$n_sites_birdnet > 0 & comparison_df$n_sites_pc == 0), "\n")
cat("Species detected by both methods:",
    sum(comparison_df$n_sites_pc > 0 & comparison_df$n_sites_birdnet > 0), "\n")
cat("\nMean overlap percentage:",
    round(mean(comparison_df$overlap_pct[comparison_df$n_sites_pc > 0 |
                                          comparison_df$n_sites_birdnet > 0]), 1), "%\n")

write.csv(comparison_df, "species_comparison_n633.csv", row.names = FALSE)

# ---- Figure 3: species overlap plot ----
# Set colour scale at circa 70%
max_overlap <- max(comparison_df$overlap_pct, na.rm = TRUE)
upper <- ceiling(max_overlap / 10) * 10  # round up to nearest 10

# Label rule (matches caption): species with PC > BN, OR with BN >= 150
library(ggrepel)
comparison_df$label <- ifelse(
  comparison_df$n_sites_pc > comparison_df$n_sites_birdnet |
    comparison_df$n_sites_birdnet >= 150,
  as.character(comparison_df$species),
  ""
)

p <- ggplot(comparison_df, aes(x = n_sites_pc, y = n_sites_birdnet, color = overlap_pct)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.75) +
  geom_text_repel(aes(label = label), size = 3, color = "grey20",
                  max.overlaps = Inf,
                  box.padding = 0.35, point.padding = 0.25,
                  segment.size = 0.3, segment.color = "grey60",
                  min.segment.length = 0.15,
                  seed = 1) +
  scale_color_viridis_c(option = "viridis", name = "Overlap\n(%)",
                        limits = c(0, upper), breaks = seq(0, upper, 10),
                        direction = -1) +
  labs(
    x = "No. of surveys detected by point counts",
    y = "No. of surveys detected by BirdNET"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  ) +
  coord_fixed(ratio = 1)

p

ggsave("Figure3_n633.tif",
       plot = p, width = 8, height = 7, dpi = 500, bg = "white")

