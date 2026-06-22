# Bird Survey Comparison: Human Point Counts (PC) vs BirdNET - restricted to the 67 species that birdnet knows
#20th November 2025        
#to see for which species there is most and least agreement between the methods

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)
library

# Load data
setwd("/path to your working directory/")
pc_data <- read.csv("pc_spp.site.csv", header=T)   
birdnet_data <- read.csv("birdnet spp.site.csv", header = T)
head(pc_data)
head(birdnet_data)

dim(pc_data)
dim(birdnet_data)

# Extract species names (first column)
pc_species <- pc_data[, 1]
birdnet_species <- birdnet_data[, 1]
pc_species
birdnet_species

# Find common species between the two datasets
common_species <- intersect(pc_species, birdnet_species)
cat("\nNumber of species in point counts:", length(pc_species), "\n")
cat("Number of species in BirdNET:", length(birdnet_species), "\n")
cat("Number of common species:", length(common_species), "\n")

# Get site names (column names, excluding first column which is species)
pc_sites <- colnames(pc_data)[-1]
birdnet_sites <- colnames(birdnet_data)[-1]

# Find common sites
common_sites <- intersect(pc_sites, birdnet_sites)
cat("Number of sites in point counts:", length(pc_sites), "\n")
cat("Number of sites in BirdNET:", length(birdnet_sites), "\n")
cat("Number of common sites:", length(common_sites), "\n\n")

# Filter both datasets to include only common species and common sites
pc_filtered <- pc_data %>%
  filter(WMshort %in% common_species) %>%
  select(WMshort, all_of(common_sites))

birdnet_filtered <- birdnet_data %>%
  filter(WMshort %in% common_species) %>%
  select(WMshort, all_of(common_sites))

# Make sure species are in the same order
pc_filtered <- pc_filtered %>% arrange(WMshort)
birdnet_filtered <- birdnet_filtered %>% arrange(WMshort)

# Metrics for each species
comparison_df <- data.frame(
  species = pc_filtered$WMshort,
  n_sites_pc = NA,
  n_sites_birdnet = NA,
  n_sites_both = NA,
  overlap_pct = NA
)

for (i in 1:nrow(comparison_df)) {
  species <- comparison_df$species[i]
  
  # Extract presence-absence vectors for this species (excluding species name column)
  pc_vector <- as.numeric(pc_filtered[pc_filtered$WMshort == species, -1])
  birdnet_vector <- as.numeric(birdnet_filtered[birdnet_filtered$WMshort == species, -1])
  
  # Number of sites where species was detected
  n_pc <- sum(pc_vector > 0, na.rm = TRUE)
  n_birdnet <- sum(birdnet_vector > 0, na.rm = TRUE)
  
  # Number of sites where both methods detected the species
  n_both <- sum(pc_vector > 0 & birdnet_vector > 0, na.rm = TRUE)
  
  # Calculate overlap percentage
  # Overlap = sites where both detected / total unique sites where either detected
  n_either <- sum(pc_vector > 0 | birdnet_vector > 0, na.rm = TRUE)
  overlap_pct <- if (n_either > 0) (n_both / n_either) * 100 else 0
  
  # Store results
  comparison_df$n_sites_pc[i] <- n_pc
  comparison_df$n_sites_birdnet[i] <- n_birdnet
  comparison_df$n_sites_both[i] <- n_both
  comparison_df$overlap_pct[i] <- overlap_pct
}

# Print summary statistics
cat("Summary of comparison:\n")
cat("Species detected by point counts only:", 
    sum(comparison_df$n_sites_pc > 0 & comparison_df$n_sites_birdnet == 0), "\n")
cat("Species detected by BirdNET only:", 
    sum(comparison_df$n_sites_birdnet > 0 & comparison_df$n_sites_pc == 0), "\n")
cat("Species detected by both methods:", 
    sum(comparison_df$n_sites_pc > 0 & comparison_df$n_sites_birdnet > 0), "\n")
cat("\nMean overlap percentage:", 
    round(mean(comparison_df$overlap_pct[comparison_df$n_sites_pc > 0 | comparison_df$n_sites_birdnet > 0]), 1), "%\n")

# Save comparison data
write.csv(comparison_df, "species_comparisonJun2026.csv", row.names = FALSE)


# Comparison plot
p <- ggplot(comparison_df, aes(x = n_sites_pc, y = n_sites_birdnet, color = overlap_pct)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_color_viridis_c(option = "viridis", name = "Overlap\n(%)", 
                      limits = c(0, 50), breaks = seq(0, 50, 10),
                      direction = -1) +
  labs(
    title = "Comparison of bird species detection methods",
    x = "No. of sites detected by point counts",
    y = "No. of sites detected by AI"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  ) +
  coord_fixed(ratio = 1)  # Equal scaling on both axes
  
  p

# Save the plot
ggsave("method_comparison_plot.png", 
       plot = p, width = 8, height = 7, dpi = 300, bg = "white")


# Display the plot
print(p)




#######################################
#data exploration 

# Show top 15 species with best agreement
cat("\nTop 15 species with highest overlap (detected by both methods):\n")
top_overlap <- comparison_df %>%
  filter(n_sites_pc > 0 & n_sites_birdnet > 0) %>%
  arrange(desc(overlap_pct)) %>%
  head(15) %>%
  select(species, n_sites_pc, n_sites_birdnet, n_sites_both, overlap_pct)
print(top_overlap)
write.csv(top_overlap,"top15overlap.csv")

#overall overlap
# list species by agreement between methods

cat("\nTop species with highest overlap (detected by both methods):\n")
all_overlap <- comparison_df %>%
  filter(n_sites_pc > 0 & n_sites_birdnet > 0) %>%
  arrange(desc(overlap_pct)) %>%
  
  select(species, n_sites_pc, n_sites_birdnet, n_sites_both, overlap_pct)
print(all_overlap)
write.csv(all_overlap,"sppoverlapJan2026.csv")

###########################################################

# Show species with large discrepancies
cat("\nSpecies with large detection differences (>50 sites difference):\n")
discrepancy <- comparison_df %>%
  mutate(diff = abs(n_sites_pc - n_sites_birdnet)) %>%
  filter(diff > 50) %>%
  arrange(desc(diff)) %>%
  select(species, n_sites_pc, n_sites_birdnet, overlap_pct, diff)
print(discrepancy)

write.csv(discrepancy, "spp.most.discrepencies50Feb2026.csv")   #gives 32 species for which there are largish discrepencies. 
##########
#what if we go for a greater difference in sites, say 100?
discrepancy <- comparison_df %>%
  mutate(diff = abs(n_sites_pc - n_sites_birdnet)) %>%
  filter(diff > 100) %>%
  arrange(desc(diff)) %>%
  select(species, n_sites_pc, n_sites_birdnet, overlap_pct, diff)
print(discrepancy)

write.csv(discrepancy, "spp.most.discrepencies100Feb2026.csv")   #this gives 23 species. 

###########
#what about 150 or more sites?
discrepancy <- comparison_df %>%
  mutate(diff = abs(n_sites_pc - n_sites_birdnet)) %>%
  filter(diff > 150) %>%
  arrange(desc(diff)) %>%
  select(species, n_sites_pc, n_sites_birdnet, overlap_pct, diff)
print(discrepancy)

write.csv(discrepancy, "spp.most.discrepencies150Feb2026.csv")   #this gives 18 species. 


