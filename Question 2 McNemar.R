#To compare the species assemblages between the two methods
#24 Nov 2025

library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)
library(RColorBrewer)
library(mvabund)

# Load data
setwd("path\\to your working\\directory")
spp.site <- read.csv("pc.birdnet.tog.csv", header=T, row.names=1)      #both pc and birdnet in one file
head(spp.site)
tail(spp.site)
summary(spp.site)
dim(spp.site)
spp.site<-t(spp.site)
dim(spp.site)
method.site <- read.csv("site.method.csv", header=T)
dim(method.site)
method.site$site.id <- as.factor(method.site$site.id)
method.site$method <- as.factor(method.site$method)



# McNemar's test for each species - handles pairing correctly
library(fmsb)

# Remove rare species first
min.detections <- 30  # removing spp with less than this number
common.spp <- colSums(spp.site) >= min.detections
filtered.data <- spp.site[, common.spp]
dim(filtered.data)  #takes out 10 spp


# McNemar's test for each species with error handling
species.results <- lapply(1:ncol(filtered.data), function(i) {
  pc <- filtered.data[1:994, i]
  ai <- filtered.data[995:1988, i]
  
  cont_table <- table(pc, ai)
  
  # Check if we have a proper 2x2 table
  if(nrow(cont_table) < 2 || ncol(cont_table) < 2) {
    return(data.frame(species = colnames(filtered.data)[i],
                     p_value = NA,
                     note = "Not enough variation for test"))
  }
  
  # Try McNemar's test
  test_result <- tryCatch({
    mcnemar.test(cont_table)
  }, error = function(e) {
    return(NULL)
  })
  
  if(is.null(test_result)) {
    return(data.frame(species = colnames(filtered.data)[i],
                     p_value = NA,
                     note = "Test failed"))
  }
  
  return(data.frame(species = colnames(filtered.data)[i],
                   p_value = test_result$p.value,
                   note = "OK"))
})


# Combine results
results <- do.call(rbind, species.results)
results

# Apply FDR correction only to valid p-values
valid_tests <- !is.na(results$p_value)
results$p_adjusted <- NA
results$p_adjusted[valid_tests] <- p.adjust(results$p_value[valid_tests], method = "fdr")
results$significant <- results$p_adjusted < 0.05

# View results
print(results)
#results[results$significant & !is.na(results$significant), ]

