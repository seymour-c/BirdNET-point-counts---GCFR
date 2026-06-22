#just trying it, INCLUDING the uncertains as "0.5" - not sure this is defensible, but let's see')
# Merge Table 2 (xeno-canto counts) with Table 3 (validation accuracy)
# and run regression of xeno-canto count vs BirdNET accuracy

# Merge Table 2 (xeno-canto counts) with Table 3 (validation accuracy)
# and run regression of xeno-canto count vs BirdNET accuracy

setwd("path\\to your working\\directory")

library(tidyverse)

# -- Table 3: validation data (species validated by manual listening) ----------
# TRUE % extracted from your table; "Not Certain" excluded from accuracy calc
# accuracy = TRUE / (TRUE + FALSE)

table3 <- read.csv("spp correct list.csv", header=T)

table3 <- table3 %>%
  mutate(
    # accuracy excluding uncertain
    accuracy_excl = true_n / (true_n + false_n),
    # accuracy treating uncertain as 0.5
    accuracy_half = (true_n + 0.5 * not_certain) / (true_n + false_n + not_certain),
    # use excluding-uncertain as default
    accuracy = accuracy_excl
  )

# -- Table 2 subset: xeno-canto counts (species with validation data only) -----
#
xeno <- read.csv("spp list with xc in.csv", header=T, fileEncoding="latin1")
head(xeno)

table3 <- table3 %>%
  mutate(accuracy_excl = true_n / (true_n + false_n),
         accuracy_uncertain = (true_n + 0.5 * not_certain) / (true_n + false_n + not_certain))
xeno<-xeno %>%
  mutate(ratio_SA = xc_SA / xc_global)

# -- Merge ---------------------------------------------------------------------
combined <- table3 %>%
  left_join(xeno, by = "common_name")

# Check for any unmatched species
combined %>% filter(is.na(xc_global)) %>% select(common_name)

head(combined)

# -- Beta regressions: uncertain excluded --------------------------------------
library(betareg)

n <- nrow(combined)
combined <- combined %>%
  mutate(
    accuracy_adj      = (accuracy_excl * (n - 1) + 0.5) / n,
    accuracy_half_adj = (accuracy_half * (n - 1) + 0.5) / n
  )

# Beta regression: uncertain EXCLUDED
mod_global_beta <- betareg(accuracy_adj ~ log10(xc_global), data = combined)
summary(mod_global_beta)
#check the model
plot(mod_global_beta, which = c(1,2,4,5))
#little grebe has a high cook's distance'

mod_SA_beta <- betareg(accuracy_adj ~ log10(xc_SA), data = combined)
summary(mod_SA_beta)
#checking model
plot(mod_SA_beta, which = c(1,2,4,5))    #line 36 is quite an outlier (white starred robin)
#what if we take it out
dim(combined)
combined.nowsr <- combined[c(1:35,37:43),]
mod_SA_beta.nowsr <- betareg(accuracy_adj ~ log10(xc_SA), data = combined.nowsr)
summary(mod_SA_beta.nowsr)
plot(mod_SA_beta.nowsr, which = c(1,2,4,5))    
mod_ratio_beta <- betareg(accuracy_adj ~ ratio_SA, data = combined)
summary(mod_ratio_beta)
plot(mod_ratio_beta, which = c(1,2,4,5))    

mod_beta <- betareg(accuracy_adj ~ratio_SA, data=combined.nowsr)
summary(mod_beta)   

# Beta regression: uncertain = 0.5
mod_global_beta_half <- betareg(accuracy_half_adj ~ log10(xc_global), data = combined)
summary(mod_global_beta_half)

mod_SA_beta_half <- betareg(accuracy_half_adj ~ log10(xc_SA), data = combined)
summary(mod_SA_beta_half)

mod_ratio_beta_half.nowsr <- betareg(accuracy_half_adj ~ ratio_SA, data = combined.nowsr)
summary(mod_ratio_beta_half.nowsr)
plot(mod_ratio_beta_half.nowsr, which = c(1,2,4,5))    #line 36 is quite an outlier (white starred robin)


