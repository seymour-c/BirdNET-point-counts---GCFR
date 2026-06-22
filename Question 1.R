#9th Nov 2025
#now we are trying with birdnet. Only the data are not in the right format. 

setwd("C:\\Users\\seymourc\\OneDrive - sanbi.org.za\\Documents\\Files.for.R\\Ostrich AI vs PC\\birdnet vs. PC and CNN")
#compdat<- read.csv("pc & birdnet1.csv", header = T)
compdat<- read.csv("pc.birdnet.csv", header = T)


#We cannot just use a normal regression, because both PC and birdnet have some error (whereas a normal regression usually assumes that one of your variables DOES NOT have error in it).  Also, we have some observations from the SAME points – so these are not independent.  We can either use site as a random variable and do mixed models. We can split the data into wet and dry and ask if the methods are generally comparable, and do a test for both wet and dry season.  
#to deal with the fact that both the independent and dependent variables (i.e., PC and birdnet) have errors, we have to do a SMA = standardised major axis analysis or MA = major axis analysis. since we're testing for if  slope = 1 and intercept = 0, we should use SMA. 
#so the package we need for that is library(smatr)
head(compdat)
summary(compdat)
dim(compdat)
                                                                                       
library(smatr)
wet <- subset(compdat, Campaign == "Wet season") 
dry <- subset (compdat, Campaign =="Dry season")
names(compdat)

# We first want to test how birdnetEst does vs. PC_richness_AllSpecies (because that's what a manager might be interested in')

##########################################################################
#sma
library(smatr)
wet <- subset(compdat, Campaign == "Wet season") 
dry <- subset (compdat, Campaign =="Dry season")

wet$brdnet.no <- wet$brdnetEst
dry$brdnet.no <- dry$brdnetEst
wet$PC_rich_all <- wet$PC_richness_AllSpecies
dry$PC_rich_all <- dry$PC_richness_AllSpecies
wet$PC_rich_BN <- wet$PC_richness_BNSpecies
dry$PC_rich_BN <- dry$PC_richness_BNSpecies

wetfit <- sma(brdnet.no ~ PC_rich_all, data = wet, method = "SMA")
summary(wetfit)
print(wetfit)

#Fit using Standardized Major Axis 

#------------------------------------------------------------
#Coefficients:
#             elevation     slope
#estimate      -1.02968053 1.185522
#lower limit -2.07007403 1.064481
#upper limit  0.01071297 1.320327

#H0 : variables uncorrelated
#R-squared : 0.08287296 
#P-value : 2.8594e-07 


#so although correlated, seems that slope is not equal to 1; Intercept –  0 falls within the CI, so not significantly different to 0. BUT the slope (which should be 1) is greater than 1

#for just the species that birdnet can detect, filtered out in the PC data
wetfit <- sma(brdnet.no ~ PC_rich_BN, data = wet, method = "SMA")       
summary(wetfit)
                                                               
----------------------------------------------------------
#Coefficients:
#             elevation    slope
#estimate    -0.1168734 1.768392
#lower limit -1.0914610 1.586020
#upper limit  0.8577142 1.971736

#H0 : variables uncorrelated
#R-squared : 0.06315254 
#P-value : 8.3143e-06 



#for the dry dataset

dryfit <- sma(brdnet.no ~ PC_rich_all, data = dry, method = "SMA")
summary(dryfit)

dryfit <- sma(brdnet.no ~ PC_rich_BN, data = dry, method = "SMA")
summary(dryfit)

######################
#to plot it all
par(mfrow=c(2,2))
plot(jitter(brdnet.no, 0.5) ~ jitter(PC_rich_all, 0.5), data = dry, main = "All, dry season")
abline (0, 1.22, col="red", lty=2)
abline(0,1, col="dark grey", lty=2)
plot(jitter(brdnet.no, 0.5) ~ jitter(PC_rich,0.5), data = dry, main ="only birdnet trained, dry")
abline (0, 1.90, col="red", lty=2)
abline(0,1, col="dark grey", lty=2)
plot(jitter(brdnet.no, 0.5) ~ jitter(PC_rich_all, 0.5), data = wet, main = "All, wet season")
abline (0, 1.19, col="red", lty=2)
abline(0,1, col="dark grey", lty=2)
plot(jitter(brdnet.no, 0.5) ~ jitter(PC_rich,0.5), data = wet, main ="only birdnet trained, wet")
abline (0, 1.77, col="red", lty=2)
abline(0,1, col="dark grey", lty=2)

#########################################

#trying to make these look more attractive using ggplot2

library(ggplot2)
library(patchwork)
dry$season<- dry$Campaign
wet$season <- wet$Campaign
dry$season <- "Dry"
wet$season <- "Wet"

# Create separate datasets for each combination - with consistent column names
dry_all <- data.frame(
  x_var = dry$PC_rich_all,
  brdnet.no = dry$brdnet.no,
  season = "Dry",
  species_type = "All species",
  slope = 1.22
)

dry_trained <- data.frame(
  x_var = dry$PC_rich_BN,
  brdnet.no = dry$brdnet.no,
  season = "Dry",
  species_type = "BirdNET trained",
  slope = 1.90
)

wet_all <- data.frame(
  x_var = wet$PC_rich_all,
  brdnet.no = wet$brdnet.no,
  season = "Wet",
  species_type = "All species",
  slope = 1.19
)

wet_trained <- data.frame(
  x_var = wet$PC_rich_BN,
  brdnet.no = wet$brdnet.no,
  season = "Wet",
  species_type = "BirdNET trained",
  slope = 1.77
)

# Combine all
plot_data <- rbind(dry_all, dry_trained, wet_all, wet_trained)

# Determine axis limits
axis_max <- max(c(plot_data$x_var, plot_data$brdnet.no), na.rm = TRUE) * 1.05

# Updated plotting function with clipped lines
plot_comparison <- function(data, season_filter, species_filter) {
  data_subset <- data[data$season == season_filter & 
                        data$species_type == species_filter, ]
  slope_val <- unique(data_subset$slope)
  
  # Set colors and shapes
  point_color <- ifelse(season_filter == "Dry", "#e76f51", "#2a9d8f")
  point_shape <- ifelse(species_filter == "All species", 16, 1)
  
  ggplot(data_subset, aes(x = x_var, y = brdnet.no)) +
    geom_abline(intercept = 0, slope = 1, 
                color = "grey30", linetype = "dashed", linewidth = 1) +
    geom_abline(intercept = 0, slope = slope_val, 
                color = "#d62828", linetype = "solid", linewidth = 1) +
    geom_jitter(width = 0.5, height = 0.5, 
                alpha = 0.7, size = 2.5, 
                color = point_color, shape = point_shape, stroke = 1.5) +
    labs(x = "Point count richness",
         y = "BirdNET richness") +
    xlim(0, axis_max) +
    ylim(0, axis_max) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey40", linewidth = 0.8)
    ) +
    coord_fixed(ratio = 1, clip = "on")  # Make sure lines are clipped
}

# Create all plots
p1 <- plot_comparison(plot_data, "Dry", "All species")
p2 <- plot_comparison(plot_data, "Dry", "BirdNET trained")
p3 <- plot_comparison(plot_data, "Wet", "All species")
p4 <- plot_comparison(plot_data, "Wet", "BirdNET trained")

# Customize each plot - back to the tag approach that worked
# p1 (top-left)
p1 <- p1 + 
  labs(title = "All species", x = "") +
  annotate("text", x = -Inf, y = Inf, label = "Dry", 
           hjust = -0.5, vjust = 1.5, angle = 90, 
           fontface = "bold", size = 4.5) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        plot.margin = margin(5.5, 2, 5.5, 45))

# p2 (top-right)
p2 <- p2 + 
  labs(title = "BirdNET trained, only", x = "", y = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        plot.margin = margin(5.5, 5.5, 5.5, 2))

# p3 (bottom-left)
p3 <- p3 + 
  annotate("text", x = -Inf, y = Inf, label = "Wet", 
           hjust = -0.5, vjust = 1.5, angle = 90, 
           fontface = "bold", size = 4.5) +
  theme(plot.margin = margin(5.5, 2, 5.5, 45))

# p4 (bottom-right)
p4 <- p4 + 
  labs(y = "") +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 2))

# Combine with panel labels A, B, C, D
final_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Comparison of BirdNET vs Point Count Species Richness",
    tag_levels = 'A',
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  ) &
  theme(plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.02, 0.98))  # Position A, B, C, D in top-left

final_plot

# Save
ggsave("birdnet_comparison.png", final_plot, 
       width = 10, height = 10, dpi = 300, units = "in")
# Save
ggsave("birdnet_comparison.png", final_plot, 
       width = 10, height = 10, dpi = 300, units = "in")