rm(list = ls())
setwd("~/Development/CSC_AT_LVC/DSC_340_Machine_learn/DSC_Project_2") # For Brian's mac
load("NoisestudyHLN.RData")

data = NoisestudyHLN

library(nlme)
library(dplyr)
library(knitr)
library(ggplot2)
library(lattice)
library(effects)

# ---------------------------------------------------Transform--------------------------------------------------------------------------

# treat subject as a factor
data$subject <- as.factor(data$subject)

# Order by subject to ensure proper grouping in analyses and visualizations
data$subject <- factor(data$subject, levels = sort(as.numeric(unique(data$subject))))

# Make years index start at 0
data$years <- data$years - 1

# ---------------------------------------------------Analysis Tables--------------------------------------------------------------------------

# analyze lvl 1 DP
analysis_table_lvl_1 <- data %>%
  summarise(
    N_Obs = n(),
    mean = mean(hearing.loss, na.rm = TRUE),
    std_dev = sd(hearing.loss, na.rm = TRUE),
    minimum = min(hearing.loss, na.rm = TRUE),
    maximum = max(hearing.loss, na.rm = TRUE),
  )

print(kable(analysis_table_lvl_1, 
            col.names = c("N",  "mean_hearing.loss", "std_dev_hearing.loss", "min_hearing.loss", "max_hearing.loss"),
            digits = 2, 
            caption = "                 Analysis Variables: LVL 1 factors (time)\n-----------------------------------------------------------------------------------"))

# analyze lvl 1 time-based info
analysis_table_lvl_1_time <- data %>%
  group_by(years) %>%
  summarise(
    N_Obs = n(),
    mean = mean(hearing.loss, na.rm = TRUE),
    std_dev = sd(hearing.loss, na.rm = TRUE),
    minimum = min(hearing.loss, na.rm = TRUE),
    maximum = max(hearing.loss, na.rm = TRUE),
  )

print(kable(analysis_table_lvl_1_time, 
            col.names = c("Years", "N", "mean_hearing.loss", "std_dev_hearing.loss", "min_hearing.loss", "max_hearing.loss"),
            digits = 2, 
            caption = "                 Analysis Variables: LVL 1 factors (time)\n-----------------------------------------------------------------------------------"))



# analyze lvl 2 data
analysis_table_lvl_2 <- data %>%
    group_by(subject) %>%
    summarize(
        mean_yearstea = mean(hearing.loss, na.rm = TRUE),
        sd_yearstea = sd(hearing.loss, na.rm = TRUE),
        minimum = min(hearing.loss, na.rm = TRUE),
        maximum = max(hearing.loss, na.rm = TRUE),
    )

print(kable(analysis_table_lvl_2, 
            col.names = c("Subject",
                          "mean_hearing.loss", "std_dev_hearing.loss", 
                          "min_hearing.loss", "max_hearing.loss"), 
            digits = 2, 
            caption = "         Analysis Variable : LVL 2 factors (Subject Level)\n----------------------------------------------------------------"))


# Analyze by noiselevel
analysis_table_noiselvl <- data %>%
  group_by(Noise.level) %>% 
  summarise(
    N_Obs = n(),
    mean = mean(hearing.loss, na.rm = TRUE),
    std_dev = sd(hearing.loss, na.rm = TRUE),
    minimum = min(hearing.loss, na.rm = TRUE),
    maximum = max(hearing.loss, na.rm = TRUE),
  ) %>%
  arrange(Noise.level)

print(kable(analysis_table_noiselvl, 
            col.names = c("Noise.Level", "N",  "mean", "std_dev", "minimum", "maximum"),
            digits = 2, 
            caption = "     Analysis : Hearing.loss by Noise.level\n------------------------------------------------------"))


# ---------------------------------------------------BW plots----------------------------------------------------------------------------


# Boxplot by Noise.level
bwplot(hearing.loss ~ Noise.level, 
       data = data,
       layout = c(1, 1), 
       main = "Hearing Loss by Noise.level Boxplot",
       xlab = "Noise Level", 
       ylab = "Hearing Loss",
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1),  
         plot.symbol = list(col = "black", pch = 1)    
       ))


# Boxplot by years
boxplot(hearing.loss ~ years, 
       data = data,
       layout = c(1, 1), 
       main = "Hearing Loss by years Boxplot",
       xlab = "Year", 
       ylab = "Hearing Loss",
       panel = function(x, y, ...) {
         panel.bwplot(x, y, ..., 
                      pch = "|",          
                      coef = 1.5,         
                      do.out = TRUE)
         m <- aggregate(y ~ x, FUN = mean)
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.5)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1),  
         plot.symbol = list(col = "black", pch = 1)    
       ))




# Ensure years is a factor for proper boxplot spacing
data$years <- as.factor(data$years)

bwplot(hearing.loss ~ years | Noise.level, 
       data = data,
       layout = c(3, 1), 
       main = "Hearing Loss Trends by Noise Level",
       xlab = "Year", 
       ylab = "Hearing Loss",
       panel = function(x, y, ...) {
         # 1. Draw the standard boxplots
         panel.bwplot(x, y, ..., 
                      pch = "|",           
                      coef = 1.5,          
                      do.out = TRUE)
         
         # 2. Calculate the means for each year
         # x is the factor index, y is hearing.loss
         m <- aggregate(y ~ x, FUN = mean)
         
         # 3. Add the mean points
         panel.points(m$x, m$y, pch = 18, col = "black", cex = 1.2)
         
         # 4. Add the smooth line connecting the means
         # We use a type "s" for a stair-step or "l" for direct lines.
         # For a 'smooth' look through points, panel.lines works best.
         panel.loess(x, y, col = "blue", lwd = 2, lty = 1)
       },
       par.settings = list(
         box.rectangle = list(col = "black", lwd = 1), 
         box.umbrella = list(col = "black", lty = 1),  
         plot.symbol = list(col = "black", pch = 1)    
       ))

# ------------------ By Subject -----------------------

# Ensure subject is a factor so R treats it as a grouping variable
data$subject <- as.factor(data$subject)

xyplot(hearing.loss ~ years | Noise.level, 
       data = data,
       groups = subject,      # This tells R to connect points by subject ID
       type = c("p", "l"),    # "p" for points, "l" for lines
       layout = c(3, 1), 
       main = "Individual Hearing Loss Trajectories",
       xlab = "Year", 
       ylab = "Hearing Loss",
       alpha = 0.5,           # Make lines slightly transparent to see overlaps
       lty = 1)


# Calculate the mean hearing loss per year, per noise level
noise_means <- aggregate(hearing.loss ~ years + Noise.level, data = data, FUN = mean)

# Plot individual subjects
xyplot(hearing.loss ~ years | subject, 
       data = data,
       type = c("p", "l"),
       col = "black",
       lty = 2,           # Dashed line for the individual subject
       layout = c(9, 6),  # 54 subjects fit well in a 9x6 grid
       as.table = TRUE,   # Starts subject 1 at the top left
       main = "Subject Trajectories vs. Group Average",
       xlab = "Year",
       ylab = "Hearing Loss",
       panel = function(x, y, subscripts, ...) {
         # Draw the individual subject's lines/points first
         panel.xyplot(x, y, ...)
         
         # Identify which Noise.level this specific subject belongs to
         current_noise <- unique(data$Noise.level[subscripts])
         
         # Pull the corresponding group means for that noise level
         group_data <- noise_means[noise_means$Noise.level == current_noise, ]
         
         # Overlay the group average as a thick red line
         panel.lines(group_data$years, group_data$hearing.loss, 
                     col = "red", lwd = 2, lty = 1)
       })


# Calculate the means for the overlay
noise_means <- aggregate(hearing.loss ~ years + Noise.level, data = data, FUN = mean)

# Function to plot a specific group correctly
plot_noise_group <- function(level) {
  
  # Filter the mean data for the red line
  group_avg <- subset(noise_means, Noise.level == level)
  
  # Plot only the subjects belonging to this noise level
  xyplot(hearing.loss ~ years | subject, 
         data = data,
         subset = (Noise.level == level), # Strictly limits data to this group
         type = c("p", "l"),
         col = "black", lty = 2, pch = 16,
         layout = c(6, 3), 
         as.table = TRUE,
         main = paste("Subjects in", level, "Noise Group"),
         panel = function(x, y, ...) {
           # This only plots the (x,y) for the specific subject in THIS panel
           panel.xyplot(x, y, ...) 
           
           # This overlays the SAME group average on every panel
           panel.lines(group_avg$years, group_avg$hearing.loss, 
                       col = "red", lwd = 2, lty = 1)
         })
}

# Generate the High Noise plot
print(plot_noise_group("Low"))
print(plot_noise_group("High"))
print(plot_noise_group("No"))


# Print some interesting cases for further exploration later
print(data[data$subject == 38, ])
print(data[data$subject == 42, ])
print(data[data$subject == 24, ])

