source("b_exploration.R")
source("c_models.R")


# ----------------------------------- EBLUP ---------------------------------------

# Extract the random effects (EBLUPs)
random_effects <- ranef(final.model)

# Rename for clarity
colnames(random_effects) <- c("u0j_Intercept", "u1j_Slope")

# Add the subject ID as a column
random_effects$subject <- rownames(random_effects)

# Sort by the Intercept offset (u0j) to see most hearing-lost subjects
top_subjects <- random_effects[order(-random_effects$u0j_Intercept), ]

# View the top 5
head(top_subjects, 5)

# Sort by the Slope offset (u1j) 
top_slopes <- random_effects[order(-random_effects$u1j_Slope), ]

# View the top 5
head(top_slopes, 5)

# Interesting.... 28, 10, and 46 are the highest in both


# Get EBLUPS for tables in Latex
eblups <- ranef(final.model)

subject_info <- data %>%
    group_by(subject) %>%
    summarise(Noise.level = first(Noise.level))

eblup_df <- data.frame(
    subject = rownames(eblups),
    intercept_offset = eblups[,"(Intercept)"],
    slope_offset = eblups[,"years"]
) %>%
    left_join(subject_info, by = "subject")

get_top_6 <- function(df, group_name) {
    df %>%
        filter(Noise.level == group_name) %>%
        # We use abs() to find the most "extreme" subjects, 
        # or remove abs() if you only want the highest positive offsets
        arrange(desc(abs(intercept_offset))) %>%
        head(6)
}

top_no   <- get_top_6(eblup_df, "No")
top_low  <- get_top_6(eblup_df, "Low")
top_high <- get_top_6(eblup_df, "High")

print(top_no)
print(top_low)
print(top_high)



# ----------------------------------- Caterpillar of EBLUPS ------------------------------------------

re_df <- ranef(final.model, augFrame = TRUE)

re_df$subject <- rownames(re_df)
re_df$subject <- factor(re_df$subject, levels = sort(as.numeric(unique(re_df$subject))))

# Plot Intercepts ordered by classid
cp1 = dotplot(subject ~ `(Intercept)`, data = re_df,
              xlab = "Random Intercept Offset (u0j)",
              main = "Subject Intercepts (Ordered by ID)",
              panel = function(...) {
                  panel.abline(v = 0, lty = "dashed", col = "red")
                  panel.dotplot(...)
              })

# Plot mathkind Slopes ordered by classid
cp2 = dotplot(subject ~ years, data = re_df,
              xlab = "Random Slope Offset (u1j)",
              main = "Year Slopes (Ordered by ID)",
              panel = function(...) {
                  panel.abline(v = 0, lty = "dashed", col = "red")
                  panel.dotplot(...)
              })

print(cp1, split = c(1, 1, 2, 1), more = TRUE)
print(cp2, split = c(2, 1, 2, 1))

# ------------------------------------ Remove potential outliers --------------------------------------------

# Store your "Full Model" coefficients
full_coefs <- fixed.effects(final.model)

# Run the model without the three potential worst outliers
model_no_outliers <- update(final.model, subset = !(subject %in% c("28", "10", "46")))

# Compare the slopes
reduced_coefs <- fixed.effects(model_no_outliers)

# Look at the percent change
abs((full_coefs - reduced_coefs) / full_coefs) * 100


# --------------------------------- Cook's distance -------------------------------------------------

# Calculate Cook's Distance for Subjects
cd <- cooks.distance(final.model, level = "subject")

subj_names <- levels(final.model$groups$subject)

# Convert to a dataframe for lattice
cd_df <- data.frame(
    subject = subj_names,
    cooks_d = as.numeric(cd)
)

# Order subjects by their Cook's D value for a cleaner look
cd_df$subject <- reorder(cd_df$subject, cd_df$cooks_d)

# Calculate the threshold once
threshold_val <- 4/54

dotplot(subject ~ cooks_d, 
        data = cd_df,
        # Force the X-axis to include 0 and at least the threshold + a buffer
        xlim = c(0, max(cd_df$cooks_d, threshold_val) * 1.1),
        main = "Figure 3.10: Cook's Distance by Subject",
        xlab = "Cook's Distance",
        ylab = "Subject ID",
        par.settings = list(
            plot.symbol = list(pch = 16, col = "black", cex = 0.8)
        ),
        panel = function(x, y, ...) {
            # 1. Draw the grid lines first
            panel.grid(h = -1, v = -1, col = "gray90")
            
            # 2. Draw the threshold line (Red Dashed)
            panel.abline(v = threshold_val, col = "red", lty = 2, lwd = 2)
            
            # 3. Draw the dots on top
            panel.dotplot(x, y, ...)
            
            # 4. Add a label so you know it's there
            panel.text(x = threshold_val, y = 1, labels = "4/n", 
                       pos = 4, col = "red", cex = 0.8)
        })

# ----------------------------------- ACF ------------------------------------------------

# Check ACF
plot(ACF(final.model, resType = "normalized"), alpha = 0.05)

# ACF looks good overall except for 6, 8, and 9, but by the time
# we get lag 6 and onwards we have less data, causing a weaker fit

# --------------------------------------- Hist of Residuals --------------------------------------------

data$resids <- resid(final.model, type = "response")

histogram(~ resids | years, 
          data = data,
          layout = c(5, 2),           
          aspect = 1,      
          type = "percent",    
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)


histogram(~ resids | Noise.level, 
          data = data,
          layout = c(3, 1),
          aspect = 1,          
          type = "percent",    
          main = "Figure 3.6: Histograms of Conditional Raw Residuals",
          xlab = "Conditional Raw Residuals",
          ylab = "percent",
          col = "gray80",)


# --------------------------------------- Q-Q of residuals -------------------------------------------

# fig 3.7 - Q-Q plot
mu_val <- round(mean(data$resids), 4)
sigma_val <- round(sd(data$resids), 4)
qqmath(~ resids | years, 
       data = data,
       layout = c(5, 2),  
       labels = data$subject,
       identify = TRUE,
       abbreviate = FALSE,
       id = list(n = 3, labels = data$subject, cex = 0.7),
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
           space = "bottom",
           columns = 2,
           text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                         as.expression(substitute(sigma == s, list(s = sigma_val))))),
           points = list(pch = c(NA, NA)) 
       ),
       panel = function(x, ...) {
           panel.qqmath(x, ...)
           panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

qqmath(~ resids | Noise.level, 
       data = data,
       layout = c(3, 1),    
       id = 0.1,
       labels = data$subject,
       main = "Figure 3.7: Normal Q-Q Plots of Conditional Raw Residuals",
       xlab = "Standard Normal Quantiles",
       ylab = "Conditional Raw Residuals",
       key = list(
           space = "bottom",
           columns = 2,
           text = list(c(as.expression(substitute(mu == m, list(m = mu_val))),
                         as.expression(substitute(sigma == s, list(s = sigma_val))))),
           points = list(pch = c(NA, NA)) 
       ),
       panel = function(x, ...) {
           panel.qqmath(x, ...)
           panel.qqmathline(x, ...)    # Adds the reference line (y = x logic)
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.8)))

# Attach normalized residuals to your dataframe
data$norm_res <- residuals(final.model, type = "normalized")

# Get the 3 biggest positive and 3 biggest negative outliers per group
diagnostic_list <- data %>%
    group_by(Noise.level) %>%
    slice_max(order_by = abs(norm_res), n = 5) %>%
    select(subject, years, Noise.level, norm_res)

print(diagnostic_list)


# ---------------------------- Scatter of fitted / residuals ------------------------------------

# fig. 3.8
data$fitted <- fitted(final.model)
data$resids <- resid(final.model)

xyplot(resids ~ fitted | factor(years), 
       data = data,
       # 5 columns, 2 rows (for 10 years total)
       layout = c(5, 2), 
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values by Year",
       xlab = "Linear predictor (Fitted)",
       ylab = "Residual",
       # This ensures the Y-axis is the same for all panels so you can compare spread
       as.table = TRUE, 
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.abline(h = 0, lty = 2, col = "red") # Red line for better visibility
           panel.loess(x, y, col = "blue", lty = 1) # Adds a trend line to check for "bowing"
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.6)))

xyplot(resids ~ fitted | Noise.level, 
       data = data,
       layout = c(3, 1), 
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values by Noise.level",
       xlab = "Linear predictor (Fitted)",
       ylab = "Residual",
       # This ensures the Y-axis is the same for all panels so you can compare spread
       as.table = TRUE, 
       panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.abline(h = 0, lty = 2, col = "red") # Red line for better visibility
           panel.loess(x, y, col = "blue", lty = 1) # Adds a trend line to check for "bowing"
       },
       par.settings = list(plot.symbol = list(pch = 1, col = "black", cex = 0.6)))

# ---------------------------- BW plot of subject studentized resids -----------------------------------

# fig. 3.9
data$stud_resids <- resid(final.model, type = "normalized")

# Create a color mapping (one color per subject based on their Noise.level)
# Get the unique Noise.level for each subject in the order they appear on the X-axis
subj_noise <- with(data, tapply(as.character(Noise.level), subject, unique))
box_cols <- ifelse(subj_noise == "High", "tomato", 
                   ifelse(subj_noise == "Low", "lightblue", "lightgreen"))

bwplot(stud_resids ~ subject, 
       data = data,
       box.width = 0.5,
       main = "Figure 3.9: Studentized Residuals by Subject ID",
       xlab = "Subject ID",
       ylab = "Studentized Residuals",
       scales = list(
           x = list(
               draw = TRUE, 
               rot = 90,
               cex = 0.6
           )
       ),
       panel = function(x, y, ...) {
           panel.bwplot(x, y, ..., pch = "|")
           
           # Reference lines at 0 and +/- 2 (standard thresholds for outliers)
           panel.abline(h = 0, lty = 1, col = "black")
           panel.abline(h = c(-2, 2), lty = 2, col = "gray40")
           
           # Add means as open diamonds
           means <- aggregate(y ~ x, FUN = mean)
           panel.points(means$x, means$y, pch = 5, col = "black", cex = 0.8)
       },
       par.settings = list(
           box.rectangle = list(fill = box_cols, col = "black"),
           plot.symbol = list(pch = 1, col = "black", cex = 0.5)
       ))

# ------------------------------------------------------------------------------
# Figure 6.8
plot(final.model, 
     resid(., type = "p") ~ fitted(.) | Noise.level, 
     layout = c(3, 1),
     abline = 0)
plot(final.model, 
     resid(., type = "p") ~ fitted(.) | years, 
     layout = c(5, 2),
     abline = 0)

# Figure 6.9
plot(final.model, 
     resid(.) ~ years, 
     abline = 0)

# Figure 6.10
qqnorm(final.model,
       ~ resid(.) | Noise.level,
       layout = c(3,1))

# Figure 6.11
qqnorm(final.model,~ranef(.),id=0.10)

# Figure 6.12
pairs(final.model,
      ~ranef(.) |factor(Noise.level),
      id =~subject==28,layout=c(3,1),aspect=2)
pairs(final.model,
      ~ranef(.) |factor(Noise.level),
      id =~subject==38,layout=c(3,1),aspect=2)

# Figure 6.13
plot(final.model,hearing.loss~fitted(.)|factor(Noise.level),
     id =0.05,layout=c(3,1),aspect=2)
