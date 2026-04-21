rm(list = ls())
setwd("~/Development/CSC_AT_LVC/DSC_340_Machine_learn/DSC_Project_2") # For Brian's mac
autism = read.csv("HW8/autism.csv")
attach(autism)

sicdegp.f <- factor(sicdegp)
age.f <- factor(age)
# Add the new variables to the data frame object.
autism.updated <- data.frame(autism, sicdegp.f, age.f)


# Number of Observations at each level of AGE
print(summary(age.f))
print(table(sicdegp.f, age.f))

# Overall summary for VSAE
summary(vsae)

# VSAE means at each AGE
tapply(vsae, age.f, mean, na.rm=TRUE)

# VSAE minimum values at each AGE
tapply(vsae, age.f, min, na.rm=TRUE)

# VSAE maximum values at each AGE
tapply(vsae, age.f, max, na.rm=TRUE)

library(lattice)

library(nlme)
autism.g1 <- groupedData(vsae ~ age | childid,
                         outer = ~ sicdegp.f, data = autism.updated)

# Generate individual profiles in Figure 6.1.
plot(autism.g1, display = "childid", outer = TRUE, aspect = 2,
     key = F, xlab = "Age (Years)", ylab = "VSAE",
     main = "Individual Data by SICD Group")


autism.g2 <- groupedData(vsae ~ age | sicdegp.f,
                         order.groups = F, data = autism.updated)

# Generate mean profiles in Figure 6.2.
#plot(autism.g2, display = "sicdegp", aspect = 2, key = F,
     #xlab = "Age (Years)", ylab = "VSAE",
     #main = "Mean Profiles by SICD Group")


# Compute age.2 (AGE minus 2) and age.2sq (AGE2 squared).
age.2 <- age - 2
age.2sq <- age.2*age.2

# Recode the SICDEGP factor for model fitting.
sicdegp2 <- sicdegp
sicdegp2[sicdegp == 3] <- 0
sicdegp2[sicdegp == 2] <- 2
sicdegp2[sicdegp == 1] <- 1
sicdegp2.f <- factor(sicdegp2)

# Omit two records with VSAE = NA, and add the recoded
# variables to the new data frame object.
autism.updated <- subset(data.frame(autism, sicdegp2.f, age.2),
                        !is.na(vsae))

# Could also cut like this
# sicdegp2 <- cut(sicdegp, breaks = 0:3, labels= FALSE)

autism.grouped <- groupedData(vsae ~ age.2 | childid,
                              data = autism.updated, order.groups = F)


model6.1.fit <- lme(vsae ~ age.2 + I(age.2^2) + sicdegp2.f +
                        age.2:sicdegp2.f + I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2), method= "REML",
                    data = autism.grouped)

model6.2.fit <- lme(vsae ~ age.2 + I(age.2^2) + sicdegp2.f +
                        age.2:sicdegp2.f + I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2) - 1, method= "REML",
                    data = autism.grouped)
summary(model6.2.fit)

model6.2a.fit <- update(model6.2.fit, random = ~ age.2 - 1)

h6.1.pvalue <- 0.5*(1-pchisq(83.9,1)) + 0.5*(1-pchisq(83.9,2))
print(h6.1.pvalue)

model6.2.ml.fit <- update(model6.2.fit, method = "ML")

model6.3.ml.fit <- update(model6.2.ml.fit,
                          fixed = ~ age.2 + I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f)

anova(model6.2.ml.fit, model6.3.ml.fit)

model6.3.fit <- update(model6.2.fit,
                       fixed = ~ age.2 + I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f)
summary(model6.3.fit)


# ------------- Residual --------------
# Figure 6.8.
plot(model6.3.fit,
       resid(., type = "p") ~ fitted(.) | factor(sicdegp),
       id = .05, layout=c(3,1), aspect=2, abline=0)

# Figure 6.9.
plot(model6.3.fit, resid(.) ~ age.2, abline = 0)
     
# Figure 6.10.
qqnorm(model6.3.fit,
    ~resid(.) | factor(sicdegp) ,
              layout = c(3,1), aspect = 2, id = 0.05)

# Figure 6.11.
qqnorm(model6.3.fit, ~ranef(.) , id = 0.10)

# Figure 6.12.
pairs(model6.3.fit,
        ~ranef(.) | factor(sicdegp),
        id = ~childid == 124, layout = c(3, 1), aspect = 2)

# Figure 6.13.
plot(model6.3.fit, vsae ~ fitted(.) | factor(sicdegp),
       id = 0.05, layout = c(3,1) , aspect = 2)

# Refit without 124 and 46
autism.grouped2 <- autism.grouped[(autism.grouped$childid != 124 &
                                       autism.grouped$childid != 46),]
model6.3.fit.out <- update(model6.3.fit, data = autism.grouped2)

summary(model6.3.fit.out)
intervals(model6.3.fit.out)


# ---------------- HW -------------------

#a) 
autism.sicdegp.group <- groupedData(vsae ~ age | childid, 
                                    outer = ~ sicdegp2.f, 
                                    data = autism.updated)

tapply(autism.updated$vsae, autism.updated$sicdegp2.f, sd, na.rm = TRUE)

library(ggplot2)

# Ensure sicdegp is treated as a factor for the x-axis
ggplot(autism.updated, aes(x = as.factor(sicdegp), y = vsae, fill = as.factor(sicdegp))) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    labs(title = "Distribution of VSAE by SICDEGP Levels",
         x = "SICDEGP Level",
         y = "VSAE Score",
         fill = "Group") +
    theme_minimal()

#b) 
model6.4.fit = update(model6.3.fit, weights = varIdent(form = ~ 1 | sicdegp2.f))
print(summary(model6.4.fit))

anova(model6.3.fit, model6.4.fit)

#c)
Model.better = model6.4.fit

Model.test.ml = update(Model.better, . ~ . + I((age - 2)^3), method = "ML")
Model.better.ml = update(Model.better, method = "ML")

anova(Model.better.ml, Model.test.ml)













# ----------------------------------- In class Code -----------------------------------

#d)
Model.best = update(Model.test.ml, method = "REML")
summary(Model.best)


# Linear mixed-effects model fit by REML
#   Data: autism.grouped 
#       AIC      BIC    logLik
#   4542.69 4604.294 -2257.345
# 
# Random effects:
#  Formula: ~age.2 + I(age.2^2) - 1 | childid
#  Structure: General positive-definite, Log-Cholesky parametrization
#            StdDev    Corr  
# age.2      4.4063494 age.2 
# I(age.2^2) 0.3863845 -0.482
# Residual   9.0518482       
# 
# Variance function:
#  Structure: Different standard deviations per stratum
#  Formula: ~1 | sicdegp2.f 
#  Parameter estimates:
#         0         1         2 
# 1.0000000 0.4477601 0.4897606 
# Fixed effects:  vsae ~ age.2 + I(age.2^2) + sicdegp2.f + I((age - 2)^3) + age.2:sicdegp2.f 
#                       Value Std.Error  DF   t-value p-value
# (Intercept)       12.212929 1.1251892 447 10.854112  0.0000
# age.2              9.069647 0.8814090 447 10.289941  0.0000
# I(age.2^2)        -0.589063 0.1259635 447 -4.676456  0.0000
# sicdegp2.f1       -4.693866 1.1960070 155 -3.924614  0.0001
# sicdegp2.f2       -3.396562 1.1847979 155 -2.866786  0.0047
# I((age - 2)^3)     0.042127 0.0074713 447  5.638505  0.0000
# age.2:sicdegp2.f1 -4.389117 0.9084657 447 -4.831351  0.0000
# age.2:sicdegp2.f2 -3.776408 0.8620045 447 -4.380961  0.0000
#  Correlation: 
#                   (Intr) age.2  I(.2^2 scd2.1 scd2.2 I((-2) a.2:2.1
# age.2             -0.384                                           
# I(age.2^2)         0.207 -0.599                                    
# sicdegp2.f1       -0.909  0.272 -0.051                             
# sicdegp2.f2       -0.915  0.268 -0.041  0.854                      
# I((age - 2)^3)    -0.160  0.506 -0.940  0.020  0.012               
# age.2:sicdegp2.f1  0.250 -0.628  0.039 -0.280 -0.230 -0.015        
# age.2:sicdegp2.f2  0.261 -0.657  0.035 -0.239 -0.291 -0.014  0.613 
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.66483469 -0.40971559 -0.06558837  0.30801656  6.01766768 
# 
# Number of Observations: 610
# Number of Groups: 158 



# Boxplot by years
boxplot(vsae ~ age, 
        data = autism.updated,
        layout = c(1, 1), 
        main = "VSAE by years Boxplot",
        xlab = "Age", 
        ylab = "VSAE",
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




Model.test_variance = update(Model.best, 
                             method = "REML",
                             weights = varComb(varIdent(form = ~ 1 | sicdegp2.f), 
                                               varPower(form = ~ age)))
summary(Model.test_variance)

# Linear mixed-effects model fit by REML
#   Data: autism.grouped 
#        AIC      BIC    logLik
#   4462.162 4528.166 -2216.081
# 
# Random effects:
#  Formula: ~age.2 + I(age.2^2) - 1 | childid
#  Structure: General positive-definite, Log-Cholesky parametrization
#            StdDev    Corr  
# age.2      3.8394745 age.2 
# I(age.2^2) 0.2264215 -0.286
# Residual   3.0185941       
# 
# Combination of variance functions: 
#  Structure: Different standard deviations per stratum
#  Formula: ~1 | sicdegp2.f 
#  Parameter estimates:
#         0         1         2 
# 1.0000000 0.5735691 0.6481098 
#  Structure: Power of variance covariate
#  Formula: ~age 
#  Parameter estimates:
#    power 
# 0.742179 
# Fixed effects:  vsae ~ age.2 + I(age.2^2) + sicdegp2.f + I((age - 2)^3) + age.2:sicdegp2.f 
#                       Value Std.Error  DF   t-value p-value
# (Intercept)       12.429581 0.6977423 447 17.813999       0
# age.2              9.259504 0.8383915 447 11.044367       0
# I(age.2^2)        -0.659557 0.1460564 447 -4.515771       0
# sicdegp2.f1       -5.213654 0.7734596 155 -6.740694       0
# sicdegp2.f2       -3.706807 0.7689799 155 -4.820421       0
# I((age - 2)^3)     0.045307 0.0099510 447  4.553036       0
# age.2:sicdegp2.f1 -4.216854 0.8778009 447 -4.803884       0
# age.2:sicdegp2.f2 -3.708704 0.8338373 447 -4.447754       0
#  Correlation: 
#                   (Intr) age.2  I(.2^2 scd2.1 scd2.2 I((-2) a.2:2.1
# age.2             -0.301                                           
# I(age.2^2)         0.163 -0.570                                    
# sicdegp2.f1       -0.873  0.185 -0.019                             
# sicdegp2.f2       -0.876  0.180 -0.014  0.787                      
# I((age - 2)^3)    -0.125  0.492 -0.962  0.008  0.006               
# age.2:sicdegp2.f1  0.183 -0.624  0.023 -0.216 -0.160 -0.012        
# age.2:sicdegp2.f2  0.191 -0.654  0.024 -0.168 -0.224 -0.017  0.608 
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.35994532 -0.47715113 -0.08507907  0.37096992  5.85620250 
# 
# Number of Observations: 610
# Number of Groups: 158 

anova(Model.best, Model.test_variance)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.best              1 14 4542.690 4604.294 -2257.345                        
# Model.test_variance     2 15 4462.162 4528.166 -2216.081 1 vs 2 82.52814  <.0001



Model.power_only = update(Model.test_variance, 
                    method = "REML",
                    weights =  varPower(form = ~ age))
Model.group_only = update(Model.test_variance, 
                    method = "REML",
                    weights = varIdent(form = ~ 1 | sicdegp2.f))
anova(Model.power_only, Model.group_only, Model.test_variance)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.power_only        1 13 4499.709 4556.913 -2236.855                        
# Model.group_only        2 14 4542.690 4604.294 -2257.345 1 vs 2 40.98069  <.0001
# Model.test_variance     3 15 4462.162 4528.166 -2216.081 2 vs 3 82.52814  <.0001

Model.add_correlation = update(Model.test_variance, 
                             method = "REML",
                             correlation = corAR1(form = ~ age | childid))
summary(Model.add_correlation)


# Linear mixed-effects model fit by REML
#   Data: autism.grouped 
#        AIC      BIC    logLik
#   4442.222 4512.626 -2205.111
# 
# Random effects:
#  Formula: ~age.2 + I(age.2^2) - 1 | childid
#  Structure: General positive-definite, Log-Cholesky parametrization
#            StdDev    Corr 
# age.2      3.7457613 age.2
# I(age.2^2) 0.2337815 -0.26
# Residual   3.1103470      
# 
# Correlation Structure: ARMA(1,0)
#  Formula: ~age | childid 
#  Parameter estimate(s):
#     Phi1 
# 0.403825 
# Combination of variance functions: 
#  Structure: Different standard deviations per stratum
#  Formula: ~1 | sicdegp2.f 
#  Parameter estimates:
#         0         1         2 
# 1.0000000 0.5525931 0.6312792 
#  Structure: Power of variance covariate
#  Formula: ~age 
#  Parameter estimates:
#     power 
# 0.7456841 
# Fixed effects:  vsae ~ age.2 + I(age.2^2) + sicdegp2.f + I((age - 2)^3) + age.2:sicdegp2.f 
#                       Value Std.Error  DF   t-value p-value
# (Intercept)       12.359375 0.7975410 447 15.496853   0e+00
# age.2              9.531215 0.7986238 447 11.934550   0e+00
# I(age.2^2)        -0.713760 0.1328961 447 -5.370817   0e+00
# sicdegp2.f1       -5.283201 0.8878236 155 -5.950733   0e+00
# sicdegp2.f2       -3.669619 0.8857179 155 -4.143101   1e-04
# I((age - 2)^3)     0.048269 0.0092863 447  5.197856   0e+00
# age.2:sicdegp2.f1 -4.230955 0.8713001 447 -4.855910   0e+00
# age.2:sicdegp2.f2 -3.755370 0.8284849 447 -4.532816   0e+00
#  Correlation: 
#                   (Intr) age.2  I(.2^2 scd2.1 scd2.2 I((-2) a.2:2.1
# age.2             -0.233                                           
# I(age.2^2)         0.068 -0.514                                    
# sicdegp2.f1       -0.892  0.176 -0.013                             
# sicdegp2.f2       -0.893  0.173 -0.009  0.801                      
# I((age - 2)^3)    -0.042  0.431 -0.957  0.003  0.003               
# age.2:sicdegp2.f1  0.172 -0.659  0.027 -0.196 -0.151 -0.012        
# age.2:sicdegp2.f2  0.180 -0.690  0.029 -0.159 -0.204 -0.019  0.615 
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.33565649 -0.46003907 -0.09307167  0.35324605  5.85660936 
# 
# Number of Observations: 610
# Number of Groups: 158 



anova(Model.group_only, Model.power_only, Model.test_variance, Model.add_correlation)


# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.group_only          1 14 4542.690 4604.294 -2257.345                        
# Model.power_only          2 13 4499.709 4556.913 -2236.855 1 vs 2 40.98069  <.0001
# Model.test_variance       3 15 4462.162 4528.166 -2216.081 2 vs 3 41.54745  <.0001
# Model.add_correlation     4 16 4442.222 4512.626 -2205.111 3 vs 4 21.93941  <.0001


Model.add_quart = update(Model.add_correlation, 
                         method = "ML",
                         fixed = . ~ . + I((age - 2)^4) )
Model.add_correlation.ml = update(Model.add_correlation, method = "ML")
anova(Model.add_correlation.ml, Model.add_quart)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.add_correlation.ml     1 16 4433.333 4503.949 -2200.667                        
# Model.add_quart              2 17 4431.158 4506.187 -2198.579 1 vs 2 4.175037   0.041


Model.remove_age = lme(vsae ~ sicdegp2.f,
                       data = autism.grouped,
                       random = ~ age.2 + I(age.2^2) - 1 | childid,
                       weights = varComb(varIdent(form = ~ 1 | sicdegp2.f), 
                                         varPower(form = ~ age)),
                       correlation = corAR1(form = ~ age | childid),
                       method = "REML")
summary(Model.remove_age)

# Linear mixed-effects model fit by REML
#   Data: autism.grouped 
#        AIC      BIC    logLik
#   4587.072 4635.565 -2282.536
# 
# Random effects:
#  Formula: ~age.2 + I(age.2^2) - 1 | childid
#  Structure: General positive-definite, Log-Cholesky parametrization
#            StdDev    Corr 
# age.2      5.9244239 age.2
# I(age.2^2) 0.2057669 -0.18
# Residual   3.0918893      
# 
# Correlation Structure: ARMA(1,0)
#  Formula: ~age | childid 
#  Parameter estimate(s):
#      Phi1 
# 0.2970668 
# Combination of variance functions: 
#  Structure: Different standard deviations per stratum
#  Formula: ~1 | sicdegp2.f 
#  Parameter estimates:
#         0         1         2 
# 1.0000000 0.5804717 0.6485669 
#  Structure: Power of variance covariate
#  Formula: ~age 
#  Parameter estimates:
#     power 
# 0.7502549 
# Fixed effects:  vsae ~ sicdegp2.f 
#                 Value Std.Error  DF   t-value p-value
# (Intercept) 13.625764 0.7625671 452 17.868282       0
# sicdegp2.f1 -6.046615 0.8634802 155 -7.002609       0
# sicdegp2.f2 -4.387946 0.8570920 155 -5.119574       0
#  Correlation: 
#             (Intr) scd2.1
# sicdegp2.f1 -0.883       
# sicdegp2.f2 -0.890  0.786
# 
# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -2.44221734 -0.39975480  0.03498752  0.43598705  5.88247128 
# 
# Number of Observations: 610
# Number of Groups: 158 


Model.remove_age.ml = update(Model.remove_age, method = "ML")


# ----------- trying the removed age model ----------


autism.grouped$fitted <- fitted(Model.remove_age)
autism.grouped$resids <- resid(Model.remove_age)


# Boxplot by age
boxplot(resids ~ age, 
        data = autism.grouped,
        layout = c(1, 1), 
        main = "Residual by age Boxplot",
        xlab = "Age", 
        ylab = "Residual",
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




xyplot(resids ~ fitted | factor(age), 
       data = autism.grouped,
       # 5 columns, 2 rows (for 10 years total)
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values by Age",
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


# ----------- trying to plot the added quartic term -------


autism.grouped$fitted <- fitted(Model.add_quart)
autism.grouped$resids <- resid(Model.add_quart)


# Boxplot by age
boxplot(resids ~ age, 
        data = autism.grouped,
        layout = c(1, 1), 
        main = "Residual by age Boxplot",
        xlab = "Age", 
        ylab = "Residual",
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



xyplot(resids ~ fitted | age, 
       data = autism.grouped,
       # 5 columns, 2 rows (for 10 years total
       main = "Figure 3.8: Conditional Residuals vs. Predicted Values by Age",
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


# maybe we take a log?






# ----------------------------------- End In class Code -----------------------------------












# Figure 6.8.
plot(Model.best,
     resid(., type = "p") ~ fitted(.) | factor(sicdegp),
     id = .05, layout=c(3,1), aspect=2, abline=0)

# Figure 6.9.
plot(Model.best, resid(.) ~ age.2, abline = 0)

# Figure 6.10.
qqnorm(Model.best,
       ~resid(.) | factor(sicdegp) ,
       layout = c(3,1), aspect = 2, id = 0.05)

# Figure 6.11.
qqnorm(Model.best, ~ranef(.) , id = 0.10)

# Figure 6.12.
pairs(Model.best,
      ~ranef(.) | factor(sicdegp),
      id = ~childid == 124, layout = c(3, 1), aspect = 2)

# Figure 6.13.
plot(Model.best, vsae ~ fitted(.) | factor(sicdegp),
     id = 0.05, layout = c(3,1) , aspect = 2)

