source("b_exploration.R")
# C. Models

# ------------------------ Initial Model ------------------------

# Model 1: The "Kitchen Sink"
# Fixed: Years, Years Squared, Noise Level, and all Interactions
# Random: Individual Intercepts and Individual Slopes for Years
# Weights: Different residual variance for every year (varIdent)

# Model 1 Model with Loaded Mean Structure
model1.fit <- lme(hearing.loss ~ (years + exp_years) * Noise.level,
                  random = ~ years | subject, 
                  data = data,
                  weights = varIdent(form = ~ 1 | (years)),
                  method = "REML") 
# Use REML for LRT comparisons later


summary(model1.fit)

# ----------------------------- Random Effects -----------------------------


# ------------------------ Hyp 1 ------------------------
# Model 2A Excludes the random slope
model2a.fit <- update(model1.fit, random = ~ 1 | subject) # Remove random slope for years
# Hypothesis 1 --> Null Hypothesis: Drop u1j (variance of random slope = 0)
# --> Alternative Hypothesis: Keep u1j (variance of random slope != 0) 
anova(model1.fit, model2a.fit) # Test Hypothesis 1
# Keep Model 1 --> Reject Null Hypothesis


# ------------------------ Hyp 2 ------------------------
# Model 2B Excludes the random intercept
model2b.fit <- update(model1.fit, random = ~ years - 1 | subject) # Remove random intercept
# Hypothesis 2 --> Null Hypothesis: Drop u0j (variance of random intercept = 0)
# --> Alternative Hypothesis: Keep u0j (variance of random intercept > 0)
anova(model1.fit, model2b.fit) # Test Hypothesis 2
# Keep Model 1 --> Reject Null Hypothesis

# ----------------------------- Residual Structure -----------------------------


# ------------------------ Hyp 3 ------------------------
# Model 3A Removed Heterogeneous Residual by years
model3a.fit <- lme(hearing.loss ~ (years + exp_years) * Noise.level,
                   random = ~ years | subject, 
                   data = data,
                   method = "REML")
# Hypothesis 3 --> Null Hypothesis: Homogeneous residual variance (all variances of tea_level groups are the same)
# Alternative Hypothesis: Residual variances are not all equal
anova(model1.fit, model3a.fit) # Test Hypothesis 3
# Keep Model 1 --> Reject Null Hypothesis

# ------------------------ Hyp 4 ------------------------

ctrl <- lmeControl(maxIter = 200, msMaxIter = 200, niterEM = 50, opt = "optim")

# Test AR correlation
# Model 3B AR1 Correlation Added
model3b.fit <- update(model1.fit, correlation = corAR1(form = ~ 1 | subject), control = ctrl)
# Hypothesis 4 --> Null Hypothesis: phi = 0
# Alternative Hypothesis: phi != 0
anova(model1.fit, model3b.fit) # Test Hypothesis 4
# Keep Model 3B --> Reject Null Hypothesis 
summary(model3b.fit)

# ----------------------------- Fixed Effects -----------------------------

# ------------------------ Hyp 5 ------------------------

summary(model3b.fit)

# fit 3b with ml
model3b.ml.fit = update(model3b.fit, method = "ML")

# Model 4A Removed exp_years:Noise.level
model4a.ml.fit <- update(model3b.ml.fit, . ~ years * Noise.level + exp_years)
# Hypothesis 5 --> Null Hypothesis: Drop exp_years:Noise.level (B_exp_years:Noise.level = 0)
# Alternative Hypothesis: Keep exp_years:Noise.level (B_exp_years:Noise.level != 0)
anova(model3b.ml.fit, model4a.ml.fit) # Test Hypothesis 5
# Keep Model 4a --> Fail to Reject Null Hypothesis
summary(model4a.ml.fit)


# ------------------------ Hyp 6 ------------------------

anova(model4a.ml.fit)

# Removed exp_years 
model4b.ml.fit <- update(model4a.ml.fit, . ~ years * Noise.level)
summary(model4b.ml.fit)
# Hypothesis 6 --> Null Hypothesis: Drop exp_years (B_exp_years = 0)
# Alternative Hypothesis: Keep exp_years (B_exp_years != 0)
anova(model4a.ml.fit, model4b.ml.fit) # Test Hypothesis 6
# Keep Model 4A --> Reject Null Hypothesis
anova(model4a.ml.fit)


# ------------------------ Final Model ------------------------

final.model = update(model4a.ml.fit, method = "REML")
summary(final.model)



# after project code -------


test = update(final.model, random = ~ years + I(years^2) | subject)
summary(test)

# Wrap 'years' in I() to perform the math inside the formula
test_power <- update(final.model, 
                     weights = varPower(form = ~ I(years + 1)))
summary(test_power)

getVarCov(final.model, type = "conditional")




# try to merge no and low
data$nlevel[data$Noise.level == "High"] <- 1
data$nlevel[data$Noise.level == "No" | data$Noise.level == "Low"] <- 2

test2 = update(final.model, . ~ years * Noise.level + exp_years, method = "ML")
final.model.ml = update(final.model, method = "ML")
summary(test2)

anova(test2, final.model.ml)
