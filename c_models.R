source("b_exploration.R")
# C. Models

# ------------------------ Initial Model ------------------------

# Model 1: The "Kitchen Sink"
# Fixed: Years, Years Squared, Noise Level, and all Interactions
# Random: Individual Intercepts and Individual Slopes for Years
# Weights: Different residual variance for every year (varIdent)

model1.fit <- lme(hearing.loss ~ (years + years_sq) * Noise.level,
                    random = ~ years | subject, 
                    data = data,
                    weights = varIdent(form = ~ 1 | factor(years)),
                    method = "ML") # Use ML for LRT comparisons later


summary(model1.fit)

# ------------------------ Hyp 1 ------------------------

# Model 2: Reduced Interaction Model
# We keep the linear interaction (years * Noise.level) 
# but treat the quadratic term as a global "shape" for everyone.

model2.fit <- update(model1.fit, . ~ years * Noise.level + years_sq)

# Compare to the maximal model using Likelihood Ratio Test (LRT)
anova(model1.fit, model2.fit)


