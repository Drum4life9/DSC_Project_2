source("b_exploration.R")
# C. Models

# ------------------------ Initial Model ------------------------

# Model 1: The "Kitchen Sink"
# Fixed: Years, Years Squared, Noise Level, and all Interactions
# Random: Individual Intercepts and Individual Slopes for Years
# Weights: Different residual variance for every year (varIdent)

model1.fit <- lme(hearing.loss ~ (years + exp_years) * Noise.level,
                    random = ~ years | subject, 
                    data = data,
                    weights = varIdent(form = ~ 1 | (years)),
                    method = "REML") # Use REML for LRT comparisons later


summary(model1.fit)

# ----------------------------- Random Effects -----------------------------


# ------------------------ Hyp 1 ------------------------

model2a.fit <- update(model1.fit, random = ~ 1 | subject) # Remove random slope for years
anova(model1.fit, model2a.fit) # keep random slope


# ------------------------ Hyp 2 ------------------------

model2b.fit <- update(model1.fit, random = ~ years - 1 | subject) # Remove random intercept
anova(model1.fit, model2b.fit) # keep random intercept



# ----------------------------- Residual Structure -----------------------------


# ------------------------ Hyp 3 ------------------------

model3a.fit <- lme(hearing.loss ~ (years + exp_years) * Noise.level,
                   random = ~ years | subject, 
                   data = data,
                   method = "REML")
anova(model1.fit, model3a.fit) # keep varIdent


# ------------------------ Hyp 4 ------------------------

ctrl <- lmeControl(maxIter = 200, msMaxIter = 200, niterEM = 50, opt = "optim")

# Test AR correlation
model3b.fit <- update(model1.fit, correlation = corAR1(form = ~ 1 | subject), control = ctrl)
anova(model1.fit, model3b.fit) # keep ARCorr
summary(model3b.fit)




# ------------------------ Hyp 5 ------------------------

summary(model3b.fit)

# fit 3 with ml
model3b.ml.fit = update(model3b.fit, method = "ML")

# model 4, remove exp_noise interaction term
model4a.ml.fit <- update(model3b.ml.fit, . ~ years * Noise.level + exp_years)

anova(model3b.ml.fit, model4a.ml.fit)
summary(model4a.fit)


# ------------------------ Hyp 6 ------------------------

anova(model4a.ml.fit)

# remove exp_years term because of high(ish) p-value
model4b.ml.fit <- update(model4a.ml.fit, . ~ years * Noise.level)
summary(model4b.ml.fit)

anova(model4a.ml.fit, model4b.ml.fit) # result: keep exp_years

anova(model4a.ml.fit)


# ------------------------ Hyp 7 ------------------------

final.model = update(model4a.ml.fit, method = "REML")
summary(final.model)

