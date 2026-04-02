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

#d)
Model.best = update(Model.test.ml, method = "REML")
summary(Model.best)

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

