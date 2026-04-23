# ============================================
#  Online Dating Income Analysis (R Script)
# ============================================

# --------------------------------------------
# 1. Import Dataset
# --------------------------------------------
OnlineDating <- read.csv("OnlineDating.csv", header = TRUE)

# View variable names
names(OnlineDating)

# Assign variables
sex        <- OnlineDating$sex
age        <- OnlineDating$age
body_type  <- OnlineDating$body_type
income     <- OnlineDating$income
diet       <- OnlineDating$diet
smokes     <- OnlineDating$smokes
status     <- OnlineDating$status
height     <- OnlineDating$height


# --------------------------------------------
# 2. Summary Statistics
# --------------------------------------------
summary(OnlineDating)


# --------------------------------------------
# 3. Distribution of Income
# --------------------------------------------
hist(income,
     main = "Histogram of Income",
     xlab = "Income",
     col = "lightblue")

# Interpretation:
# The distribution of income is right-skewed.
# Most individuals earn between 0 and 100,000 USD,
# with a small number of individuals earning significantly higher incomes.


# --------------------------------------------
# 4. Relationship Between Income and Predictors
# --------------------------------------------
par(mfrow = c(1, 3))

# Age vs Income
plot(age, income,
     main = "Income vs Age",
     xlab = "Age",
     ylab = "Income")

# Interpretation:
# Most individuals earn relatively low incomes across all ages.
# Only a few individuals earn very high incomes,
# suggesting a weak relationship between age and income.

# Income vs Sex
boxplot(income ~ sex,
        main = "Income by Sex",
        xlab = "Sex",
        ylab = "Income")

# Interpretation:
# Males appear to have a slightly higher median income and greater variability than females.
# However, there is considerable overlap between the groups,
# indicating that sex may not strongly influence income.

# Income vs Body Type
boxplot(income ~ body_type,
        main = "Income by Body Type",
        xlab = "Body Type",
        ylab = "Income")

# Interpretation:
# Income varies across body types, but differences are minimal.
# There is substantial overlap between categories,
# suggesting that body type is not a strong predictor of income.


# --------------------------------------------
# 5. Convert Variables to Factors
# --------------------------------------------
sex_factor        <- as.factor(sex)
body_type_factor  <- as.factor(body_type)
diet_factor       <- as.factor(diet)
smokes_factor     <- as.factor(smokes)
status_factor     <- as.factor(status)


# --------------------------------------------
# 6–7. Set Reference Levels
# --------------------------------------------
sex_ref        <- relevel(sex_factor, ref = "f")
body_type_ref  <- relevel(body_type_factor, ref = "average")
diet_ref       <- relevel(diet_factor, ref = "0")
smokes_ref     <- relevel(smokes_factor, ref = "0")
status_ref     <- relevel(status_factor, ref = "0")

# Justification:
# - "f" (female) is used as a baseline for gender comparison.
# - "average" represents a natural baseline for body type.
# - "0" represents the absence of a condition (e.g., non-smoker).


# --------------------------------------------
# 8. Fit Multiple Linear Regression Model
# --------------------------------------------
mymodel <- lm(income ~ age + sex_ref + body_type_ref +
                diet_ref + smokes_ref +
                status_ref + height,
              data = OnlineDating)

summary(mymodel)


# --------------------------------------------
# 9. Interpretation of Coefficients
# --------------------------------------------

# Sex:
# Holding all other variables constant, males earn on average more than females.
# However, this effect is not statistically significant (p > 0.05).

# Age:
# For each additional year of age, income increases slightly on average.
# This effect is weak and not statistically significant.

# Body Type:
# Compared to individuals with an average body type,
# other body types show small differences in income.
# These effects are not statistically significant.

# Diet:
# Individuals with different dietary habits show some variation in income,
# but the effect is not statistically significant.

# Smoking:
# Smokers tend to earn less on average than non-smokers.
# However, this effect is not statistically significant.

# Relationship Status:
# Income varies slightly across relationship statuses,
# but the effect is not statistically significant.

# Height:
# There is a small positive association between height and income,
# but it is not statistically significant.


# --------------------------------------------
# 10. Overall Model Significance
# --------------------------------------------
# H0: β1 = β2 = ... = βk = 0
# H1: At least one βj ≠ 0

# Based on the F-statistic p-value (~0.849),
# we fail to reject the null hypothesis at α = 0.05.

# Conclusion:
# The explanatory variables, as a group, do not significantly
# explain variation in income.


# --------------------------------------------
# 11. Diagnostic Plots
# --------------------------------------------
par(mfrow = c(2, 2))
plot(mymodel)


# --------------------------------------------
# 12. Assumption Checks
# --------------------------------------------

# Linearity:
# Residual plots do not show a clear linear pattern,
# suggesting that the linearity assumption may not hold.

# Normality:
# The Q-Q plot deviates from the reference line,
# indicating that residuals are not normally distributed.

# Homoscedasticity:
# The spread of residuals is uneven,
# suggesting heteroscedasticity (non-constant variance).


# --------------------------------------------
# 13. Influential Observations
# --------------------------------------------

# Cook's Distance
plot(cooks.distance(mymodel), type = "h",
     main = "Cook's Distance")

# Leverage
n <- length(hatvalues(mymodel))
p <- length(coef(mymodel))

which(hatvalues(mymodel) > (2 * p / n))

# Interpretation:
# Observations with high leverage or large Cook’s distance
# may have a strong influence on the model and should be examined further.


# --------------------------------------------
# 14. Model Selection
# --------------------------------------------

# Stepwise selection
step_model <- step(mymodel, direction = "both")
summary(step_model)

# Interpretation:
# The stepwise procedure selects the intercept-only model (income ~ 1),
# indicating that none of the predictors significantly improve the model.


# --------------------------------------------
# 15. Log Transformation
# --------------------------------------------
incomeLog <- log(income + 1)

# Refit model
mymodel2 <- lm(incomeLog ~ age + sex_ref + body_type_ref +
                 diet_ref + smokes_ref +
                 status_ref + height,
               data = OnlineDating)

summary(mymodel2)


# --------------------------------------------
# 16. Model Comparison
# --------------------------------------------
summary(mymodel)
summary(mymodel2)

plot(mymodel2, 1)

# Interpretation:
# The log transformation improves the distribution of residuals
# and stabilizes variance.

# Final Conclusion:
# The transformed model performs better than the original model,
# but the predictors still do not strongly explain income.