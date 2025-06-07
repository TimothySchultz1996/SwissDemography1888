# Fertility and the Demographic Transition: R Code

## Data

# Load dataset
library(datasets)
data(swiss)
summary(swiss)

# Create Urbanization variable
swiss$Urbanization <- 100 - swiss$Agriculture

# Define key variables
X1 <- swiss$Urbanization
X2 <- swiss$Education
X3 <- swiss$Examination
X4 <- swiss$Catholic
X5 <- swiss$Infant.Mortality
Y  <- swiss$Fertility

## Descriptive Analysis

# Correlation check
cor(swiss$Urbanization, swiss$Education)

# Scatterplot matrix
library(GGally)

ggpairs(
  swiss[, c("Fertility", "Urbanization", "Education", "Examination", "Catholic", "Infant.Mortality")],
  lower = list(continuous = wrap("points", colour = "skyblue")),
  upper = list(continuous = wrap("cor", size = 3)),
  diag = list(continuous = wrap("densityDiag", colour = "red"))
)

# Fertility
par(mfrow = c(1, 2))
hist(Y, main = "Fertility", col = "skyblue", xlab = "")
boxplot(Y, main = "Fertility")

# Urbanization
par(mfrow = c(1, 2))
hist(X1, main = "Urbanization", col = "skyblue", xlab = "")
boxplot(X1, main = "Urbanization")

# Education
par(mfrow = c(1, 2))
hist(X2, main = "Education", col = "skyblue", xlab = "")
boxplot(X2, main = "Education")

# Examination
par(mfrow = c(1, 2))
hist(X3, main = "Examination", col = "skyblue", xlab = "")
boxplot(X3, main = "Examination")

# Catholic
par(mfrow = c(1, 2))
hist(X4, main = "Catholic", col = "skyblue", xlab = "")
boxplot(X4, main = "Catholic")

# Infant Mortality
par(mfrow = c(1, 2))
hist(X5, main = "Infant Mortality", col = "skyblue", xlab = "")
boxplot(X5, main = "Infant Mortality")

## Ordinary Least Squares (OLS)

# Simple Regression 1: Fertility ~ Urbanization
lm_urban <- lm(Fertility ~ Urbanization, data = swiss)
summary(lm_urban)

plot(X1, Y,
     xlab = "Urbanization", ylab = "Fertility",
     main = "Fertility vs Urbanization",
     col = "skyblue", pch = 16)
abline(lm_urban, col = "red")

plot(fitted(lm_urban), resid(lm_urban),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Urbanization",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Simple Regression 2: Fertility ~ Education
lm_edu <- lm(Fertility ~ Education, data = swiss)
summary(lm_edu)

plot(X2, Y,
     xlab = "Education", ylab = "Fertility",
     main = "Fertility vs Education",
     col = "skyblue", pch = 16)
abline(lm_edu, col = "red")

plot(fitted(lm_edu), resid(lm_edu),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Education",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Simple Regression 3: Fertility ~ Examination
lm_exam <- lm(Fertility ~ Examination, data = swiss)
summary(lm_exam)

plot(X3, Y,
     xlab = "Examination", ylab = "Fertility",
     main = "Fertility vs Examination",
     col = "skyblue", pch = 16)
abline(lm_exam, col = "red")

plot(fitted(lm_exam), resid(lm_exam),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Examination",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Simple Regression 4: Fertility ~ Catholic
lm_cat <- lm(Fertility ~ Catholic, data = swiss)
summary(lm_cat)

plot(X4, Y,
     xlab = "Catholic", ylab = "Fertility",
     main = "Fertility vs Catholic",
     col = "skyblue", pch = 16)
abline(lm_cat, col = "red")

plot(fitted(lm_cat), resid(lm_cat),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Catholic",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Simple Regression 5: Fertility ~ Infant Mortality
lm_mort <- lm(Fertility ~ Infant.Mortality, data = swiss)
summary(lm_mort)

plot(X5, Y,
     xlab = "Infant Mortality", ylab = "Fertility",
     main = "Fertility vs Infant Mortality",
     col = "skyblue", pch = 16)
abline(lm_mort, col = "red")

plot(fitted(lm_mort), resid(lm_mort),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Infant Mortality",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

## Multivariate Models

# Model 1: Urbanization + Education
lm_urb_edu <- lm(Fertility ~ Urbanization + Education, data = swiss)
summary(lm_urb_edu)

plot(fitted(lm_urb_edu), resid(lm_urb_edu),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Urb + Edu",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 2: Urbanization + Education + Examination
lm_urb_edu_exam <- lm(Fertility ~ Urbanization + Education + Examination, data = swiss)
summary(lm_urb_edu_exam)

plot(fitted(lm_urb_edu_exam), resid(lm_urb_edu_exam),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: + Examination",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 3: Urbanization + Education + Examination + Catholic
lm_model4 <- lm(Fertility ~ Urbanization + Education + Examination + Catholic, data = swiss)
summary(lm_model4)

plot(fitted(lm_model4), resid(lm_model4),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: + Catholic",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 4: Full Model (Urbanization + Education + Examination + Catholic + Infant Mortality)
lm_model5 <- lm(Fertility ~ Urbanization + Education + Examination + Catholic + Infant.Mortality, data = swiss)
summary(lm_model5)

plot(fitted(lm_model5), resid(lm_model5),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted: Full Model",
     col = "skyblue", pch = 16)
abline(h = 0, col = "red", lty = 2)

## Q-Q Plots: Normality Check of Residuals

par(mfrow = c(3, 3))

qqnorm(resid(lm_urban), main = "Q-Q: Urbanization", col = "skyblue", pch = 16); qqline(resid(lm_urban), col = "red")
qqnorm(resid(lm_edu), main = "Q-Q: Education", col = "skyblue", pch = 16); qqline(resid(lm_edu), col = "red")
qqnorm(resid(lm_exam), main = "Q-Q: Examination", col = "skyblue", pch = 16); qqline(resid(lm_exam), col = "red")
qqnorm(resid(lm_cat), main = "Q-Q: Catholic", col = "skyblue", pch = 16); qqline(resid(lm_cat), col = "red")
qqnorm(resid(lm_mort), main = "Q-Q: Infant Mortality", col = "skyblue", pch = 16); qqline(resid(lm_mort), col = "red")

qqnorm(resid(lm_urb_edu), main = "Q-Q: Urb + Edu", col = "skyblue", pch = 16); qqline(resid(lm_urb_edu), col = "red")
qqnorm(resid(lm_urb_edu_exam), main = "Q-Q: + Examination", col = "skyblue", pch = 16); qqline(resid(lm_urb_edu_exam), col = "red")
qqnorm(resid(lm_model4), main = "Q-Q: + Catholic", col = "skyblue", pch = 16); qqline(resid(lm_model4), col = "red")
qqnorm(resid(lm_model5), main = "Q-Q: Full Model", col = "skyblue", pch = 16); qqline(resid(lm_model5), col = "red")




