#PCA MODELLING
cormatrix <- cor(Cp_c[c("wheelbase","carwidth","curbweight","enginesize","boreratio","horsepower","citympg","highwaympg")])
cormatrix

fmv2e <- lm(price ~ wheelbase+ carwidth +curbweight + enginesize+horsepower+ citympg+highwaympg, data=Cp_c)
vif(fmv2e)

# correlated variables
correlated_vars <- Cp_c[, c("carwidth","curbweight","enginesize","horsepower","citympg","highwaympg")]
scaled_vars <- scale(correlated_vars)
pca_result <- prcomp(scaled_vars, center = TRUE, scale. = TRUE)
# proportion of variance explained by each component
summary(pca_result)

# Access PCA loadings and scores
pca_loadings <- pca_result$rotation
pca_scores <- pca_result$x  # Principal component scores

#X1+ X5 + X9+ X10+ X17 + X16 + X5X7 + X4 + X13 + X18 + X14 + X7 + X3 + X1X8 + X10X9 + X15 + X1X6 + X8X9 + X6 + X10X8 + X8 
# Add PCA scores to the dataset
Cp_c$pca3 <- pca_scores[, 1]
Cp_c$pca4 <- pca_scores[, 2]
Cp_c$pca5 <- pca_scores[, 3]
Cp_c$pca7 <- pca_scores[, 4]
Cp_c$pca8 <- pca_scores[, 5]
Cp_c$pca9 <- pca_scores[, 6]

#X1c+ X5c + X9c+ X10c+ X17c + X16c + X5X7c + X4c + X13c + X18c + X14c + X7c + X3c + X1X8c + X10X9c + X15c + X1X6c + X6c + X8c+ X10X8c
# Build the regression model using PCA components
modelpc <- lm(log(Yc) ~ X1c+ pca5+pca9+X10c+ X17c+ X16c+ (pca5*pca7)+pca4+ X13c+ X18c+ X14c+ pca7+ pca3+
              (X1c*pca8)+ (X10c*pca9) + X15c + (X1c*X6c) + X6c+ pca8+ (X10c*pca8), data = Cp_c)
summary(modelpc) #R2=95,94
anova(modelpc)
 