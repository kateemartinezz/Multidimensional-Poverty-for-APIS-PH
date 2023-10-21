install.packages("readxl")
library(readxl)

df <- (MPI_and_Independent)

rm(predictor_vars)
# Extract columns 2 to 5 (regompi, livbirth, wagerate, enroll) as predictor variables
predictor_vars <- df[, 3:7]

# Extract column 6 as the response variable
response_var <- df$`Regional MPI`

# Perform Principal Component Analysis
pca_result <- prcomp(predictor_vars, scale = TRUE)

# Extract the principal components
principal_components <- pca_result$x

PCAW <- cbind(response_var, principal_components)


################################

# Extract the response variable from column 1
response_var2 <- PCAW[, 1]

# Extract the independent variables from columns 2 to 6
independent_vars <- PCAW[, 2:6]

# Fit the regression model
reg_model <- lm(response_var2 ~ ., data = data.frame(response_var2, independent_vars))

# Get the regression coefficients
reg_coeffs <- coef(reg_model)

# Print the regression coefficients
print(reg_coeffs)


summary(reg_model)



######################################
install.packages("pls")
install.packages("psych")
library(psych)

pc.fit <- prcomp(predictor_vars, scale = TRUE)
summary(pc.fit)

trans_test <- predict(pc.fit)

trans_test <- cbind(response_var2, trans_test)

components <- trans_test[, 2:6]

pcr_lmodel <- lm(response_var2 ~ ., data = data.frame(response_var2, components))

summary(pcr_lmodel)
