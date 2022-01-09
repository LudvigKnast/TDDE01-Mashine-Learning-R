library(glmnet)

df = read.csv("tecator.csv")

# Splitting data to training and test data
n = dim(df)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train=df[id,]
test=df[-id,]


############################################## 
##----------------- Task 1 -----------------##
##############################################

# Fit using all listed voice characteristics on training data
fit = lm(formula=Fat ~. -Sample-Protein-Moisture, data=train)

# Show summary of fitted model
summary(fit)

# Print coefficients of the model
coeff <- coefficients(fit)
print(coeff)

# Get predictions on training data
y_hat_train = fitted(fit)

# Estimate training MSE
n_train <- nrow(train)
mse_train <- sum((y_hat_train-train$Fat)^2)/n_train
cat("MSE training data:", mse_train, "\n")

# Get predictions on test data
y_hat_test <- predict(fit, test)

# Estimate test MSE
n_test <- nrow(test)
mse_test <- sum((y_hat_test-test$Fat)^2)/n_test
cat("MSE test data:", mse_test, "\n")


##############################################
##----------------- Task 2 -----------------##
##############################################

# take loss function from slide


##############################################
##----------------- Task 3 -----------------##
##############################################

covariates = train[,2:101]
y_train = train$Fat

model_Las=glmnet(as.matrix(covariates), y_train, alpha=1, family="gaussian")
plot(model_Las, xvar="lambda", label=TRUE)


##############################################
##----------------- Task 4 -----------------##
##############################################

model_Reg=glmnet(as.matrix(covariates), y_train, alpha=0, family="gaussian")
plot(model_Reg, xvar="lambda", label=TRUE)


##############################################
##----------------- Task 5 -----------------##
##############################################

model_Las_cv=cv.glmnet(as.matrix(covariates), y_train, alpha=1, family="gaussian")
plot(model_Las_cv)
print(coef(model_Las_cv, s="lambda.min"))
lambda_opt <- model_Las_cv$lambda.min
cat("lambda min:", lambda_opt)

model_opt=glmnet(as.matrix(covariates), 
                 y_train, alpha=1, 
                 family="gaussian", 
                 lambda=lambda_opt)
test_predict=predict(model_opt, newx=as.matrix(test[,2:101]), type="response")
plot(test$Fat,
     xlab = "Sample",
     ylab = "Level of Fat",
     col ="red",
     ylim = c(0, 75),
     xlim = c(0, 110),
)
points(test_predict, col = "blue")
legend(x="topright",
       legend = c("Truth","Prediction"),
       col=c("red", "blue"),
       pch = "oo")


plot(test$Fat, test_predict,
     xlab = "Actual fat level",
     ylab = "Predicted fat level"
)