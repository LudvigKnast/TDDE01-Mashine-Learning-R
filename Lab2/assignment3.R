# Read data
data = read.csv("communities.csv")

# Load ggplot2
library(ggplot2)

############################################## 
# ----------------- Task 1 ----------------- #
##############################################

# Scale and center all data (except for ViolentCrimesPerPop)
X = data[-data$ViolentCrimesPerPop]
X = scale(x = X,center = TRUE, scale = TRUE)

PCA <- function(X) {
  # Calculate covariance matrix
  X = as.matrix(X)
  n = nrow(X)
  S = t(X)%*%X/n 
  eigen = eigen(S)
  return(eigen)
}

# Get principal components and corresponding eigenvalues 
# NOTE: Eigenvalues are sorted in descending order already
eigen = PCA(X)


# Get variance captured by each PC
variance = eigen$values/sum(eigen$values)*100
sprintf("%2.3f", variance)

# Print cumulative sum
print(cumsum(variance) >= 95)



############################################## 
# ----------------- Task 2 ----------------- #
##############################################

# PCA using princomp(...)
res = princomp(X, scores = TRUE)

# Loadings plot for first PC
U = res$loadings
plot(U[,1], ylab = "Loading PC1", main="Loadings plot, PC1")

# Get the 5 features that contribute the most, by absolute value
cont_most = sort(abs(U[,1]), decreasing = TRUE)
print(cont_most[1:5])

# Plot the score using PC1 and PC2 as coordinates and show target as color
scores = as.data.frame(res$scores)
scores$ViolentCrimesPerPop = data$ViolentCrimesPerPop
ggplot(data = scores, aes(x = Comp.1, y = Comp.2, color = ViolentCrimesPerPop)) + 
  geom_point() + 
  scale_color_gradient2(midpoint=0.5, low="blue", mid="green", high="red")



############################################## 
# ----------------- Task 3 ----------------- #
##############################################

# Scale all of the original data
scaled_data = as.data.frame(scale(data, center = TRUE, scale = TRUE))

# Split data into train and test
n = dim(scaled_data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = scaled_data[id,]
test = scaled_data[-id,]

# Fit linear regression model
fit = lm(formula=ViolentCrimesPerPop~., data=train)

# Get summary of model
summary(fit)

# Get predictions on training data
y_hat_train = fitted(fit)

# Training MSE error
n_train <- nrow(train)
mse_train <- sum((y_hat_train-train$ViolentCrimesPerPop)^2)/n_train

# Get predictions on test data
y_hat_test <- predict(fit, test)

# Test MSE error
n_test <- nrow(test)
mse_test <- sum((y_hat_test-test$ViolentCrimesPerPop)^2)/n_test

cat("Training MSE = ", mse_train, " and test MSE = ", mse_test)


############################################## 
# ----------------- Task 4 ----------------- #
##############################################

# MSE estimate function
mse_est <- function(theta, X, y) {
  mse = mean((y-X%*%theta)^2)
  return(mse)
}

# Split train and test into features and targets
X_train = as.matrix(subset(train, select = -ViolentCrimesPerPop))
y_train = as.matrix(train$ViolentCrimesPerPop)
X_test = as.matrix(subset(test, select = -ViolentCrimesPerPop))
y_test = as.matrix(test$ViolentCrimesPerPop)

# Create structure to store MSE and iteration number
mse_train = c()
mse_test = c()
k = 0

# Linear regression cost function to use for optimization
lin_cost <- function(theta, X_train, y_train, X_test, y_test) {
  n = nrow(y_train)
  cost = sum((X_train%*%theta - y_train)^2)/n
  .GlobalEnv$k= .GlobalEnv$k+1
  .GlobalEnv$mse_train[k] = mse_est(theta, X_train, y_train)
  .GlobalEnv$mse_test[k] = mse_est(theta, X_test, y_test)
  return(cost)
}

# Initialize theta to zero vector
init_theta = matrix(0, ncol(X_train), 1)

# Optimize theta
optimRes <- optim(par=init_theta, 
                  fn=lin_cost, 
                  gr=NULL, 
                  X_train, 
                  y_train, 
                  X_test,
                  y_test,
                  method="BFGS")


# Throw away first N iterations and plot
N = 500
plot((N+1):k, mse_train[(N+1):k],  
     col = "red", 
     type = 'l', 
     xlab = "Iteration",
     ylab = "Estimated MSE",
     main = "Estimated train and test MSE",
     xlim = c(N+1, k),
     ylim =  c(0, 1))
lines((N+1):k, mse_test[(N+1):k], type = "l", col = "blue")
legend(x="topright", 
       legend = c("Training data","Test data"),
       col=c("red", "blue"),
       pch = "--")

cat("Minimal test error in iteration number:", which.min(mse_test), "\n")
cat("Train error for iteration number", which.min(mse_test), "was: ", 
    mse_train[which.min(mse_test)], "\n")
cat("Test error for iteration number", which.min(mse_test), "was: ", 
    mse_test[which.min(mse_test)], "\n")










