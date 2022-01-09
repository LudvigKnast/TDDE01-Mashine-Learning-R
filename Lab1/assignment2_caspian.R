
##############################################
# ----------------- Task 1 ----------------- #
##############################################

df = read.csv("parkinsons.csv")

## Scaling the data using mean and standard deviation.
sdf = df

sdf[2] = (sdf[2]-mean(sdf[[2]]))/sd(sdf[[2]])
for (i in 4:22)
{
  sdf[i] = (sdf[[i]]-mean(sdf[[i]]))/sd(sdf[[i]])
}

# Splitting data to training and test data
n = dim(sdf)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
train=sdf[id,]
test=sdf[-id,]


##############################################
# ----------------- Task 2 ----------------- #
##############################################
# Fit using all listed voice characteristics on training data
fit = lm(formula=motor_UPDRS~0+.-subject.-age-sex-test_time-total_UPDRS, data=train)

# Show summary of fitted model
summary(fit)

# Print coefficients of the model
coeff <- coefficients(fit)
print(coeff)

# Get predictions on training data
y_hat_train = fitted(fit)

# Estimate training MSE
n_train <- nrow(train)
mse_train <- sum((y_hat_train-train$motor_UPDRS)^2)/n_train

# Get predictions on test data
y_hat_test <- predict(fit, test)

# Estimate test MSE
n_test <- nrow(test)
mse_test <- sum((y_hat_test-test$motor_UPDRS)^2)/n_test

# Answer: The variables with asterisks (small p-value) and large coefficients
# in the summary contribute significantly



##############################################
# ----------------- Task 3 ----------------- #
##############################################

# Loglikelihood, according to 3.20 in MLFC (course book)
loglikelihood <- function(theta, sigma, X, Y)
{
  n = length(Y)
  res = -(n*log(2*pi*sigma^2)/2) - sum((X%*%theta-Y)^2)/(2*sigma^2) 
  return(res)
}

# Ridge function, using norm(type=2) to get svd / 2-norm.
ridge <- function(params, lambda, X, Y)
{
  theta <- params[1:ncol(X)]
  sigma <- params[ncol(X)+1]
  return (lambda*norm(theta, type="2") - loglikelihood(theta, sigma, X, Y))
}

# RidgeOpt function, 
ridgeopt <- function(lambda, initTheta, initSigma, X, Y)
{
  
  optimRes <- optim(par=c(initTheta, initSigma), 
                    fn=ridge, 
                    gr=NULL, 
                    lambda, 
                    X, 
                    Y, 
                    method="BFGS")
  return(optimRes)
}

# Degrees of freedom function
DF <- function(lambda, X)
{
  P = X %*% solve((t(X)%*%X + lambda*diag(ncol(X)))) %*% t(X)
  return(sum(diag(P)))
}


##############################################
# ----------------- Task 4 ----------------- #
##############################################

# Setting lambda to 0 gives us almost identical results as the linear 
# regression, as it should. So most likely correct solution.

# Use RidgeOpt
lambdas = c(1, 100, 1000)
initTheta <- matrix(0, length(coeff), 1)
initSigma <- 1
X_train <- as.matrix(train[, 7:ncol(train)])
Y_train <- as.matrix(train$motor_UPDRS)
X_test <- as.matrix(test[, 7:ncol(test)])
Y_test <- as.matrix(test$motor_UPDRS)

# Init empty result data frame
res <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(res) <- c("mse_train", 
                   "mse_test", 
                   "theta_hat", 
                   "sigma_hat", 
                   "dof")


for(lambda in lambdas) {
  # Find optimal theta and sigma using RidgeOpt
  ridgeRes <- ridgeopt(lambda, initTheta, initSigma, X_train, Y_train)
  
  # Extract theta and sigma
  theta_hat <- ridgeRes$par[1:length(initTheta)]
  sigma_hat <- ridgeRes$par[length(initTheta)+1]
  
  # Predict on training data
  y_hat_train <- X_train%*%theta_hat
  
  # Compute estimated training MSE
  n_train <- nrow(X_train)
  mse_train <- sum((y_hat_train-Y_train)^2)/n_train
  
  # Predict on test data
  y_hat_test <- X_test%*%theta_hat
  
  # Compute estimated test MSE
  n_test <- nrow(X_test)
  mse_test <- sum((y_hat_test-Y_test)^2)/n_test
  
  # Calculate dof 
  dof <- DF(lambda, X_train)
  
  # Append results
  row <- list(mse_train, mse_test, list(theta_hat), sigma_hat, dof)
  res[nrow(res)+1,] <- row
  
}

# Print result and set row names
rownames(res) <- lambdas
print(res)


# Lambda = 100 or 1000 seem most appropriate, they reduce the degrees of 
# freedom down from 16, which makes sense since in the linear regression
# some coefficients were not contributing so much, i.e they could probably be
# removed in a general model.

