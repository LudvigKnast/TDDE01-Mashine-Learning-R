# Assignment 2 Linear Regression and Ridge regression.

### Task 1
df = read.csv("parkinsons.csv")

## Scaling the data using mean and standard deviation.
sdf = df

sdf[2] = (sdf[2]-mean(sdf[[2]]))/sd(sdf[[2]])
for (i in 4:22)
{
  sdf[i] = (sdf[[i]]-mean(sdf[[i]]))/sd(sdf[[i]])
}

# Splitting data to training and test data
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
train=sdf[id,]
test=sdf[-id,]

# Normal distribution for motor_UPDRS


### Task 2
# Can't access Jitter... values. !?!?!?
fit = lm(formula=motor_UPDRS~Shimmer+NHR+RPDE+DFA+PPE, data=sdf, subset=train)
summary(fit)
fitting = fitted(fit)
predicted <- predict(fit, test, interval="confidence")
predict.lm
coefficients(fit)[2:6] -> coeff
# Could it only be to get these values??

attach(test)
plot(subject., motor_UPDRS)
lines(subject., predicted[, "fit"])

lines(subject., predicted[, "lwr"], lty = "dotted",col="blue")
lines(subject., predicted[, "upr"], lty = "dotted",col="blue")
detach(test)



### Task 3
# Loglikelihood, according to 3.20 in MLFC (course book)
loglikelihood <- function(theta, sigma, X, Y)
{
  n = dim(X)[0]
  summation = 0
  for (i in 1:n)
  {
    summation = summation + (theta*X[i]-Y[i])^2
  }
  
  res = -n/2*ln(2*pi*sigma^2) - 1/sqrt(2*sigma^2) * summation
  return(res)
}

# Ridge function, using norm(type=2) to get svd / 2-norm.
ridge <- function(lambda, theta, sigma, X, Y)
{
  return (lambda*norm(theta, type="2") - loglikelihood(theta, sigma, x, y))
}

# RidgeOpt function, 
ridgeopt <- function(lambda)
{
  optim(par=lambda, fn=ridge, theta, sigma, X, Y, method="BFGS") #??
}

# Degrees of freedom function
DF <- function(lambda)
{
  return(df.residuals(...))
}


### Task 4
# Use RidgeOpt
lambdas = c(1, 100, 1000)


# compare DF for values

