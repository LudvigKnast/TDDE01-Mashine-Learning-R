# Load libraries and create data
library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))

############################################## 
# ----------------- Task 1 ----------------- #
##############################################
# Split into train and test data sets
train <- mydata[1:25,] 
test <- mydata[26:500,]

# Random initialization of the weights in the interval [-1, 1], we have 31 weights
winit <- runif(min = -1, max = 1, n = 31) 
nn <- neuralnet(formula = Sin ~ Var, 
                hidden = 10, 
                startweights = winit, 
                data = mydata)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1],predict(nn,test), col="red", cex=1)
legend(x="bottomleft", 
       legend = c("train data","test data", "predictions"),
       col=c("black", "blue", "red"), 
       pch = "Ooo")

# Plot neural network (might crash...)
plot(nn)

# Answer: We get really good results.

############################################## 
# ----------------- Task 2 ----------------- #
##############################################
# Custom activation functions
linear <- function(x) x
relu <- function(x) max(0, x)
softplus <- function(x) log(1 + exp(x))


# ----- Linear activation ---- #
# Random initialization of the weights in the interval [-1, 1], we have 31 weights
winit <- runif(min = -1, max = 1, n = 31) 
nn_linear <- neuralnet(formula = Sin ~ Var, 
                       hidden = 10, 
                       startweights = winit, 
                       data = mydata, 
                       act.fct = linear)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1], predict(nn_linear,test), col="red", cex=1)
legend(x="bottomleft", 
       legend = c("train data","test data", "predictions"),
       col=c("black", "blue", "red"), 
       pch = "Ooo")

# ----- Softplus activation ---- #
nn_softplus <- neuralnet(formula = Sin ~ Var, 
                         hidden = 10, 
                         startweights = winit, 
                         data = mydata, 
                         act.fct = softplus, 
                         threshold = 0.2)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)
points(test[,1], predict(nn_softplus, test), col="red", cex=1)
legend(x="bottomleft", 
       legend = c("train data","test data", "predictions"),
       col=c("black", "blue", "red"), 
       pch = "Ooo")


# ---- ReLU activation ---- #
# ReLU doesn't work since it's not differentiable at 0.

# Answer: The linear activation function gives a straight line of predictions,
# since we don't introduce any non-linearity. The softplus works quite well, but
# required a higher threshold in order to converge. The relu doesn't work, since
# it can't be derived at 0.

############################################## 
# ----------------- Task 3 ----------------- #
##############################################
# Create data
set.seed(1234567890)
Var <- runif(500, 0, 50)
new_data <- data.frame(Var, Sin=sin(Var))

# Plot of the true data (blue), and predictions (red)
plot(new_data[,1],predict(nn,new_data), col="red", cex=1, ylab = "Sin", xlab = "Var")
points(new_data, col = "blue", cex=1)
legend(x="bottomleft", 
       legend = c("true data", "predictions"),
       col=c("red", "blue"), 
       pch = "oo")

# Answer: The predictions are good until we try to predict sin(x) for x greater
# than ~10. For x > 10 the predictions get worse and worse.

############################################## 
# ----------------- Task 4 ----------------- #
##############################################
# Look at some (large) x value (1 is the bias)
x <- c(1, 100)
print(x)

# Print the hidden units
h <- c(1, 1/(1+exp(-x %*% nn$weights[[1]][[1]])))
print(h)

# Print the prediction
y_hat <- h %*% nn$weights[[1]][[2]]
print(y_hat)

# Print the weights
print(nn$weights)

# Answer: The predictions seem to converge to approximately -3.73.
# The reason for this is that as the values of x grow larger 
# some of the hidden units seem to go towards one, and the rest to zero.
# This will cause the output to be approximately the sum of the weights 
# corresponding to the non-zero hidden units. And if we check, we can see that 
# sum of those weights are approximately -3.73. 
# Weights 1, 2, 3, 4, 7 and 9 are included in this summation.
# The reason that some hidden units go to one and some to zero is that the
# if the weight for x in the first layer is > 0 then as x is large the hidden 
# unit will go towards one. Else if it is < 0 then it will go towards 0.
# This is due to what happens to exp(-x*w) in the sigmoid function.


############################################## 
# ----------------- Task 5 ----------------- #
##############################################
# Choose all data as training data
train <- mydata

# Random initialization of the weights in the interval [-1, 1], we have 31 weights
winit <- runif(min = -1, max = 1, n = 31) 
nn <- neuralnet(formula = Var ~ Sin, 
                hidden = 10, 
                startweights = winit, 
                data = mydata, 
                threshold = 0.1)

# Plot of training data (blue), and predictions (red)
plot(train[,2], train[,1], col = "blue", cex=1, xlab = "Sin", ylab = "Var")
points(train[,2],predict(nn,train), col="red", cex=1)
legend(x="bottomleft", 
       legend = c("true data", "predictions"),
       col=c("blue", "red"), 
       pch = "oo")

# Answer: We get bad results. Not so surprising, as e.g sin(x)=0 has 4 different
# x values that should be predicted, but it's impossible to know given a value 
# of sin, which of these values should be returned. Our network must predict the
# same value for the same input.



