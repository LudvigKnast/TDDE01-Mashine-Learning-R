# Load data and init
df = read.csv("pima-indians-diabetes.csv", header=FALSE)

##############################################
# ----------------- Task 1 ----------------- #
##############################################

# Split data into diabetes or no diabetes
diabetes = df[df$V9 == 1, ]
no_diabetes = df[df$V9 == 0,]

# Scatter plot of age (x-axis) and plasma glucose concentration (y-axis)
plot(x = diabetes$V8, 
     y = diabetes$V2, 
     type = "p", 
     xlab = "Age [years]", 
     ylab = "Plasma glucose concentration", 
     xlim = c(min(df$V8), max(df$V8)),
     ylim = c(min(df$V2), max(df$V2)),
     col = "red",
     )
points(x = no_diabetes$V8, y = no_diabetes$V2, col = "blue")
legend(x="bottomright", 
       legend = c("diabetes","no diabetes"),
       col=c("red", "blue"), 
       pch = "oo")

# Answer: Logistic regression probably won't work very well since the data is 
# too "mixed" to be separated so easily.


##############################################
# ----------------- Task 2 ----------------- #
##############################################

# Data and parameters
r = 0.5
y = df$V9
n = length(y)
x1 = df$V2
x2 = df$V8
X = data.frame(x1 = x1, x2 = x2)

# Fit model
glm_logistic = glm(formula = y ~ x1 + x2, family = "binomial")

# Make predictions
y_hat = as.numeric(predict(object = glm_logistic, type = "response") > r)

# Create confusion matrix 
conf_matrix = table(y_hat, y)

# Misclassification rate
miss_rate = (conf_matrix[1, 2] + conf_matrix[2, 1])/n

# Scatter plot of predictions
plot(x = x2[y_hat == 1], 
     y = x1[y_hat == 1], 
     type = "p", 
     xlab = "Age [years]", 
     xlim = c(min(df$V8), max(df$V8)),
     ylim = c(min(df$V2), max(df$V2)),
     ylab = "Plasma glucose concentration",
     main = "Predictions for r = 0.5",
     col = "red",
)
points(x = x2[y_hat == 0], y = x1[y_hat == 0], col = "blue")
legend(x="bottomright", 
       legend = c("predicted diabetes","no predicted diabetes"),
       col=c("red", "blue"), 
       pch = "oo")

# Answer: The training misclassification error rate is 0.2630208. This is not 
# great, but it is not horrible either. 
# The probabilistic equation is p(y=1|x, theta) = g(x) and 
# p(y=0|x, theta) = 1-g(x) where 
# g(x) = 1/(1 + exp(-(-5.91245 + 0.03564*x1 + 0.02478*x2)))


##############################################
# ----------------- Task 3 ----------------- #
##############################################

# a) 

# Answer: The decision boundary between the two classes is given by 
# x1 = (5.91245 - ln(1/r - 1) - 0.02478*x2)/0.03564

# b) 
# Plot decision boundary
boundary = (5.91245 - log(1/r - 1) - 0.02478*x2)/0.03564
x2_sorted = sort(x = x2, index.return = TRUE)
boundary_ordered = boundary[x2_sorted$ix]
lines(x = x2_sorted$x, y = boundary_ordered, col = "black")

# Answer: The boundary fits perfectly for the scatterplot of the predicted 
# values, to no surprise.


##############################################
# ----------------- Task 4 ----------------- #
##############################################

# Set r 
r = 0.2

# Make predictions
y_hat = as.numeric(predict(object = glm_logistic, type = "response") > r)

# Create confusion matrix 
conf_matrix = table(y_hat, y)

# Misclassification rate
miss_rate = (conf_matrix[1, 2] + conf_matrix[2, 1])/n

# Scatter plot of predictions
plot(x = x2[y_hat == 1], 
     y = x1[y_hat == 1], 
     type = "p", 
     xlab = "Age [years]", 
     xlim = c(min(df$V8), max(df$V8)),
     ylim = c(min(df$V2), max(df$V2)),
     ylab = "Plasma glucose concentration",
     main = "Predictions for r = 0.2",
     col = "red",
)
points(x = x2[y_hat == 0], y = x1[y_hat == 0], col = "blue")
legend(x="bottomright", 
       legend = c("predicted diabetes","no predicted diabetes"),
       col=c("red", "blue"), 
       pch = "oo")

# Plot decision boundary
boundary = (5.91245 - log(1/r - 1) - 0.02478*x2)/0.03564
x2_sorted = sort(x = x2, index.return = TRUE)
boundary_ordered = boundary[x2_sorted$ix]
lines(x = x2_sorted$x, y = boundary_ordered, col = "black")


# Set r 
r = 0.8

# Make predictions
y_hat = as.numeric(predict(object = glm_logistic, type = "response") > r)

# Create confusion matrix 
conf_matrix = table(y_hat, y)

# Misclassification rate
miss_rate = (conf_matrix[1, 2] + conf_matrix[2, 1])/n

# Scatter plot of predictions
plot(x = x2[y_hat == 1], 
     y = x1[y_hat == 1], 
     type = "p", 
     xlab = "Age [years]", 
     xlim = c(min(df$V8), max(df$V8)),
     ylim = c(min(df$V2), max(df$V2)),
     ylab = "Plasma glucose concentration",
     main = "Predictions for r = 0.8",
     col = "red",
)
points(x = x2[y_hat == 0], y = x1[y_hat == 0], col = "blue")
legend(x="bottomright", 
       legend = c("predicted diabetes","no predicted diabetes"),
       col=c("red", "blue"), 
       pch = "oo")

# Plot decision boundary
boundary = (5.91245 - log(1/r - 1) - 0.02478*x2)/0.03564
x2_sorted = sort(x = x2, index.return = TRUE)
boundary_ordered = boundary[x2_sorted$ix]
lines(x = x2_sorted$x, y = boundary_ordered, col = "black")

# Answer: As the r value changes so does the predictions. If the r value is 
# lower, the amount of diabetes predictions increases and vice versa.


##############################################
# ----------------- Task 5 ----------------- #
##############################################

# Create new features
z1 = x1^4
z2 = x1^3 * x2
z3 = x1^2 * x2^2
z4 = x1 * x2^3
z5 = x2^4

# Set r 
r = 0.5

# Fit model
glm_logistic = glm(formula = y ~ z1 + z2 + z3 + z4 +z5 + x1 + x2, 
                   family = "binomial")

# Make predictions
y_hat = as.numeric(predict(object = glm_logistic, type = "response") > r)

# Create confusion matrix 
conf_matrix = table(y_hat, y)

# Misclassification rate
miss_rate = (conf_matrix[1, 2] + conf_matrix[2, 1])/n

# Scatter plot of predictions
plot(x = x2[y_hat == 1], 
     y = x1[y_hat == 1], 
     type = "p", 
     xlab = "Age [years]", 
     xlim = c(min(df$V8), max(df$V8)),
     ylim = c(min(df$V2), max(df$V2)),
     ylab = "Plasma glucose concentration",
     main = "Predictions for r = 0.5",
     col = "red",
)
points(x = x2[y_hat == 0], y = x1[y_hat == 0], col = "blue")
legend(x="bottomright", 
       legend = c("predicted diabetes","no predicted diabetes"),
       col=c("red", "blue"), 
       pch = "oo")


# Answer: The misclassification rate is now ~0.2448, which is better than 
# before. The decision boundary is now curved instead of a straight line. 
# The new decision boundary hasn't really made the model better. 



