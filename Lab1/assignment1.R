library(kknn)
# Read data file to data frame
df = read.csv("optdigits.csv", header=FALSE)


############################################## 
##----------------- Task 1 -----------------##
##############################################

# Divide data into training/validation/test sets.
n = dim(df)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = df[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
valid = df[id2,]

id3 = setdiff(id1, id2)
test = df[id3,]


############################################## 
##----------------- Task 2 -----------------##
##############################################

# Confusion matrix and misclassification error for test data

# Fitting with kknn
df.kknn = kknn(formula = as.factor(train$V65) ~ ., train = train, test = test, 
               kernel = "rectangular", k = 30)
df.kknn.fit <- fitted(df.kknn)

#Confusion matrix
conf_matrix.test <- table(df.kknn.fit, test$V65)
print(conf_matrix.test)

# Missclassification rate
miss_rate <- 1-sum(diag(conf_matrix.test)/sum(conf_matrix.test))
cat("Miss rate for (K=30, test data) = ", miss_rate, "\n")

#--------------------------------------------#

# Confusion matrix and misclassification error for training data

# Fitting with kknn
df.kknn = kknn(formula = as.factor(train$V65) ~ ., train = train, test = train,
               kernel = "rectangular", k = 30)
df.kknn.fit <- fitted(df.kknn)

# Confusion matrix
conf_matrix.train <- table(df.kknn.fit, train$V65)
print(conf_matrix.train)

# Misclassification rate
miss_rate <- 1-sum(diag(conf_matrix.train)/sum(conf_matrix.train))
cat("Miss rate for (K=30, training data) = ", miss_rate, "\n")



##############################################
##----------------- Task 3 -----------------##
##############################################

# Creating data frame "summary" for test cases with 8 and to see
# prob from the model for those cases, then determine the
# 3 hardest and 2 easiest cases
truth <- train$V65
prob_8 <- predict(df.kknn, train, type = "prob")[,9]
summary <- data.frame(truth, prob_8)
summary <- subset(summary, truth == 8)
summary <- summary[order(summary$prob_8),]
hard.8 <- c(strtoi(row.names(summary[1,])))
hard.8 <- c(hard.8, strtoi(row.names(summary[2,])))
hard.8 <- c(hard.8, strtoi(row.names(summary[3,])))
easy.8 <- c(strtoi(row.names(summary[nrow(summary),])))
easy.8 <- c(easy.8, strtoi(row.names(summary[nrow(summary) -1,])))

cat("Easiest 8s to predict:", easy.8, "\n")
cat("Hardest 8s to predict:", hard.8, "\n")

# Easiest 8s to predict: 1864 1811
# Hardest 8s to predict: 1624 1663 229

# Visualize given digit (x)
rotate <- function(mat){
  return(t(apply(mat,2, rev)))
}
x <- 229 # change x to visualize wanted entry
hmMatrix = matrix(as.numeric(train[x,0:-1]), ncol = 8, nrow = 8)
hmMatrix = rotate(rotate(rotate(hmMatrix)))
hm <- heatmap(hmMatrix, Colv = NA, Rowv = NA)



##############################################
##----------------- Task 4 -----------------##
##############################################

# Misclass function
missclass=function(X,Y)
{
  return(1-sum(diag(table(X,Y)))/length(X))
}

# Collecting misclassification and cross entropy for each K (1..30)
K.missclass.valid = c()
K.missclass.train = c()
K.optimal = c(0,1,1) # (K, miss_rate.valid, miss_rate.train)

for(KVal in 1:30) {
  df.kknn <- kknn(formula = as.factor(V65) ~ ., train=train,
                  test=valid, kernel="rectangular", k = KVal)
  df.kknn.fit <- fitted(df.kknn)
  miss_rate.valid <- missclass(df.kknn.fit, valid$V65)
  K.missclass.valid <- c(K.missclass.valid, miss_rate.valid)

  df.kknn <- kknn(formula = as.factor(V65) ~ ., train=train,
                  test=train, kernel="rectangular", k = KVal)
  df.kknn.fit <- fitted(df.kknn)
  miss_rate.train <- missclass(df.kknn.fit, train$V65)
  K.missclass.train <- c(K.missclass.train, miss_rate.train)

  if (miss_rate.valid < K.optimal[2]) {
    K.optimal <- c(KVal, miss_rate.valid, miss_rate.train)
  }

}
plot(K.missclass.valid,
     xlab = "K",
     ylab = "Miss rate",
     col ="red",
     ylim = c(0,max(K.missclass.valid)),
     )
points(K.missclass.train, col = "blue")
legend(x="bottomright",
       legend = c("Validation data","Training data"),
       col=c("red", "blue"),
       pch = "oo")

# Testing optimal K from task 4
df.kknn = kknn(formula = as.factor(V65) ~ ., train = train, test = test,
               kernel = "rectangular", k = K.optimal[1])
df.kknn.fit <- fitted(df.kknn)
miss_rate <- missclass(df.kknn.fit, test$V65)
cat("For the optimal K from task 4 the following results can be retrieved:",
    "\nOptimal K =", K.optimal[1], "\nMiss rate with training data:",
    K.optimal[3],"\nMiss rate with validation data:",K.optimal[2],
    "\nMiss rate with test data:", miss_rate)



##############################################
##----------------- Task 5 -----------------##
##############################################

# Collecting cross entropy for each K = 1..30

K.crossEntropy = c()
for(KVal in 1:30) {
  df.kknn <- kknn(formula = as.factor(V65) ~ ., train=train,
                  test=valid, kernel="rectangular", k = KVal)
  df.kknn.fit <- fitted(df.kknn)

  x <- 0
  for (i in 1:length(valid$V65)){
    for (j in 0:9) {
      if (valid$V65[i] == j) {
        x <- x + (log(df.kknn$prob[i, j+1] + 1e-15))
      }
    }
  }

  K.crossEntropy <- c(K.crossEntropy, -x)
}
plot(K.crossEntropy, xlab = "K", ylab = "Cross Entropy", col = "blue")

