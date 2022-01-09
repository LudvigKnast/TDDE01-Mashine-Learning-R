# Load library for this assignment
library(tree)

# Load data
bank = read.csv2("bank-full.csv", stringsAsFactors = TRUE)

##############################################
# ----------------- Task 1 ----------------- #
##############################################

# Remove variable duration
bank$duration <- NULL

# Split data to train, test and validation
n     = dim(bank)[1]
set.seed(12345)
id    = sample(1:n, floor(n*0.4))
train = bank[id,]

id1   = setdiff(1:n, id)
set.seed(12345)
id2   = sample(id1, floor(n*0.3))
valid = bank[id2,]

id3  = setdiff(id1,id2)
test = bank[id3,]


##############################################
# ----------------- Task 2 ----------------- #
##############################################

# =============================
# Grow tree with default values
# =============================
bankTree = tree(formula = y ~ ., data = train)

# Display tree
plot(bankTree)
text(bankTree, pretty=0)
summary(bankTree)

# Predict and calculate misclassification rate for test-data
predictTest = predict(bankTree, newdata=test, type='class')
confMatrix_test <- table(actual=test$y, predicted=predictTest)
missRate_test   <- 1-sum(diag(confMatrix_test))/nrow(test)

# Predict and calculate misclassification rate for validation-data
predictValid = predict(bankTree, newdata=valid, type="class")
confMatrix_valid <- table(actual=valid$y, predicted=predictValid)
missRate_valid <- 1-sum(diag(confMatrix_valid))/nrow(valid)

# =============================
# Grow tree with node size 7000
# =============================
bankTree = tree(formula = y ~ ., data = train,
                control = tree.control(nrow(train), minsize = 7000))

# Display tree
plot(bankTree)
text(bankTree, pretty=0)
summary(bankTree)

# Predict and calculate misclassification rate for test-data
predictTest = predict(bankTree, newdata=test, type='class')
confMatrix_test <- table(actual=test$y, predicted=predictTest)
missRate_test2  <- 1-sum(diag(confMatrix_test))/nrow(test)

# Predict and calculate misclassification rate for validation-data
predictValid = predict(bankTree, newdata=valid, type="class")
confMatrix_valid <- table(actual=valid$y, predicted=predictValid)
missRate_valid2  <- 1-sum(diag(confMatrix_valid))/nrow(valid)

# =============================
# Grow tree with min.dev 0.0005
# =============================
bankTree = tree(formula = y ~ ., data = train,
                control = tree.control(nrow(train), mindev = 0.0005))

# Display tree
plot(bankTree)
text(bankTree, pretty=0)
summary(bankTree)

# Predict and calculate misclassification rate for test-data
predictTest = predict(bankTree, newdata=test, type="class")
confMatrix_test <- table(actual=test$y, predicted=predictTest)
missRate_test3  <- 1-sum(diag(confMatrix_test))/nrow(test)

# Predict and calculate misclassification rate for validation-data
predictValid = predict(bankTree, valid, type="class")
confMatrix_valid <- table(actual=valid$y, predicted=predictValid)
missRate_valid3  <- 1-sum(diag(confMatrix_valid))/nrow(valid)


##############################################
# ----------------- Task 3 ----------------- #
##############################################
bankTree=tree(formula = y ~ ., data = train,
              control = tree.control(nrow(train), mindev = 0.0005))

trainScore=rep(0,50)
validScore=rep(0,50)
for(i in 2:50)
{
  prunedTree = prune.tree(bankTree, best=i)
  predictValid <- predict(prunedTree, newdata=valid, type="tree")
  trainScore[i] = deviance(prunedTree)/nrow(train)
  validScore[i] = deviance(predictValid)/nrow(valid)
}
plot(2:50, trainScore[2:50],
     type = "b", col = "red", 
     xlab = "Number of leaves",
     ylab = "Average deviance score",
     main = "Average deviance for training and validation data")
points(2:50, validScore[2:50],
       type = "b", col = "blue")
legend(x="topright", 
       legend = c("Training data","Validation data"),
       col = c("red", "blue"), 
       pch = "oo")
best_leaves = which.min(validScore[-1])+1
cat("Best # of leaves = ", best_leaves)
finalTree = prune.tree(bankTree, best=best_leaves)
Yfit <- predict(finalTree, newdata=valid, type="class")
table(predicted=Yfit, actual=valid$y)

plot(finalTree)
text(finalTree, pretty=1)
summary(finalTree)

##############################################
# ----------------- Task 4 ----------------- #
##############################################
testOptTree = predict(finalTree, newdata=test, type="class")

# Calc confusion matrix
confM = table(actual=test$y, predicted=testOptTree)

# Calc accuracy
accuracy <- sum(diag(confM)) / sum(confM)

# Calc F1 score
precision = confM[2,2]/sum(confM[,2])
recall    = confM[2,2]/sum(confM[2,])
F1   <- 2 * (precision * recall) / (precision + recall)


##############################################
# ----------------- Task 5 ----------------- #
##############################################
# If p(y=no|x)/p(y=yes|x) > 5 => classify as no else classify as yes

# Optimal tree probabilities
loss = list(probs = predict(finalTree, newdata=test, type="vector"))

# Predict for test-data using loss matrix
loss$fit = ifelse(loss$probs[,"no"]/loss$probs[,"yes"] > 5, "no", "yes")
confM_loss = table(actual=test$y, predicted=loss$fit)

# Calculating accuracy and F1-score for new tree.
accuracyLoss <- sum(diag(confM_loss)) / sum(confM_loss)

precisionLoss = confM_loss[2,2]/sum(confM_loss[,2])
recallLoss    = confM_loss[2,2]/sum(confM_loss[2,])
F1Loss  <- 2 * (precisionLoss * recallLoss) / (precisionLoss + recallLoss)


##############################################
# ----------------- Task 6 ----------------- #
##############################################

# Model a logistic regression
glm_logistic = glm(formula = y ~ ., data = train,
                   family = "binomial")

# Remodelling the optimal tree (to easier use only this section of code).
bankTree  = tree(formula = y ~ ., data = train,
                 control = tree.control(nrow(train), mindev = 0.0005))
finalTree = prune.tree(bankTree, best=22)

# Computing FPR and TPR for tree and log-model.
FPRs_glm = numeric(19)
TPRs_glm = numeric(19)
FPRs_tree = numeric(19)
TPRs_tree = numeric(19)

i = 1
for (r in seq(0.05, 0.95, 0.05))
{
  # TPR and FPR for regression model
  glm_test = as.numeric(predict(object=glm_logistic, newdata = test,
                                type='response') > r)
  conf_glm <- table(predicted=glm_test, actual=test$y)
  TPRs_glm[i] <- conf_glm[2,2] / sum(conf_glm[,2])
  FPRs_glm[i] <- conf_glm[2,1] / sum(conf_glm[,1])
  
  # TPR and FPR for tree
  # Get "yes" probabilities
  tree_test = factor(x = predict(object=finalTree, newdata = test,
                                 type='vector')[, 2] > r, 
                     levels = c(FALSE, TRUE), 
                     labels = c("no", "yes"))
  conf_tree <- table(predicted=tree_test, actual=test$y)
  TPRs_tree[i] <- conf_tree[2,2] / sum(conf_tree[,2])
  FPRs_tree[i] <- conf_tree[2,1] / sum(conf_tree[,1])
  
  i = i+1
}
table(predicted=glm_test, actual=test$y)

# Plot ROC-curves
plot(FPRs_glm, TPRs_glm,
     col  = "blue", type = 'l',
     xlim = c(0,1), ylim = c(0,1),
     xlab = "False Positive Rate", 
     ylab = "True Positive Rate", 
     main = "ROC-curves for tree and regression")
lines(FPRs_tree, TPRs_tree,
      col = "red")
legend(x = "bottomright", 
       legend = c("Decision Tree","Logistic Regression"),
       col = c("red", "blue"), 
       pch = "--")