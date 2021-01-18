
install.packages("tree")
library("tree")



real.estate <- read.table("C:/Users/Muthu Govindhan/Desktop/project/cadata.csv", header=TRUE)

tree.model <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=real.estate)
plot(tree.model)
text(tree.model, cex=.75)

price.deciles <- quantile(real.estate$MedianHouseValue, 0:10/10)
cut.prices    <- cut(real.estate$MedianHouseValue, price.deciles, include.lowest=TRUE)
plot(real.estate$Longitude, real.estate$Latitude, col=grey(10:2/11)[cut.prices], pch=20, xlab="Longitude",ylab="Latitude")
partition.tree(tree.model, ordvars=c("Longitude","Latitude"), add=TRUE)

summary(tree.model)

tree.model2 <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=real.estate, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)

summary(tree.model2)

tree.model3 <- tree(log(MedianHouseValue) ~ ., data=real.estate)
plot(tree.model3)
text(tree.model3, cex=.75)

summary(tree.model3)

#Classification Trees
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(iris), alpha * nrow(iris))
train.set <- iris[inTrain,]
test.set  <- iris[-inTrain,]

tree.model <- tree(Species ~ Sepal.Width + Petal.Width, data=train.set)
tree.model

summary(tree.model)

# Distributional prediction
my.prediction <- predict(tree.model, test.set) # gives the probability for each class
head(my.prediction)


# Point prediction
# Let's translate the probability output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('setosa', 'versicolor', 'virginica')[idx]
table(prediction, test.set$Species)

plot(tree.model)
text(tree.model)

# Another way to show the data:
plot(iris$Petal.Width, iris$Sepal.Width, pch=19, col=as.numeric(iris$Species))
partition.tree(tree.model, label="Species", add=TRUE)
legend("topright",legend=unique(iris$Species), col=unique(as.numeric(iris$Species)), pch=19)

summary(tree.model)

pruned.tree <- prune.tree(tree.model, best=4)
plot(pruned.tree)
text(pruned.tree)

pruned.prediction <- predict(pruned.tree, test.set, type="class") # give the predicted class
table(pruned.prediction, test.set$Species)

# here, let's use all the variables and all the samples
tree.model <- tree(Species ~ ., data=iris)
summary(tree.model)

cv.model <- cv.tree(tree.model)
plot(cv.model)

cv.model$dev  # gives the deviance for each K (small is better)

best.size <- cv.model$size[which(cv.model$dev==min(cv.model$dev))] # which size is better?
best.size

# let's refit the tree model (the number of leafs will be no more than best.size)
cv.model.pruned <- prune.misclass(tree.model, best=best.size)
summary(cv.model.pruned
        
#Package rpart
install.packages("rpart")
library("rpart")

rpart.tree <- rpart(Species ~ ., data=train.set)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")

predictions <- predict(rpart.tree, test.set, type="class")
table(test.set$Species, predictions)

lmat <- matrix(c(0,1,2,
                 1,0,100,
                 2,100,0), ncol = 3)
lmat

rpart.tree <- rpart(Species ~ ., data=train.set, parms = list(loss = lmat))
predictions <- predict(rpart.tree, test.set, type="class")
table(test.set$Species, predictions)

plot(rpart.tree)
text(rpart.tree)


## Define a plotting function with decent defaults
plot.rpart.obj <- function(rpart.obj, font.size = 0.8) {
  ## plot decision tree
  plot(rpart.obj,
       uniform   = T,    # if 'TRUE', uniform vertical spacing of the nodes is used
       branch    = 1,    # controls the shape of the branches from parent to child node
       compress  = F,    # if 'FALSE', the leaf nodes will be at the horizontal plot
       nspace    = 0.1,
       margin    = 0.1, # an extra fraction of white space to leave around the borders
       minbranch = 0.3)  # set the minimum length for a branch
  
  ## Add text
  text(x      = rpart.obj,   #
       splits = T,           # If tree are labeled with the criterion for the split
       all    = T,           # If 'TRUE', all nodes are labeled, otherwise just terminal nodes
       use.n  = T,           # Use numbers to annotate
       cex    = font.size)   # Font size
}

plot.rpart.obj(rpart.tree, 1)




#The package party gives better plotting and text functions:

install.packages("partykit")
library("partykit")

rparty.tree <- as.party(rpart.tree)
rparty.tree

plot(rparty.tree)

fit <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fit) # display the results

plotcp(fit) # visualize cross-validation results

summary(fit) # detailed summary of splits


# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results 


par(mfrow=c(1,1)) 

# plot tree
plot(fit, uniform=TRUE, main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



#Random Forests
#Random forests are an ensemble learning method for classification (and regression) that
#operate by constructing a multitude of decision
#trees at training time and outputting the class that is the mode of the classes output by individual trees

library("randomForest")
r <- randomForest(Species ~., data=train.set, importance=TRUE, do.trace=100, ntree=100)
print(r)

predictions <- predict(r, test.set)
table(test.set$Species, predictions)

# next function gives a graphical depiction of the marginal effect of a variable on the class probability (classification) or response (regression).
partialPlot(r, train.set, Petal.Width, "versicolor")

t <- getTree(r, k=2) # get the second tree
print(t)

treesize(r) # size of trees of the ensemble

hist(treesize(r))

#We can also tune the structure, ie, finding the best hyperparameters of the method via grid search

library("e1071") # to access 'tune' method

tuned.r <- tune(randomForest, train.x = Species ~ .,
                data = train.set,
                validation.x = test.set)

best.model <- tuned.r$best.model
predictions <- predict(best.model, test.set)
table.random.forest <- table(test.set$Species, predictions)
table.random.forest

# computing overall error:
error.rate <- 1 - sum(diag(as.matrix(table.random.forest))) / sum(table.random.forest)
error.rate

#Conditional Inference Trees
install.packages("party")
library("party")
help(party)
#Conditional inference trees estimate a regression relationship by binary recursive partitioning in a conditional inference framework. Roughly, the algorithm works as follows: 1) Test the global null hypothesis of independence between any of the input variables and the response (which may be multivariate as well). Stop if this hypothesis cannot be rejected. Otherwise select the input variable with strongest association to the resonse. This association is measured by a p-value corresponding to a test for the partial null hypothesis of a single input variable and the response. 2) Implement a binary split in the selected input variable
iris.model <- ctree(Species ~ . , data = train.set)
plot(iris.model)


predictions <- predict(iris.model, test.set[,-5])
table(predictions, test.set$Species)

# what are the predicted probabilities for the given samples?
treeresponse(iris.model, newdata=iris[c(10,87,128),])

# get the probabilities from the barplots showen above:
tapply(treeresponse(iris.model), where(iris.model), unique)


# The package is able to format the plot tree. Eg:
innerWeights <- function(node){
  grid.circle(gp = gpar(fill = "White", col = 1))
  mainlab <- paste( node$psplit$variableName, "\n(n = ")
  mainlab <- paste(mainlab, sum(node$weights),")" , sep = "")
  grid.text(mainlab,gp = gpar(col='red'))
}
plot(iris.model, type='simple', inner_panel = innerWeights)