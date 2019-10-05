# We read the data
library(mlbench)
library(rpart)
library(rpart.plot)

#credit.dat = read.csv("/Users/Asun/Desktop/credit.txt")
#x = credit.dat[, 1:length(credit.dat)-1]
#y = credit.dat[, 6]

data(PimaIndiansDiabetes)
x = PimaIndiansDiabetes[, 1:length(PimaIndiansDiabetes)-1]
y = PimaIndiansDiabetes[, length(PimaIndiansDiabetes)]
y = as.integer(y)
for (i in 1:length(y)){
  y[i] = y[i] - 1
}

nmin = 20
minleaf = 5
nfeat = length(x)

# With this line we can use the functions written in the script
source("impurity_function.R")
source("split_function.R")
source("children_nodes_function.R")
source("majority_function.R")
source("random_features_function.R")
source("evaluate_interval_function.R")
source("tree_function.R")
source("classify_function.R")

# Algorithm to create a tree, print it and classify some data using that tree
#dat = cbind(x,y)
#fit = rpart(y~., data = dat, method = 'class')
#rpart.plot(fit, extra = 206)
#predict_unseen <-predict(fit, x, type = 'class')

#debug(tree.grow)
tree = tree.grow(x, y, nmin, minleaf, nfeat)
#debug(tree.classify)
labels = tree.classify(x, tree)
t = table(labels, y)
accuracy = (table(labels, y)[1,1] + table(labels, y)[2,2]) / sum(table(labels, y))
accuracy
