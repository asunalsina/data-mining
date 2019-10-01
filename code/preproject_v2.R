# We read the data
credit.dat = read.csv("/Users/Asun/Desktop/credit.txt")
x = credit.dat[, 1:length(credit.dat)-1]
xt = credit.dat[, 4]
y = credit.dat[, 6]
nmin = 2
minleaf = 1
nfeat = length(x)
# With this line we can use the functions written in the script
source("impurity_function.R")
source("split_function.R")
source("best_split.R")
source("children_nodes_function.R")
source("majority_function.R")
source("select_feature_function.R")
source("tree_function.R")


#debug(tree.grow)
tree.grow(x, y, nmin, minleaf, nfeat)
