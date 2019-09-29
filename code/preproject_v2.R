# We read the data
credit.dat = read.csv("/Users/Asun/Desktop/credit.txt")
x = credit.dat[, 1:length(credit.dat)-1]
#x = credit.dat[, 3]
y = credit.dat[, 6]

# With this line we can use the functions written in the script
source("impurity_function.R")
source("split_function.R")
source("best_split.R")
source("children_nodes_function.R")
source("majority_function.R")
source("tree_function.R")
tree = tree.grow(x, y, 1, 1, length(x))

a = 36
feature = 4
nodes = children_nodes(x, y, feature, a)
left = nodes[[2]]
right = nodes[[1]]




