# We read the data
credit.dat = read.csv("/Users/Asun/Desktop/credit.txt")
x = credit.dat[, 4]
y = credit.dat[, 6]

# With this line we can use the functions written in the script
source("impurity_function.R")

# We call the function to know the best split
best_split(x, y)