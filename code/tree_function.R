# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  # Check if the node can be split
  # We need to know the length of y to know how many cases there are in the first node
  observation = length(y)
  
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c())
  
  # Split feature and best split based on that feature
  first_feature = floor(runif(1, min = 1, max = nfeat + 1))
  first_split = best_split(x[, first_feature], y)
  
  # Remaining features we can choose
  remaining_features = x[, -first_feature]
  
  # Create children nodes
  children = split(x, y, first_split) 
  right_child = children[1]
  left_child = children[2]
  
  # We create the row for the node and add it to the tree
  node = data.frame("attribute" = colnames(tree[first_feature]), "value" = first_split, "left" = 2, "right" = 3, "majority" = "-")
  tree = rbind(tree, node)
  
  return(tree_object)
}