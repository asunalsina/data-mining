# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  # Check if the node can be split
  # We need to know the length of y to know how many cases there are in the first node
  observation = length(y)
  
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c())
  remaining_nodes = list()
  # Split feature and best split based on that feature
  first_feature = floor(runif(1, min = 1, max = nfeat + 1))
  nfeat = nfeat - 1
  
  if (length(unique(x[,first_feature])) == 2){
    first_split = 0.5
  }else{
    first_split = best_split(x[, first_feature], y)
  }
  
  # Remaining features we can choose
  remaining_features = x[, -first_feature]
  
  # Create children nodes
  #children = split(x, y, first_split) 
  children = children_nodes(x, y, first_feature, first_split)
  right_child = children[[1]]
  left_child = children[[2]]
  
  # We create the row for the node and add it to the tree
  node = data.frame("attribute" = colnames(x[first_feature]), "value" = first_split, "left" = 2, "right" = 3, "majority" = "-")
  tree = rbind(tree, node)
  
  remaining_nodes[[1]] = right_child
  remaining_nodes[[2]] = left_child

  # while remaining_nodes is not empty:
  # check whether it can be split
  # split node
  # delete node from remaining nodes
  # check whether can be split new nodes
  # store new nodes in remaining nodes
  while (length(remaining_nodes) != 0 && nfeat != 0) {
    feature = floor(runif(1, min = 1, max = nfeat + 1))
    nfeat = nfeat - 1
    
    for (i in 1:length(remaining_nodes)){
    
      x_node = remaining_nodes[[i]][,1:length(remaining_nodes[[i]])-1]
      y_node = remaining_nodes[[i]]$class
      
      # Update remaining nodes
      remaining_nodes = remaining_nodes[[-i]]
      
      # We check whether the node is formed by only one class
      # One class: leaf node
      # Two classes: split
      if (length(unique(y_node)) != 1){
        if (length(unique(x[,first_feature])) == 2){
          # Binary class
          split = 0.5
        }else{
          # Numerical class
          split = best_split(x_node[, feature], y_node)
        }
    
        remaining_features = x_node[, -feature]
        
        # Get the children nodes
        children = children_nodes(x_node, y_node, feature, split)
        right_child = children[[1]]
        left_child = children[[2]]
        
        # Add the node to the tree
        node = data.frame("attribute" = colnames(x_node[feature]), "value" = split, "left" = 2+2*i, "right" = 3+2*i, "majority" = "-")
        tree = rbind(tree, node)
        
        # Update remaining nodes
        remaining_nodes[[2*i+1]] = right_child
        remaining_nodes[[2*i+2]] = left_child
        
      }else{
        # Add the node to the tree
        node = data.frame("attribute" = colnames(x_node[feature]), "value" = split, "left" = "-", "right" = "-", "majority" = majority(y_node))
        tree = rbind(tree, node)
      }
      
    }
  }
    
  return(tree)
}