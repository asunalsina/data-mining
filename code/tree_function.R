# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  first_node = cbind(x, y)
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c(), stringsAsFactors=FALSE)
  remaining_nodes = list(first_node)
  i = 1
  
  while (length(remaining_nodes) != 0) {
    
    # Split feature and best split based on that feature
    feature = floor(runif(1, min = 1, max = nfeat + 1))
    
    x_node = remaining_nodes[[1]][,1:(length(remaining_nodes[[1]])-1)]
    y_node = remaining_nodes[[1]]$y
    
    while (length(unique(x_node[,feature])) == 1 && length(x_node[,feature]) > 1){
      feature = floor(runif(1, min = 1, max = nfeat + 1))
    }
    
    # We check if the node has more than one class
    if(length(y_node) > nmin && length(unique(y_node)) != 1){ # If this is true the node can be split
      
      split = check_binary(x_node, y_node, feature)
      
      # Get the children nodes
      children = children_nodes(x_node, y_node, feature, split)
      right_child = children[[1]]
      left_child = children[[2]]
      
      # Add the node to the tree
      node = data.frame("attribute" = colnames(x_node[feature]), "value" = split, "left" = 2*i, "right" = 1+2*i, "majority" = "-", stringsAsFactors=FALSE)
      tree = rbind(tree, node)
      i = i + 1
      
      # Update remaining nodes
      l = length(remaining_nodes)
      remaining_nodes[[l+1]] = left_child
      remaining_nodes[[l+2]] = right_child
      remaining_nodes = remaining_nodes[-1]
      
    } else{
      if(length(y_node) >= minleaf){
        # Leaf node
        m = majority(y_node)
        node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = m, stringsAsFactors=FALSE)
        tree = rbind(tree, node)
        remaining_nodes = remaining_nodes[-1]
      }
    }
  }

  return(tree)
}