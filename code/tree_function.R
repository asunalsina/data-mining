# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  first_node = cbind(x, y)
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c())
  remaining_nodes = list(first_node)
  
  while (length(remaining_nodes) != 0 && nfeat != 0) {
    
    # Split feature and best split based on that feature
    feature = floor(runif(1, min = 1, max = nfeat + 1))
    nfeat = nfeat - 1
  
    for (i in 1:length(remaining_nodes)){
      
      x_node = remaining_nodes[[i]][,1:length(remaining_nodes[[i]])-1]
      y_node = remaining_nodes[[i]]$class
      
      # Update remaining nodes
      # remaining_nodes = remaining_nodes[[-i]]
      
      # We check if the node has more than one class
      if(length(y_node) > nmin && length(unique(y_node)) != 1){ # If this is true the node can be split
        
        split = check_binary(x_node, y_node)
        
        remaining_features = x_node[, -feature]
        
        # Get the children nodes
        children = children_nodes(x_node, y_node, feature, split)
        right_child = children[[1]]
        left_child = children[[2]]
      
        # Add the node to the tree
        node = data.frame("attribute" = colnames(x_node[feature]), "value" = split, "left" = 2*i, "right" = 1+2*i, "majority" = "-")
        tree = rbind(tree, node)
        
        # Update remaining nodes
        remaining_nodes[[2*i+1]] = right_child
        remaining_nodes[[2*i]] = left_child

      } else{
          if(length(y_node) > minleaf){
            # Leaf node
            node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = majority(y_node))
            tree = rbind(tree, node)
          }
      }
    }
  }

  return(tree)
}