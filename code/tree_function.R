# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  first_node = cbind(x, y)
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c(), stringsAsFactors=FALSE)
  remaining_nodes = list(first_node)
  i = 1
  
  while(length(remaining_nodes) != 0){
    
    x = remaining_nodes[[1]][,1:length(x)]
    x = random_features(x, nfeat)
    y = remaining_nodes[[1]]$y

    featsplit = evaluate_feature(x, y, minleaf)
    feature = featsplit[[1]]
    interval = featsplit[[2]]
    no_feature = featsplit[[3]]

    # Get the children nodes
    children = children_nodes(x, y, feature, interval)
    right_child = children[[1]]
    left_child = children[[2]]
    
    if(nrow(right_child) < minleaf | nrow(left_child) < minleaf | no_feature == 0){
      m = majority(y)
      node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = m, stringsAsFactors=FALSE)
      tree = rbind(tree, node)
      remaining_nodes = remaining_nodes[-1]
    }else{
    if ((length(y) >= minleaf && length(y) < nmin) || length(unique(y)) == 1){
      # Leaf node
      m = majority(y)
      node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = m, stringsAsFactors=FALSE)
      tree = rbind(tree, node)
      remaining_nodes = remaining_nodes[-1]
    }
    if(length(y) >= nmin){
      
      node = data.frame("attribute" = colnames(x[feature]), "value" = interval, "left" = 2*i, "right" = 1+2*i, "majority" = "-", stringsAsFactors=FALSE)
      tree = rbind(tree, node)
      i = i + 1
      # Update remaining nodes
      l = length(remaining_nodes)
      remaining_nodes[[l+1]] = left_child
      remaining_nodes[[l+2]] = right_child
      remaining_nodes = remaining_nodes[-1]
    }
    if(length(y) < minleaf){
      # Error check
      print("Something wrong")
    }
    }
  }  
  return(tree)
}