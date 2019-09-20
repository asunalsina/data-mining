# Observation: number of cases both good and bad

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  # Check if the node can be split
  # We need to know the length of y to know how many cases there are in the first node
  observation = length(y)
  
  if (observation < minleaf){
    #node not acceptable
  }
  else{
    if (observation < nmin){
      #leaf node
    }
    else{
      # Select feature
      first_feature = floor(runif(1, min = 1, max = nfeat + 1))
      first_split = best_split(x[, selected_feature], y)
      
      # Create children nodes
      right_child = y[x <= first_split] 
      left_child = y[x  > first_split]
      
      # Check dimension of each node to know if it will be leaf or not and if it is acceptable
      if (length(right_child) < minleaf || length(left_child) < minleaf)
      {
        # split not acceptable
      }
      else{
        if (length(right_child) < nmin){
          # leaf node
        }
        else{
          # next split
        }
        if (length(left_child) < nmin){
          # leaf node
        }
        else{
          # next split
        }
      }
    }
  }
  
  return(tree_object)
}