# Function to calculate the split
split_node <- function(x, y, feature, minleaf){
  
  # Children nodes
  right_child = y[x >= feature] 
  left_child = y[x < feature] 
  
  if(length(right_child) < minleaf | length(left_child) < minleaf){
    return(0)
  }else{
    return(list(right_child, left_child))
  }
}