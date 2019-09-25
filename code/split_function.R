# Function to calculate the split
split <- function(x, y, feature){
  
  # Children nodes
  right_child = y[x <= feature] 
  left_child = y[x > feature] 
  
  return(list(right_child, left_child))
}