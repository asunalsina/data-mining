# Function to calculate the gini index
gini_index <- function(node){

  p1 = sum(node) / length(node_len)
  p0 = (1 - p1)

  gini_impurity = p0 * p1
  
  return(gini_impurity)
}