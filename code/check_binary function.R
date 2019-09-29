check_binary <- function(x_node, y_node){
  
  if (length(unique(x_node[,feature])) == 2){
    # Binary class
    split = 0.5
  }else{
    # Numerical class
    split = best_split(x_node[, feature], y_node)
  }

  return (split)
}