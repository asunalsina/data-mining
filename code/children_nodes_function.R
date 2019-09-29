children_nodes <- function(x, y, feature, best_split){
  
  x = cbind(x,y)
  
  left_child = x[x[,feature] >= best_split, ]
  left_child = left_child[,-feature]
  
  right_child = x[x[,feature] < best_split, ]
  right_child_x = right_child[,-feature]
  
  return(list(right_child, left_child))
}