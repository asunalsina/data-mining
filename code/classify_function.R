tree.classify = function(x, tr){
  # x = attribute values of the cases for which predictions are required
  # tr = tree object
  # y = vector of predictions
  y = c()
  for (i in 1:nrow(x)){
    y_new = c()
    j = 1
    case = x[i,]
    attribute = tree$attribute[j]
    tree_value = as.integer(tree$value[j])
    case_value = case[attribute][[1]]
    while(is.null(y_new)){
      if (case_value > tree_value){
        # go to even node 2*j
        attribute = tree$attribute[2*j]
        if (attribute == "-")
        {
          y_new = tree$majority[2*j]
          y[i] = as.integer(y_new)
        }else{
          tree_value = as.integer(tree$value[2*j])
          case_value = case[attribute][[1]]
          j = j + 1
        }
      }else{
        # go to odd node 2*j+1
        attribute = tree$attribute[2*j+1]
        if (attribute == "-")
        {
          y_new = tree$majority[2*j+1]
          y[i] = as.integer(y_new)
        }
        else{
          tree_value = as.integer(tree$value[2*j+1])
          case_value = case[attribute][[1]]
          j = j + 1
        }
      }
    }
  }
  return(y)
}