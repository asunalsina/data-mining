tree.classify = function(x, tr){
  # x = attribute values of the cases for which predictions are required
  # tr = tree object
  # y = vector of predictions
  y = c()
  for (i in 1:nrow(x)){
    y_new = c()
    j = 1
    case = x[i,]
    attribute = tr$attribute[j]
    tr_value = as.integer(tr$value[j])
    case_value = case[attribute][[1]]
    while(is.null(y_new)){
      if (case_value > tr_value){
        # go to even node 2*j
        j = as.integer(tr$left[j])
        attribute = tr$attribute[j]
        if (attribute == "-")
        {
          y_new = tr$majority[j]
          y[i] = as.integer(y_new)
        }else{
          tr_value = as.integer(tr$value[j])
          case_value = case[attribute][[1]]
        }
      }else{
        # go to odd node 2*j+1
        j = as.integer(tr$right[j])
        attribute = tr$attribute[j]
        if (attribute == "-")
        {
          y_new = tr$majority[j]
          y[i] = as.integer(y_new)
        }
        else{
          tr_value = as.integer(tr$value[j])
          case_value = case[attribute][[1]]
        }
      }
    }
  }
  return(y)
}