select_feature = function(x, y){
  # First, we calculate the different possible splits
  qualities = c()
  

  for (j in 1:length(x)){
    df = sort(unique(x[,j]))
    feature = (df[1:(length(df)-1)] + df[2:length(df)]) / 2
    total_quality = c()
    
    parent_impurity = gini_index(y)
    # For loop to calculate the quality of each interval
    for (i in 1:length(feature)){
      
      # Interval selection
      children = split(x[,j], y, feature[i])
      right_child = children[[1]]
      left_child = children[[2]]
      
      # Number of 1 on each node
      right_child_sum = sum(right_child)
      left_child_sum = sum(left_child)
      
      # Impurity of each node using gini_index
      impurity_right_node = gini_index(right_child)
      impurity_left_node = gini_index(left_child)
      
      # Quality of the split according to splits on numeric attributes (slide 31)
      split_quality = impurity_right_node * length(right_child) / length(x[,j]) + impurity_left_node * length(left_child) / length(x[,j])
      total_quality[i] = parent_impurity - split_quality
    }
    qualities[j] = max(total_quality)
    selected_quality = max(qualities)
    selected_feature = match(selected_quality, qualities)
  }
  return(selected_feature)
}