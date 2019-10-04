evaluate_feature = function(x, y, minleaf){

qualities = c()
total_quality_intervals = list()
parent_impurity = gini_index(y)

  for (j in 1:length(x)){
    df = sort(unique(x[,j]))
    if (length(df) > 1){
      feature = (df[1:(length(df)-1)] + df[2:length(df)]) / 2
      total_quality = c()
      
      # For loop to calculate the quality of each interval
      for (i in 1:length(feature)){
        
        # Interval selection
        children = split_node(x[,j], y, feature[i], minleaf)

        if(length(children) > 1){
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
        }else{total_quality[i] = 0}
      }

      total_quality_intervals[[j]] = total_quality
      qualities[j] = max(total_quality)

    }else{
      total_quality_intervals[[j]] = 0
      qualities[j] = 0
    }
  }

  selected_quality = max(qualities)
  selected_feature = match(selected_quality, qualities)
    
  index_of_interval = match(selected_quality, total_quality_intervals[[selected_feature]])
  df_new = sort(unique(x[,selected_feature]))
  feature_new = (df_new[1:(length(df_new)-1)] + df_new[2:length(df_new)]) / 2
  best_interval = feature_new[index_of_interval]
  
  return(list(selected_feature, best_interval, selected_quality))
}





