# Function to calculate the gini index
gini_index <- function(node_sum, node_len){

  p1 = node_sum / node_len
  p0 = (1 - p1)

  gini_impurity = p0 * p1
  
  return(gini_impurity)
}

# Function to calculate the best split
best_split <- function(x, y){

  # First, we calculate the different possible splits
  df = sort(unique(x))
  income_mean = (df[1:7] + df[2:8]) / 2
  
  total_impurity = c()
  
  parent_impurity = gini_index(sum(x), length(x))
  
  # For loop to calculate the quality of each interval
  for (i in 1:length(income_mean)){
    
    # Interval selection
    right_child = y[x > income_mean[i]] 
    left_child = y[x <= income_mean[i]] 
    
    # Number of 1 on each node
    right_child_sum = sum(right_child)
    left_child_sum = sum(left_child)
    
    # Impurity of each node using gini_index
    impurity_right_node = gini_index(right_child_sum, length(right_child))
    impurity_left_node = gini_index(left_child_sum, length(left_child))
    
    # Quality of the split according to splits on numeric attributes (slide 31)
    split_quality = impurity_right_node * length(right_child) / length(x) + impurity_left_node * length(left_child) / length(x)
    
    # Quality of the interval second part (also slide 31)
    total_quality[i] = parent_impurity - split_quality
  }
  
  # Selection of the interval with the greatest quality
  best_quality = max(total_quality)
  
  # Index of the selected interval
  index_of_interval = match(best_quality, total_quality)
  
  # Value of the selected interval
  best_interval = income_mean[index_of_interval]
  
  return(best_interval)
}