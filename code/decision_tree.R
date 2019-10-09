# x: attribute values (data matrix)
# y: class labels (vector)
# nmin: number of observations that a node must contain at least, for it to be allowed to be split (integer)
# minleaf: minimum number of observations required for a leaf node (integer)
# nfeat: features from which the bestsplit is to be selected (integer)
# tree: tree object that can be used to predict new classes (data frame)
# tree.grow builds a decision tree
tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  # Dataframe for the final tree
  tree = data.frame("attribute" = c(), "value" = c(), "left" = c(), "right" = c(), "majority" = c(), stringsAsFactors=FALSE)
  # List for the nodes that can be split
  remaining_nodes = list(cbind(x, y))
  i = 1
  
  # Loop to split nodes
  while(length(remaining_nodes) != 0){
    
    x = remaining_nodes[[1]][,1:length(x)]
    # Selection of the features that are going to be used (if nfeat = length(x) then all of them will be used)
    x = x[,sort(sample(1:length(x), nfeat, replace=FALSE))]
    y = remaining_nodes[[1]]$y
    
    # Function that evaluates the split and returns the best split that meets the minleaf condition
    featsplit = evaluate_feature(x, y, minleaf)
    # It returns the feature and the interval to split the data
    # Also returns a third parameter to indicate that there is no valid split and the node should be leaf node
    feature = featsplit[[1]]
    interval = featsplit[[2]]
    no_feature = featsplit[[3]]
    
    # Get the children nodes
    children = children_nodes(x, y, feature, interval)
    # right_child = children[[1]]
    # left_child = children[[2]]
    
    # Check if both children meet the conditions
    # If they do not meet them, the parent node will be converted into a leaf node
    if(nrow(children[[1]]) < minleaf | nrow(children[[2]]) < minleaf | no_feature == 0){
      # Leaf node
      # Create of the node row
      node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = majority(y), stringsAsFactors=FALSE)
      # Add the node to the tree
      tree = rbind(tree, node)
      # Delete the node from the list of nodes to split
      remaining_nodes = remaining_nodes[-1]
    }else{
      if ((length(y) >= minleaf && length(y) < nmin) || length(unique(y)) == 1){
        # Leaf node
        # Create of the node row
        node = data.frame("attribute" = "-", "value" = "-", "left" = "-", "right" = "-", "majority" = majority(y), stringsAsFactors=FALSE)
        # Add the node to the tree
        tree = rbind(tree, node)
        # Delete the node from the list of nodes to split
        remaining_nodes = remaining_nodes[-1]
      }
      if(length(y) >= nmin){
        # Create of the node row
        node = data.frame("attribute" = colnames(x[feature]), "value" = interval, "left" = 2*i, "right" = 1+2*i, "majority" = "-", stringsAsFactors=FALSE)
        # Add the node to the tree
        tree = rbind(tree, node)
        # Index for the number of the children nodes
        i = i + 1
        # Update remaining nodes
        l = length(remaining_nodes)
        remaining_nodes[[l+1]] = children[[2]] # left child
        remaining_nodes[[l+2]] = children[[1]] # right child
        remaining_nodes = remaining_nodes[-1]
      }
      
    }
  }  
  return(tree)
}


# x: attribute values of the cases for which predictions are required (data matrix)
# tr: tree object (data frame)
# y: predicted class labels (vector)
# tree.classify uses a decision tree to classify new cases (x) and return the predictions in a vector (y)
tree.classify = function(x, tr){
  
  # Vector of predictions
  y = c()
  # Iterate the cases in the data matrix
  for (i in 1:nrow(x)){
    # Variable to store the prediction for a case
    y_new = c()
    j = 1
    # Selected case
    case = x[i,]
    # Attribute of the split
    attribute = tr$attribute[j]
    # Value of the split
    tr_value = as.integer(tr$value[j])
    # Value of the case for the selected attribute
    case_value = case[attribute][[1]]
    # Loop to calculate the prediction
    while(is.null(y_new)){
      # Go to the left
      if (case_value > tr_value){
        # Calculate the next node
        j = as.integer(tr$left[j])
        # Extract the attribute of the next node
        attribute = tr$attribute[j]
        if (attribute == "-"){
          # When the attribute is empty is a leaf node
          # Look for the majority and assign it as the prediction
          y_new = tr$majority[j]
          y[i] = as.integer(y_new)
        }else{
          # Extract the tree value of the next node
          tr_value = as.integer(tr$value[j])
          # Extract the case value for the next attribute
          case_value = case[attribute][[1]]
        }
      }else{ #Go to the right
        # Calculate the next node
        j = as.integer(tr$right[j])
        # Extract the attribute of the next node
        attribute = tr$attribute[j]
        
        if (attribute == "-"){
          # When the attribute is empty is a leaf node
          # Look for the majority and assign it as the prediction
          y_new = tr$majority[j]
          y[i] = as.integer(y_new)
        }else{
          # Extract the tree value of the next node
          tr_value = as.integer(tr$value[j])
          # Extract the case value for the next attribute
          case_value = case[attribute][[1]]
        }
      }
    }
  }
  return(y)
}


# x: attribute values (data matrix)
# y: class labels (vector)
# minleaf: minimum number of observations required for a leaf node (integer)
# selected_feature: best column to split the node (integer)
# selected_interval: best value to split the node (integer)
# selected_quality: quality used to select the best feature (integer)
# evaluate_feature calculates which column (feature) of the input data is the best option to split the node
#                  and also calculates the best interval to split the selected feature. Also returns a third
#                  value, that is the selected quality, used to check if there is a valid split or not
evaluate_feature = function(x, y, minleaf){
  
  # List that stores all the quality values for the different features of the data matrix
  total_quality_intervals = list()
  # Vector that stores the maximum quality value for each feature of the data matrix
  qualities = c()
  # Calculate the impurity of the parent node
  parent_impurity = gini_index(y)
  
  # Loop to iterate all the columns in the data matrix
  for (j in 1:length(x)){
    # Get the uniques values in the data
    df = sort(unique(x[,j]))
    # Check that df is not empty
    if (length(df) > 1){
      # Get the different possible splits
      feature = (df[1:(length(df)-1)] + df[2:length(df)]) / 2
      total_quality = c()
      
      # For loop to calculate the quality of each interval
      for (i in 1:length(feature)){
        
        # Interval selection
        children = split_node(x[,j], y, feature[i], minleaf)
        
        # Check that the list children is not empty
        if(length(children) > 1){
          # right_child = children[[1]]
          # left_child = children[[2]]
          # Quality of the split
          split_quality = gini_index(children[[1]]) * length(children[[1]]) / length(x[,j]) + gini_index(children[[2]]) * length(children[[2]]) / length(x[,j])
          total_quality[i] = parent_impurity - split_quality
          
        }else{total_quality[i] = 0} # If it is empty the quality is 0
      }
      
      total_quality_intervals[[j]] = total_quality
      qualities[j] = max(total_quality)
      
    }else{ # If df is empty the quality for that split is 0
      total_quality_intervals[[j]] = 0
      qualities[j] = 0
    }
  }
  
  # Select the maximum quality and then select the feature that correspond to that quality value
  selected_quality = max(qualities)
  selected_feature = match(selected_quality, qualities)
  
  # Select the best interval for the selected quality
  index_of_interval = match(selected_quality, total_quality_intervals[[selected_feature]])
  df_new = sort(unique(x[,selected_feature]))
  feature_new = (df_new[1:(length(df_new)-1)] + df_new[2:length(df_new)]) / 2
  best_interval = feature_new[index_of_interval]
  
  return(list(selected_feature, best_interval, selected_quality))
}


# node: class labels (vector)
# gini_impurity: impurity of the input node (integer)
# gini_index computes the gini index of a given node
gini_index <- function(node){
  
  # p1 = number of 1 in the node / total number of observations in that node
  p1 = sum(node) / length(node)
  
  # p0 = (1 - p1)
  # Gini-index formula: p0 * (1 - p0)
  gini_impurity = (1 - p1) * p1
  
  return(gini_impurity)
}


# x: attribute values (data matrix)
# y: class labels (vector)
# feature: selected feature for the split (integer)
# best_split: selected split (integer)
# children: right and left children result of splitting the node (list)
# children_nodes splits a node using a feature (column) and split
#                returns as a list the calculated children
children_nodes <- function(x, y, feature, best_split){
  
  x = cbind(x, y)
  
  # Create the children using the best split
  left_child = x[x[,feature] >= best_split, ]
  right_child = x[x[,feature] < best_split, ]
  
  return(list(right_child, left_child))
}


# x: attribute values (vector)
# y: class labels (vector)
# selected_split: split used to divide the node (integer)
# minleaf: minimum number of observations required for a leaf node (integer)
# children: right and left children result of splitting the node [only observations] (list)
# split_node splits a vector of attributes using a selected split before returning the children [only observations (y)]
#            it checks if the children meet the minleaf condition
split_node <- function(x, y, selected_split, minleaf){
  
  # Vector of observations for the children nodes
  right_child = y[x >= selected_split] 
  left_child = y[x < selected_split] 
  
  # Check that the children nodes meet the minleaf condition
  if(length(right_child) < minleaf | length(left_child) < minleaf){
    return(0)
  }else{
    return(list(right_child, left_child))
  }
}


# y: class labels (vector)
# majority: indicates the majority class in the labels (binary)
# majority calculates the predominating class in a label vector
majority <- function(y){
  
  # Count the number of 1 in the observation of a node
  # Check if that number is greater of half of the length of the observations
  if  (sum(y) >= length(y)/2){
    majority = 1
  } else{
    majority = 0
  }
  return(majority)
}

