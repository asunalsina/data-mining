# Function to calculate the majority class of a node
majority <- function(class){
  # We count the number of 1 and if it greater than half of the length of the class
  # then we assign 1 as the majority class
  if  (sum(class) >= length(class)/2){
    majority = 1
  } else{
    majority = 0
  }
  return(majority)
}