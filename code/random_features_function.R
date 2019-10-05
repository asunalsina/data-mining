random_features = function(x, nfeat){
  features = sample(1:length(x), nfeat, replace=FALSE)
  features = sort(features)
  x = x[,features]
  return(x)
}