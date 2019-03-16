# the function inSamplesharpe expects a data frame, where the first column 
# contains an index (e.g. date) and all other rows contain returns.
# it returns the in-sample Sharpe ratio of said data frame.
inSampleSharpe = function(return_matrix) {
  
  # calculate weights
  weights = calcMinVarWeights(return_matrix)
  
  # calculate mean of each asset
  mean_returns = data.matrix(apply(return_matrix[,-1], 2, mean)) 
  
  # calculate sharpe-ratio according to formula
  cov_matrix = cov(return_matrix[,-1])
  sharpe_ratio_in_sample = 
    t(mean_returns) %*% weights/ sqrt(t(weights)%*%cov_matrix%*%weights)
  
  return (sharpe_ratio_in_sample)
}

# function calcMinVarWeights expects a matrix of returns, where the first column
# contains an index (e.g. date) and all other columns contain returns.
# the function returns a normalized weights vector
calcMinVarWeights = function(return_matrix) {
  return_matrix = data.matrix(return_matrix) # convert for applying linear algebra
  
  # solve system of linear equations
  nA = dim(return_matrix[,-1])[2] # number of assets
  b = vector(length=nA) + 1 # constraint-vector
  cov_matrix = cov(return_matrix[,-1]) # dates are deleted
  weights = solve(cov_matrix, b)
  weights = data.matrix(weights/sum(weights)) # normalize the vector
  
  return (weights)
}
