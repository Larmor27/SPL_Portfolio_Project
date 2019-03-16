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