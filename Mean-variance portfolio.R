
data=read.csv("Average_value_weighted_inklsv_US.csv",sep=";",header=TRUE)

# Function for calculating the means of every column/asset in the dataset:
get_means_vector=function(data){
  means_vector= NULL
  for (i in 1:length(data)){
    #Filling in the mean of each asset in every iteration:
    means_vector=c(means_vector,mean(data[[i]]))
  }
  return(means_vector)
}

# Function for calculating the weights of the assets in the dataset:
get_weights_vector= function(means_vector,cov_matrix){
  # Create the inverse of the passed covariance matrix:
  inverse_cov=solve(cov_matrix)
  # Implement the formula for Xt (DeMiguel, Garlappi, Uppal; page 1922):
  x_t=inverse_cov%*%means_vector
  x_t_vector=c(x_t)
  return (x_t_vector)
}

# Function for calculating the relative weights of the assets in the dataset:
get_rel_weights_vector=function(weights_vector){
  # Calculate the absolute sum of the weights vector:
  abs_sum=abs(sum(weights_vector))
  # Divide each of the values in the weights vector by the absolute sum:
  rel_weights=weights_vector/abs_sum
  return (rel_weights)
}

#1.) Sharpe ratio out-of-sample:
  #1.1.) Drop the column with the months:
  data=data[,-1]
  #1.2.) Set the rolling window:
  rolling_window=120
  #1.3.) Calculate length of the new vector with the portfolio returns:
  len_portfolio_returns=length(data[,1])-rolling_window
  #1.4.) Create the vector with the respective length:
  portfolio_returns=c(length=len_portfolio_returns)

#1.5.) Calculate the (in this case 497 - 120) excess returns 
# and add each value in the portfolio_returns vector:
for(i in 1:len_portfolio_returns){
  # Set the start index for each iteration:
  start_window=i
  # Set the start index for each iteration:
  end_window=rolling_window+i-1
  # Create a new "time"-matrix with the start index and the end index (rowise):
  time_matrix=data[start_window:end_window,]
  # Create the covariance matrix of the "time"-matrix:
  cov_time_matrix=cov(time_matrix)
  # Calculate the weights of the assets in row end_window + 1/
  # the weights for the new portfolio based on the last 120 rows:
  weights_vct=get_weights_vector(get_means_vector(time_matrix),cov_time_matrix)
  # Calculate the relative weights:
  rel_weights_vct=get_rel_weights_vector(weights_vct)
  # Calculate the portfolio return using the excess returns in row
  # end_window + 1 of the initial data and the computed weights:
  single_pf_return=unlist(data[end_window+1,])%*%rel_weights_vct
  # Add each value in the vector portfolio returns:
  portfolio_returns[start_window]=single_pf_return
}

  # Calculate the sharpe ratio out of sample
  sharpe_ratio_out_of_sample=mean(portfolio_returns)/sd(portfolio_returns)
  sharpe_ratio_out_of_sample

#2.) Sharpe ratio in-sample:
  #2.1.) Alternative 1:
  # The means of the columns/ assets:
  means_vct_in_sample=get_means_vector(data)
  # The weights for each asset:
  weights=get_weights_vector(means_vct_in_sample,cov(data))
  # The relative weights for each asset:
  rel_wg=get_rel_weights_vector(weights)
  # The portfolio return:
  pf_return_in_sample=rel_wg%*%means_vct_in_sample
  # The variance of the portfolio: 
  var_pf=c(rel_wg%*%cov(data))%*%rel_wg
  # The sd of the portfolio:
  sd=sqrt(var_pf)
  # The sharpe ratio in-sample:
  sharpe_ratio_in_sample=pf_return_in_sample/sd
  sharpe_ratio_in_sample

  #2.2.) Alternative 2:
  # Create a new vector to be filled in a loop rowise:
  pf_rtr=c(length=length(data[,1]))
  for(i in 1:length(data[,1])){
    pf_rtr[i]=unlist(data[i,])%*%rel_wg 
  }
  mean(pf_rtr)/sd(pf_rtr)
  # Leads to the same result

