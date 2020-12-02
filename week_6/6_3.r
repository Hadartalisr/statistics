my_var <- function(v){
  return (mean((v)^2) - (mean(v))^2)
}

#q.1_1
k = 10^4

lambda_v = seq(0.1,10, 1)

mse_1_v = c()

mse_2_v = c()

for (lambda in lambda_v){

  get_random_x <- function(lambda){
    x = rexp(20, lambda)
  }
  
  get_random_y <- function(lambda){
    x = rpois(20, lambda)
  }
  
  lambda_tag_1_v = c()
  
  lambda_tag_2_v = c()
  
  for (i in seq(1,k)){
    
    x = get_random_x(lambda)
    
    lambda_tag_1 = (1/mean(x))
    
    lambda_tag_1_v = c(lambda_tag_1_v, lambda_tag_1)
    
    y = get_random_y(lambda)
    
    lambda_tag_2 = mean(y)
    
    lambda_tag_2_v = c(lambda_tag_2_v, lambda_tag_2)
    
  }
  
  bias_1 = lambda - mean(lambda_tag_1_v)
  
  bias_2 = lambda - mean(lambda_tag_2_v)
  
  var_1 = my_var(lambda_tag_1_v)
  
  var_2 = my_var(lambda_tag_2_v)
  
  mse_1 = var_1 + bias_1^2
  
  mse_2 = var_2 + bias_2^2
  
  mse_1_v = c(mse_1_v, mse_1)
  
  mse_2_v = c(mse_2_v, mse_2)

}


plot(x = lambda_v ,y= mse_1_v , xlab = "lambda", ylab = "mse", 
     xlim = c(0,10), col="blue")

points(x = lambda_v ,y= mse_2_v , xlab = "lambda", 
     ylab = "mse", xlim = c(0,10), col="red")




