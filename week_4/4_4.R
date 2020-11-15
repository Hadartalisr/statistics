#q.4

my_var <- function(v){
  return (mean((v)^2) - (mean(v))^2)
}

my_sd <- function(v){
  var = my_var(v)
  return (sqrt(var))
}


result = seq(1,6)
prob =  c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
df = data.frame(x = result , y = prob)
colnames(df)[1] <- "number"
colnames(df)[2] <- "probability"
df
mean_x = mean(df$number)
mean_x
var_x = my_var(df$number)
var_x
sd_x = my_sd(df$number)
sd_x 
get_random_x2 <- function(){
  y1 = sample(1:6, 1)
  y2 = sample(1:6, 1)
  x2 = y1+y2
  return(x2) 
}
x2 = get_random_x2()
# mean of x2 (2 cube results) equals 2 mean of x (Expectation is linear)
# therefore, mean(x2) = mean(x+x) = 2 mean(x) = 2*3.5 = 7
# the cubes are i.i.d therefore - 
# var(y) = var(x1+x2) = var(x1) + var(x2) = 2*var(x)
sd_x2 = sqrt(2*var_x)
sd_x2
mean_x2 = mean(x2)
mean_x2


#q.2
x2_vec = c()
for(i in seq(1,10001)){
  x2_vec = c(x2_vec, get_random_x2())
}
plot(density(x2_vec), main = "x2", xlab = "result")
mean_x2_vec = mean(x2_vec)
mean_x2_vec
sd_x2_vec = my_sd(x2_vec)
sd_x2_vec
#calculate the diff :
mean_x2_vec - mean_x2
sd_x2_vec - sd_x2
# thus, the results were close to the expected theory values.


#q.3






