#q.4

par(mfrow=c(1,1))

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
  x2 = (y1+y2)/2
  return(x2) 
}
x2 = get_random_x2()
# mean of x2 (2 cube results) equals 2 mean of x (Expectation is linear)
# therefore, mean(x2) = mean(x+x)/2 = 2 mean(x)/2 = 3.5
mean_x2 = 3.5
# the cubes are i.i.d therefore - 
# var(y) = var((x1+x2)/2) = 0.25(var(x1) + var(x2)) = 0.5*var(x)
var_x2 = 0.5*var_x
var_x2

sd_x2 = sqrt(var_x2)
sd_x2


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
mean_x2_vec - 3.5
sd_x2_vec - sd_x2
# thus, the results were close to the expected theory values.


#q.3
x <- seq(-2, 8,length=1000)
y <- dnorm(x, mean=mean_x2, sd=sd_x2)
lines(x, y, type="l", lwd=1)
# yes, we can say that x2 distribute approximately noraml


#q.4
 

n_vec = c()
mean_vec = c()
sd_vec = c()

par(mfrow=c(2,2))
for(n in seq(100,1000,300)){
  plot(x,y, main = n, xlab = "result",type="l",lwd=1)
  
  n_vec = c(n_vec,n)
  x3_vec = c()
  for(i in seq(1,n)){
    x3_vec = c(x3_vec, get_random_x2())
  }
  lines(density(x3_vec), xlab = "", ylab = "")

  mean_x3_vec = mean(x3_vec)
  mean_vec = c(mean_vec, mean_x3_vec)
  
  sd_x3_vec = my_sd(x3_vec)
  sd_vec = c(sd_vec, sd_x3_vec)
}






