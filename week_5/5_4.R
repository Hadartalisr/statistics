#q.1_1
k = 10^4
get_random_v <- function(n){
  v = c()
  for(i in seq(1,n)){
    v = c(v,sample(0:5, 1))
  }
  return(v) 
}

my_var <- function(v){
  return (mean((v)^2) - (mean(v))^2)
}
my_bias <- function(v, t){
  return((t - mean(v))^2)
}

t1_v = c()
t2_v = c()

for(i in seq(1,k)){
  v = get_random_v(10)
  t1_v = c(t1_v, 2*mean(v))
  t2_v = c(t2_v, max(v))
}

var_t1 = my_var(t1_v)
var_t1
var_t2 = my_var(t2_v)
var_t2
bias_t1 = my_bias(t1_v,5)
bias_t1
bias_t2 = my_bias(t2_v,5)
bias_t2

#q.1_3
par(mfrow=c(2,1))
plot(density(t1_v), main = "??1", xlab = "statistic")
abline(v = 5, col="red") 
plot(density(t2_v), main = "??2", xlab = "statistic")
abline(v = 5, col="red") 

#q.1_4
# we chose t2 as out statistic
v2 = t2_v[sample.int(10000, 20)]
v2
v2_var = my_var(v2)
v2_var
v2_bias = my_bias(v2,5)
v2_bias
# we chose randomly 20 sample out of the results of t2 statistic.
# then we calculated the variance and the bias - 
# those are not the empirical bias and var because 
# now we estimated the performance of the statistic, 
# not the population.


#q.2

lambda = 1

get_random_v <- function(n){
  v = c(v,rexp(n,lambda))
  return(v) 
}

t1_v = c()
t2_v = c()

for(i in seq(1,k)){
  v = get_random_v(10)
  t1_v = c(t1_v, 1/(mean(v)))
  t2_v = c(t2_v, (log(2)/median(v)))
}

var_t1 = my_var(t1_v)
var_t1
var_t2 = my_var(t2_v)
var_t2
bias_t1 = my_bias(t1_v,5)
bias_t1
bias_t2 = my_bias(t2_v,5)
bias_t2

#q.1_3
par(mfrow=c(2,1))
plot(density(t1_v), main = "??1", xlab = "statistic", xlim=c(0.95,1.05))
abline(v = 1, col="red") 
plot(density(t2_v), main = "??2", xlab = "statistic", xlim=c(0.95,1.05))
abline(v = 1, col="red") 


#q.2_4
v2 = t2_v[sample.int(10000, 20)]
v2
v2_var = my_var(v2)
v2_var
v2_bias = my_bias(v2,5)
v2_bias
