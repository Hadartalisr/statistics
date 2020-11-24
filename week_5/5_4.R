#q.1
k = 10^4
get_random_v <- function(){
  v = c()
  for(i in seq(1,10)){
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
  v = get_random_v()
  t1_v = c(t1_v, 2*median(v))
  t2_v = c(t2_v, max(v))
}

var_t1 = my_var(t1_v)
var_t2 = my_var(t2_v)

