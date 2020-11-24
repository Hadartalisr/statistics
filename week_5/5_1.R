#q.1
#q.1_4
mse_1 <- function(u) {
  ret = (60-u)^2
  return(ret)
}

mse_2 <- function(u){
  return(2.5)
}

mse_3 <- function(u){
  return ( (10 - (u/6))^2 + (1.763)) 
}
x = seq(50,70,0.1)
plot(x, mse_1(x), type="l", col="blue", 
     main="MSE as a function of ??", xlab = "??",
     ylim = c(0,10), ylab = "MSE" ) 
abline(h = 2.5, col="red") 
lines(x, mse_3(x), col="orange") 
legend("topleft", c("mse_1","mse_2","mse_3"),fill=c("blue","red","orange"))
# From the following comparison we can determine that for 
# each statistic there is an interval of ??'s s.t the function minimize the 
# corresponding MSE the most.


#q.1_5
v = c(81,95,85,75,98,100,85,86,92,91)
u = mean(v)
v_1 = mse_1(u)
v_1
v_2 = mse_2(u)
v_2
v_3 = mse_3(u)
v_3

