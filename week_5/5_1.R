#q.1
#q.1.4
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
abline(h = 2.5, type="l", col="red") 
lines(x, mse_3(x), type="l", col="orange") 
legend("topleft", c("mse_1","mse_2","mse_3"),fill=c("blue","red","orange"))



